# ======================== FUNCTION DESCRIPTION ===========================
# This function fetches the top videos from YouTube based on specific parameters.
# Key features:
#   - Continues fetching until there are no more results available via the YouTube API or 
#     until it retrieves the top `k` videos of assignable categories or in general 
#     (if no categoryID is specified).
#   - Includes videos without specific categories when categoryID = NULL.
#   - Allows control over regionCode and chart for finer API requests.
#   - Filters videos by duration when `shorts` is FALSE (short videos are < 90 seconds).
#
# Recent Updates (26/Nov/2024):
#   - Enhanced filtering: excludes short-format videos (< 60 seconds) and those without comments.
#   - Functions separated from the main script for improved clarity.
#   - Added a `verbose` argument for debugging: prints API request URLs instead of showing progress bars.
#
# Author: Michel Mendivenson Barragán Zabala
#         Universidad Nacional de Colombia, Statistics Department
#         Social Network Analysis
# ========================================================================

# ============================= DEPENDENCIES ==============================
library(httr)      # For HTTP requests
library(jsonlite)  # To handle JSON responses
library(dplyr)     # For data manipulation

# ========================== FETCH TOP VIDEOS =============================
get_top_videos = function(key,                       # YouTube API key
                          howMany = 20,             # Number of videos to fetch
                          shorts = F,               # Filter short videos? (TRUE = include, FALSE = exclude)
                          categoryID = NULL,        # Category ID (NULL fetches all categories)
                          chart = 'mostPopular',    # Sorting criteria ('mostPopular' or 'chartUnspecified')
                          regionCode = 'US',        # Restrict search to specific region (ISO 3166-1 alpha-2)
                          verbose = F,              # Debug: Show API URLs instead of a progress bar?
                          encoding = 'UTF-8'){      
  # Initialize variables
  response = list()                      # Stores the query result
  response$nextPageToken = 'Placeholder' # Used for pagination; NULL when no more results are available
  video_data = as.data.frame(c())        # Data frame to store video information
  
  if (!verbose) {pb = txtProgressBar(min = 0, max = howMany, style = 3)}  # Progress bar initialization
  
  flag = T  # Flag to handle the first iteration (nextPageToken starts as NULL)
  
  # Loop: Fetch videos until the desired count or no more results
  while (nrow(video_data) < howMany & !is.null(response$nextPageToken)) {
    if (flag) {response$nextPageToken = NULL; flag = F}  # Reset nextPageToken for the first call
    
    # API request
    response = GET(url = "https://www.googleapis.com/youtube/v3/videos",
                   query = list(part = 'snippet, contentDetails, statistics',
                                type = 'video', 
                                videoCategoryId = categoryID,
                                key = key, 
                                regionCode = regionCode,
                                chart = chart, 
                                maxResults = 50,
                                pageToken = response$nextPageToken))
    url = response$url
    if (verbose) {cat('\n', url, '\n')}  # Debug: Print API URL
    
    # Parse response
    response = fromJSON(content(response, type = 'text', encoding = encoding), flatten = T)
    
    if (is.null(response$error)){
      
      # Append video details to the data frame
      video_data = rbind(video_data,
                         response$items %>% select(
                           'ID' = id,                                                # Video ID
                           'published' = snippet.publishedAt,                        # Publication date
                           'channel ID' = snippet.channelId,                         # Channel ID
                           'channel name' = snippet.channelTitle,                    # Channel name
                           'title' = snippet.title,                                  # Video title
                           'description' = snippet.description,                      # Video description
                           'category ID' = snippet.categoryId,                       # Category ID
                           'Language' = snippet.defaultAudioLanguage,                # Default audio language
                           'Duration' = contentDetails.duration,                     # Video duration
                           'Views' = statistics.viewCount,                           # View count
                           'likeCount' = statistics.likeCount,                       # Like count
                           'commentCount' = statistics.commentCount) %>%             # Comment count
                           # Filter short videos if `shorts` is FALSE
                           {if (!shorts) filter(., lubridate::duration(Duration) > lubridate::dseconds(90)) else .})
      
      # Update progress bar
      if (!verbose) {setTxtProgressBar(pb, value = nrow(video_data))}
    } else {
      cat('\n\n \t\t\t =======>  ERROR: The category', categoryID, 'does not have available information!!!!!\n\n')
      response$nextPageToken = NULL
      cat('\n The URL request was:', url, '\n')
    }
  }
  
  if (is.null(response$error)){
    
    if (!verbose) {setTxtProgressBar(pb, value = howMany)}  # Finalize progress bar
    if (nrow(video_data) > howMany) {video_data = video_data[1:howMany,]}  # Truncate excess videos
    
    # Completion message
    cat('\n\n\n\n \t\t\t\t\t\t ================================================= \n\n',
        '\t\t\t\t\t\t Successfully fetched information for', nrow(video_data), 'videos',
        '\n\n \t\t\t\t\t\t ================================================= \n\n\n\n')
  }
  
  
  if (!verbose) {close(pb)}  # Close progress bar
  if (is.null(response$error)){
    return(video_data)
  } else {
    return(response)
  }
}