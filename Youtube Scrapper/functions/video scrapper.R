# ======================== FUNCTION DESCRIPTION ===========================
# This function fetches the top videos from YouTube based on specific parameters.
# Key features:
#   - Continues fetching until there are no more results available via the YouTube API or
#     until it retrieves the top `k` videos of assignable categories or in general
#     (if no categoryID is specified).
#   - Includes videos without specific categories when categoryID = NULL.
#   - Allows control over regionCode and chart for finer API requests.
#   - Filters videos by duration when `shorts` is FALSE (short videos are < 90 seconds).
#   - Filter videos by comment count (Videos withc no comments are of no use)

# Recent Updates (26/Nov/2024):
#   - Enhanced filtering: excludes short-format videos (< 60 seconds) and those without comments.
#   - Functions separated from the main script for improved clarity.
#   - Added a `verbose` argument for debugging: prints API request URLs instead of showing progress bars.

# Last Update (27/Nov/2024):
#   - There are some videos that, even though they have comments enabled on the platform, 
#     return a 403 error when attempting to retrieve the comments. The error indicates 
#     that the comments for the video are disabled. The most likely explanation is that 
#     the video contains explicit content or lyrics. However, since the YouTube API does 
#     not currently provide a way to filter videos with explicit content, as a temporary fix, 
#     the function will attempt a basic comment fetch for each retrieved video. If an error 
#     code is encountered, the video will be removed from the `video_data` data frame.
#
#     Example of such an error: 
#     See for example: https://www.googleapis.com/youtube/v3/commentThreads?part=snippet&videoId=U0KTVVMvcc4&key=<YOUR_API_KEY>&maxResults=1&order=relevance

# Author: Michel Mendivenson Barragán Zabala
#         Universidad Nacional de Colombia, Statistics Department
#         Social Network Analysis
# ========================================================================

# ============================= DEPENDENCIES ==============================
library(httr)      # For HTTP requests
library(jsonlite)  # To handle JSON responses
library(dplyr)     # For data manipulation

# ========================== FETCH TOP VIDEOS =============================
get_top_videos = function(key,                           # YouTube API key
                          howMany = 20,                  # Number of videos to fetch
                          shorts = F,                    # Filter short videos? (TRUE = include, FALSE = exclude)
                          categoryID = NULL,             # Category ID (NULL fetches all categories)
                          chart = 'mostPopular',         # Sorting criteria ('mostPopular' or 'chartUnspecified')
                          regionCode = 'US',             # Restrict search to specific region (ISO 3166-1 alpha-2)
                          verbose = F,                   # Debug: Show API URLs instead of a progress bar?
                          encoding = 'UTF-8') {
  # Initialize variables
  response = list()                      # Stores the query result
  response$nextPageToken = 'Placeholder' # Used for pagination; NULL when no more results are available
  video_data = as.data.frame(c())        # Data frame to store video information
  
  if (!verbose) {
    pb = txtProgressBar(min = 0,
                        max = howMany,
                        style = 3)
  }  # Progress bar initialization
  
  flag = T  # Flag to handle the first iteration (nextPageToken starts as NULL)
  
  # Loop: Fetch videos until the desired count or no more results
  while (nrow(video_data) < howMany &
         !is.null(response$nextPageToken)) {
    if (flag) {
      response$nextPageToken = NULL
      flag = F
    }  # Reset nextPageToken for the first call
    
    # API request
    response = GET(
      url = "https://www.googleapis.com/youtube/v3/videos",
      query = list(part = 'snippet, contentDetails, statistics, status', type = 'video',
                   videoCategoryId = categoryID, key = key, regionCode = regionCode, 
                   chart = chart, maxResults = 50, pageToken = response$nextPageToken))
    url = response$url
    if (verbose) {cat('\n', url, '\n')}  # Debug: Print API URL
    
    # Parse response
    response = fromJSON(content(response, type = 'text', encoding = encoding),
                        flatten = T)
    
    if (is.null(response$error)) {
      video_data = rbind(
        video_data,
        response$items %>% select('ID' = id, 'published' = snippet.publishedAt, 'channelID' = snippet.channelId,
                                  'channelname' = snippet.channelTitle,'title' = snippet.title, 'description' = snippet.description,
                                  'categoryID' = snippet.categoryId,'language' = snippet.defaultAudioLanguage,
                                  'duration' = contentDetails.duration,'viewsCount' = statistics.viewCount,
                                  'likeCount' = statistics.likeCount,'commentCount' = statistics.commentCount
        ) %>%             
          # Filter short videos if `shorts` is FALSE
          {if (!shorts)
            filter(., lubridate::duration(duration) > lubridate::dseconds(90))
           else
              .} |>
          filter(commentCount > 0) %>%
          {if (nrow(.) > 0) {
            filter(., unlist(lapply(.$ID,
                                    FUN = function(x)
                                      is.null(fromJSON(content(GET(
                                        url =  "https://www.googleapis.com/youtube/v3/commentThreads",
                                        query = list(
                                          part = 'snippet',
                                          videoId = x,
                                          key = key,
                                          maxResults = 1
                                        )
                                      ),
                                      type = 'text',
                                      encoding = 'UTF8'
                                      ),
                                      flatten = T
                                      )$error))))}}
      )
      
      # Update progress bar
      if (!verbose) {
        setTxtProgressBar(pb, value = nrow(video_data))
      }
    } else {
      cat(
        '\n\n \t\t\t =======>  ERROR: The category',
        categoryID,
        'does not have available information!!!!!\n\n'
      )
      response$nextPageToken = NULL
      cat('\n The URL request was:', url, '\n')
    }
  }
  
  if (is.null(response$error)) {
    if (!verbose) {
      setTxtProgressBar(pb, value = howMany)
    }  # Finalize progress bar
    if (nrow(video_data) > howMany) {
      video_data = video_data[1:howMany, ]
    }  # Truncate excess videos
    
    # Completion message
    cat(
      '\n\n\n\n \t\t\t\t\t\t ================================================= \n\n',
      '\t\t\t\t\t\t Successfully fetched information for',
      nrow(video_data),
      'videos',
      '\n\n \t\t\t\t\t\t ================================================= \n\n\n\n'
    )
  }
  
  
  if (!verbose) {
    close(pb)
  }  # Close progress bar
  if (is.null(response$error) & nrow(video_data) > 0) {
    return(video_data)
  } else {
    return(response)
  }
}