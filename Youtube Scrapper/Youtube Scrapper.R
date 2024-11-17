# The data was retrieved on 17/Nov/2024 by running this code specifically. 
# Visit https://developers.google.com/youtube/v3 for the YouTube API documentation.

# Author: Michel Mendivenson Barragán Zabala
#         Universidad Nacional de Colombia, Statistics Department.
#         Social Network Analysis

dir = '~/UN/1) Análisis de redes sociales (ARS)/2) Workplace'
setwd(dir)

library(httr)
library(jsonlite)
library(dplyr)


# ============== FETCHING THE MOST POPULAR VIDEOS OF EACH CATEGORY ==================

# =====> Load the API key
key = readLines('API_KEY')[2]                       # There is more than one key in the file

# =====> Fetch video categories
categories <- GET(url = "https://www.googleapis.com/youtube/v3/videoCategories",
                  query = list(part = "snippet", regionCode = "US", key = key))
categories = fromJSON(content(categories, as = "text"), flatten = TRUE)
categories = as.data.frame(cbind('ID' = categories$items$id,
                                 'Category' = categories$items$snippet.title,
                                 'Assignable' = categories$items$snippet.assignable))


# "Assignable" (TRUE) categories are "real" categories that can be assigned or viewed through the video's metadata.

# =====> A function to obtain the 50 most popular videos under a category
get_top_videos_by_category = function(api_key, video_category_id, 
                                      page_token = NULL, m = 50) {
  query_params = list(part = 'snippet, contentDetails, statistics',
                      type = 'video',
                      videoCategoryId = video_category_id,
                      key = key,
                      maxResults = m,                        # YouTube API allows a maximum of 50 results per request
                      regionCode = 'US',
                      chart = 'mostPopular')
  response = GET(url = "https://www.googleapis.com/youtube/v3/videos", 
                 query = query_params)
  fromJSON(content(response, as = "text"), flatten = TRUE)
}

videos = list()                                   # All the objects will be stored in a list
for (i in 1:nrow(categories)) {
  if (categories$Assignable[[i]]) {
    cat('Processing information for the category: ',
        categories$Category[[i]], '.\n')
    
    # Actually scraping the data
    video_data = get_top_videos_by_category(api_key = key, m = 50,
                                            video_category_id = categories$ID[[i]])$items
    
    # Some data is not accessible through the API, so a NULL check is necessary
    if (!is.null(video_data) && length(video_data) > 0) {
      
      videos[[categories$Category[[i]]]] = video_data %>% select(
        'ID' = id,                                               # Video's ID
        'published' = snippet.publishedAt,                        # Publication Date
        'channel ID' = snippet.channelId,                         # Channel's ID
        'channel name' = snippet.channelTitle,                    # Channel's name
        'title' = snippet.title,                                  # Video's title
        'description' = snippet.description,                      # Video's description
        'category ID' = snippet.categoryId,                       # Category's ID
        'Language' = snippet.defaultAudioLanguage,                # Default language
        'Duration' = contentDetails.duration,                     # Video's duration
        'Views' = statistics.viewCount,                           # View count
        'likeCount' = statistics.likeCount,                       # Like count
        'commentCount' = statistics.commentCount                  # Comment count
      )
    } else {
      cat('====> No data was found for the category:', categories$Category[[i]], '\n')
    }
  }
}

# ----------------------------------------------------------------------------------------------
# | No data was found for the categories "Travel & Events" and "Education"                      |
# | because the API did not return any results. Additionally, "Nonprofits & Activism" has only |
# | one entry, and "Music" only returns 30 results instead of the 50 possible.                  |
# ----------------------------------------------------------------------------------------------

save(categories, videos, file = 'Youtube Scrapper/videos.RData')