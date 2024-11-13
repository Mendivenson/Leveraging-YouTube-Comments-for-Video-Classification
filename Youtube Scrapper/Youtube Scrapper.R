# The data was retrieved on 13/Nov/2024 by running this code specifically. 
# Visit https://developers.google.com/youtube/v3 for the YouTube API documentation.

# Author: Michel Mendivenson Barragán Zabala
#         Universidad Nacional de Colombia, Statistics Department.
#         Social Network Analysis

dir = '~/UN/1) Análisis de redes sociales (ARS)/2) Workplace'
setwd(dir)

library(httr2)
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
categories$Assignable = categories$Assignable == 'TRUE'

# "Assignable" (TRUE) categories are "real" categories that can be assigned or viewed through the video's metadata.

# =====> A function to obtain the 50 most popular videos under a category
get_top_videos_by_category = function(api_key, video_category_id, 
                                      page_token = NULL, m = 50) {
  query_params = list(part = 'snippet, contentDetails',
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
        'Language' = snippet.defaultAudioLanguage                 # Default language
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


# ================= FETCHING THE 20 MOST LIKED COMMENTS FOR EACH VIDEO =======================
rm(list = ls())
load('Youtube Scrapper/videos.RData')

# =====> A function to fetch the top 20 comments for a given video (sorted by like count)
get_top_comments_by_video = function(api_key, video_id, m = 20) {
  query_params = list(
    part = 'snippet,replies',
    videoId = video_id,
    key = api_key,
    maxResults = m,  # Limit to the top 20 comments
    order = 'relevance'  # Sort by relevance (can be changed to 'time' for latest comments)
  )
  
  response = GET(url = "https://www.googleapis.com/youtube/v3/commentThreads", 
                 query = query_params)
  
  comment_data = fromJSON(content(response, as = "text"), flatten = TRUE)
  return(comment_data)
}

# Create an empty list to store the top comments for each video
top_comments = list()

# Loop through each category and its videos to fetch the top comments
for (category in names(videos)) {
  category_data = videos[[category]]
  
  for (i in 1:nrow(category_data)) {
    video_id = category_data$ID[i]
    cat('Fetching top comments for video:', category_data$title[i], '(\', video_id, \')\n')
    
    # Fetch the top comments for the current video
    comments = get_top_comments_by_video(api_key = key, video_id = video_id, m = 20)
    
    # Check if the comments data is valid and not empty
    if (!is.null(comments$items) && length(comments$items) > 0) {
      # Store the top comments for this video
      top_comments[[video_id]] = comments$items %>% select(
        'Comment ID' = id,
        'Comment' = snippet.topLevelComment.snippet.textDisplay,
        'Author' = snippet.topLevelComment.snippet.authorDisplayName,
        'Like Count' = snippet.topLevelComment.snippet.likeCount,
        'Published At' = snippet.topLevelComment.snippet.publishedAt
      )
    } else {
      cat('No comments found for video:', video_id, '\n')
    }
  }
}

# Save the top comments along with the video data
save(categories, videos, top_comments, file = 'Youtube Scrapper/videos_with_comments.RData')
