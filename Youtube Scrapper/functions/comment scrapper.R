# ======================== FUNCTION DESCRIPTION ===========================
# This function retrieves the top comments from a YouTube video. It serves as a 
# wrapper for an httr function that fetches comments and returns a dataframe 
# instead of the JSON response from the HTTP request.
#
# It considers the following limitation: The API only returns the first 100 
# comments for any given video. Additionally, if the video has restrictions, 
# the API request will return an error. However, for this specific function, 
# videos are pre-verified using the function get_top_videos.
#
# Creation Date: 28/11/2024
#
# Author: Michel Mendivenson Barragán Zabala
#         Universidad Nacional de Colombia, Statistics Department
#         Statistical Social Network Analysis

# ============================= DEPENDENCIES ===================================
library(httr)      # For HTTP requests
library(jsonlite)  # To handle JSON responses
library(dplyr)     # For data manipulation

# ======================= FETCH TOP COMMENTS PER VIDEO =========================

get_top_comments = function(key, videoID, maxComments = 100, order = 'relevance',
                            verbose = F,encoding = 'UTF8') {
  
  comment_data = as.data.frame(c())      # Data frame to store video information
  
  response = GET(url = 'https://www.googleapis.com/youtube/v3/commentThreads',
                 query = list(part = 'id, snippet', key = key, videoId = videoID,
                              maxResults = maxComments, order = order))
  url = response$url
  
  if (verbose) {
    cat('\n', url, '\n')
  }
  
  response = fromJSON(content(response, type = 'text', encoding = encoding),
                      flatten = T)
  
  if (is.null(response$error)) {
    comment_data = rbind(
      comment_data,
      response$items |>
        select(
          'videoID' = snippet.videoId, 'commentID' = id,
          'author' = snippet.topLevelComment.snippet.authorDisplayName,
          'authorID' = snippet.topLevelComment.snippet.authorChannelId.value,
          'comment' = snippet.topLevelComment.snippet.textOriginal,
          'repliesCount' = snippet.totalReplyCount, 'likesCount' = snippet.topLevelComment.snippet.likeCount,
          'published' = snippet.topLevelComment.snippet.publishedAt
        )
    )
    comment_data$order = 1:nrow(comment_data)
    if (verbose){cat(
      '\n\n\n\n \t\t\t\t\t\t ================================================= \n\n',
      '\t\t\t\t\t\t Successfully fetched information of',
      nrow(comment_data),
      'comments for the', videoID, 'video',
      '\n\n \t\t\t\t\t\t ================================================= \n\n\n\n'
    )}
    return(comment_data)
  } else {
    cat(
      '\n\n \t\t\t =======>  ERROR: The video',
      videoID,
      'does not have available comments!!!!\n\n'
    )
    cat('\n The URL request was:', url, '\n')
    return(response)
  }
}
