# ======================== FUNCTION DESCRIPTION ===========================
# This function fetches the top comments from a video in YouTube. It works more like a 
# wrapper for a function in httr that fetch the comments and returns a dataframe instead
# of the JSON response of the html request.

# It is mean to take into consideration the fact: API just returns the first 100
# comments of any given video except if the video has restrictions in which case 
# the API request is returned with an error but for this specific function the
# videos are first verified in the function get_top_videos.

# Creation date: 28/11/2024

# Author: Michel Mendivenson Barragán Zabala
#         Universidad Nacional de Colombia, Statistics Department.
#         Social Network Analysis

library(httr)
library(jsonlite)
library(dplyr)

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
