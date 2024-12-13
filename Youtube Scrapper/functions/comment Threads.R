# ======================== FUNCTION DESCRIPTION ===========================
# This function retrieves the thread generated for a single comment identified
# by commentID, The results are, again, limited to 100 replies for the API and
# are retrieved in the order of reply (Ordered by time)

# Creation Date: 28/11/2024
#
# Author: Michel Mendivenson Barragán Zabala
#         Universidad Nacional de Colombia, Statistics Department
#         Statistical Social Network Analysis

# ============================= DEPENDENCIES ===================================
library(httr)      # For HTTP requests
library(jsonlite)  # To handle JSON responses
library(dplyr)     # For data manipulation

# =============== FETCH THREAD FOR AN SPECIFIC COMMENT =========================

get_thread = function(key, commentID, maxReplies = 100,
                      verbose = F, encoding = 'UTF8'){
  thread_data = as.data.frame(c())
  response = GET(url = 'https://www.googleapis.com/youtube/v3/comments',
                 query = list(
                   part = 'id, snippet',
                   parentId = commentID,
                   maxResults = 100,
                   key = key,
                   encoding = 'UTF8'))
  url = response$url
  response = fromJSON(content(response, type = 'text', encoding = encoding),flatten = T)
  if (is.null(response$error)){
    if(length(response$items) > 0){
      response = response$items |> select('threadID' = snippet.parentId,
                               'commentID' = id,
                               'author' = snippet.authorDisplayName,
                               'authorID' = snippet.authorChannelId.value,
                               'comment' = snippet.textOriginal,
                               'likesCount' = snippet.likeCount,
                               'published' = snippet.publishedAt)
    } else {
      return(response$items)
      cat('\n\n\n El comentario',commentID, 'no tiene respuestas.')
    } 
    response$order = 1:nrow(response)
  } else {
    cat(
      '\n\n \t\t\t =======>  ERROR: The comment identified as',
      commentID,
      'does not have available replies!!!!\n\n'
    )
    cat('\n The URL request was:', url, '\n')
  }
  return(response)
}
