# The data was retrieved on 17/Nov/2024 by running this code specifically. 
# Visit https://developers.google.com/youtube/v3 for the YouTube API documentation.

# Author: Michel Mendivenson Barragán Zabala
#         Universidad Nacional de Colombia, Statistics Department.
#         Social Network Analysis

dir = '~/UN/1) Análisis de redes sociales (ARS)/2) Workplace'
setwd(dir)

# =====> Load the API key
key = readLines('API_KEY')[2] 

load('Youtube Scrapper/videos.RData')
#str(videos)

library(httr)
library(jsonlite)
library(dplyr)

# ================= FETCHING THE 50 MOST LIKED COMMENTS FOR EACH VIDEO =======================

# =====> A function to fetch the top comments for a given video (sorted by like count)
get_top_comments_by_video = function(api_key, video_id, m = 50) {
  query_params = list(
    part = 'snippet',
    videoId = video_id,
    key = api_key,
    maxResults = m,  # The <YouTube's API only shows 50 results at a time.
    order = 'relevance'  # Sort by relevance (Not equal to most liked)
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


# =============== FETCHING THE FIRST 50 COMMENTS IN EACH VIDEO =================

TopComments = list()

for (i in names(videos)){
  TopComments[[i]] = c()
  for (j in 1:nrow(videos[[i]])){
    video = videos[[i]][j,]
    comments = get_top_comments(api = key, video = video$ID, m = 50)
    comments = comments$items %>%
      select('videoId' = snippet.videoId, 'commentID' = id,
             'replyCount' = snippet.totalReplyCount,
             'likeCount' = snippet.topLevelComment.snippet.likeCount,
             'authorId' = snippet.topLevelComment.snippet.authorChannelId.value,
             'comment' = snippet.topLevelComment.snippet.textOriginal)
    TopComments[[i]] =  rbind(TopComments[[i]],
                              cbind('Top' = seq(1:nrow(comments)), comments))
  }
}
