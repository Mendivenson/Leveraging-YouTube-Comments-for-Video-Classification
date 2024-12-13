# The data was retrieved on 12/Dec/2024 by running this code specifically. 
# Visit https://developers.google.com/youtube/v3 for the YouTube API documentation.

# All the functions used here are available under the functions folder.

# Author: Michel Mendivenson Barragán Zabala
#         Universidad Nacional de Colombia, Statistics Department.
#         Social Network Analysis

dir = '~/UN/1) Análisis de redes sociales (ARS)/2) Workplace/Youtube Scrapper/'
setwd(dir)

library(httr)
library(jsonlite)
library(dplyr)

# ==============================================================================
# ============== FETCHING THE MOST POPULAR VIDEOS OF EACH CATEGORY =============
# ==============================================================================

# =====> Load the API key
key = readLines('keys/key1')[1]                    # There is more than one key in the file

# =====> Load the scrapping function
source(file = 'functions/video scrapper.R')

# =====> Is needed to first fetch video categories
categories <- GET(url = "https://www.googleapis.com/youtube/v3/videoCategories",
                  query = list(part = "snippet", regionCode = "US", key = key))
categories = fromJSON(content(categories, as = "text"), flatten = TRUE)
categories = as.data.frame(cbind('ID' = categories$items$id,
                                 'Category' = categories$items$snippet.title,
                                 'Assignable' = categories$items$snippet.assignable))

# "Assignable" (TRUE) categories are "real" categories that can be assigned or viewed 
# through the video's metadata.

# =====> Fetching the actual videos
videos = list()                                   # All the objects will be stored in a list
for (i in 1:nrow(categories)) {
  if (categories$Assignable[[i]]) {
    cat('Processing information for the category: ',
        categories$Category[[i]], '.\n')
    
    # Actually scraping the data
    video_data = get_top_videos(key = key,
                                howMany = 30, 
                                shorts = F, 
                                categoryID = categories$ID[i])
    
    # Some data is not accessible through the API, so a NULL check is necessary
    if (is.data.frame(video_data) ) {
      videos[[categories$Category[[i]]]] = video_data
    } 
  }
}

#  ---------------------------------------------------------------------------------------------
# | Note: No data was found for the categories "Travel & Events" and "Education"                |
# | because the API did not return any results and for Howto & Style after the                  |
# | all the filtering the function returns no data.                                             |
#  ---------------------------------------------------------------------------------------------

# After the use of the function each category counts with: 
for (i in names(videos)) {
  cat(sprintf('Category: %-22s | Videos: %-4d\n', i, nrow(videos[[i]])))
}


# Category: Film & Animation       | Videos: 6   
# Category: Autos & Vehicles       | Videos: 16  
# Category: Music                  | Videos: 28  
# Category: Pets & Animals         | Videos: 6   
# Category: Sports                 | Videos: 13  
# Category: Gaming                 | Videos: 30  
# Category: People & Blogs         | Videos: 30  
# Category: Comedy                 | Videos: 5   
# Category: Entertainment          | Videos: 3   
# Category: News & Politics        | Videos: 30  
# Category: Howto & Style          | Videos: 6   
# Category: Science & Technology   | Videos: 30  
# Category: Nonprofits & Activism  | Videos: 1   

save(categories, videos, file = 'data retrieved/videos.RData')

# ==============================================================================
# ======== FETCHING THE TOP 100 MOST RELEVANT COMMENTS FOR EACH VIDEO ==========
# ==============================================================================

# Load the necessary data and functions
# dir = '~/UN/1) Análisis de redes sociales (ARS)/2) Workplace/Youtube Scrapper/'
# setwd(dir)
# load('data retrieved/videos.RData')             # Load pre-saved video data

# =====> Load the scrapping function
source('functions/comment scrapper.R')            # Load the function for fetching comments

# =====> Load the API key
key = readLines('keys/key2')[1]                   

# =====> Fetching the actual comments
comments = list()

for (i in names(videos)) {
  cat('\n\n\n Processing information for the category: ', i, '\n\n\n')
  data = videos[[i]]                              # Extract the list of videos for the category
  comment = as.data.frame(c())                    # Initialize an empty dataframe for comments
  
  pb = txtProgressBar(min = 0, max = nrow(data), style = 3)  # Create a progress bar
  for (j in 1:nrow(data)) {
    # Fetch top comments for the current video
    video_comments = get_top_comments(
      key = key,
      videoID = data$ID[j],
      maxComments = 100,
      order = 'relevance'
    )
    comment = rbind(comment, video_comments)      # Combine comments from the current video
    setTxtProgressBar(pb, j)                      # Update progress bar
  }
  close(pb)                                       # Close progress bar
  comments[[i]] = comment                         # Store all comments for the category
}

#  -------------------------------------------------------------------------------------------
# | Note: For some videos, the API does not return all the requested comments even if there   |
# | are enough comments available. However, this does not significantly affect the analysis,  |
# | as the comments retrieved are representative and collected as intended.                   |
#  -------------------------------------------------------------------------------------------

# Summary: After running the function, the number of comments collected per category is:
for (i in names(comments)) {
  cat(sprintf('Category: %-22s | Comments: %-4d\n', i, nrow(comments[[i]])))
}

# Category: Film & Animation       | Comments: 600 
# Category: Autos & Vehicles       | Comments: 1600
# Category: Music                  | Comments: 2800
# Category: Pets & Animals         | Comments: 600 
# Category: Sports                 | Comments: 1300
# Category: Gaming                 | Comments: 2666
# Category: People & Blogs         | Comments: 2926
# Category: Comedy                 | Comments: 500 
# Category: Entertainment          | Comments: 300 
# Category: News & Politics        | Comments: 3000
# Category: Howto & Style          | Comments: 600 
# Category: Science & Technology   | Comments: 2852
# Category: Nonprofits & Activism  | Comments: 100 

# Note: Each video can be analyzed as a separate network within its respective category.

save(comments, file = 'data retrieved/comments.Rdata')


# === FETCHING THE THREAD OF THE MOST RELEVANT COMMENT THREAD FOR EACH VIDEO ===

# Load the necessary data and functions
# load(file = 'data retrieved/comments.Rdata')

# =====> Load the scrapping function
source(file = 'functions/comment Threads.R')

# =====> Load the API key
key = readLines('keys/key3')[1]                  

threads = list()
for (i in names(comments)){
  cat('\n\n\n Processing information for the category: ', i, '\n\n\n')
  tops = comments[[i]] |> 
    filter(order == 1) |> 
    select('videoID' = videoID, 
           'threadID' = commentID, 
           'commentID' = commentID, 
           'author' = author, 
           'authorID' = authorID,
           'comment' = comment, 
           'likesCount' = likesCount,
           'published' = published,
           'order' = order)
  pb = txtProgressBar(min = 0, max = nrow(tops), style = 3)  # Create a progress bar
  threads[[i]] = as.data.frame(c())
  for (j in 1:nrow(tops)){
    idParent = tops[j,"threadID"]
    idVideo = tops[j, "videoID"]
    thread = get_thread(key = key, commentID = idParent, maxReplies = 100, verbose = TRUE)
    if (length(thread) > 0){
      thread = cbind('videoID' = idVideo, thread)
      thread$order = thread$order + 1
      thread = rbind(tops[j,], thread)
      threads[[i]] = rbind(threads[[i]], thread)
    } else {
      threads[[i]] = rbind(threads[[i]], tops[j,])
    }
    setTxtProgressBar(pb, j)
  }
  setTxtProgressBar(pb, nrow(tops))
  close(pb)
}


# Summary: After running the function, the number of comments collected per category is:
for (i in names(threads)) {
  cat(sprintf('Category: %-22s | Comments: %-4d\n', i, nrow(threads[[i]])))
}

# Category: Film & Animation       | Comments: 369 
# Category: Autos & Vehicles       | Comments: 585 
# Category: Music                  | Comments: 1376
# Category: Pets & Animals         | Comments: 294 
# Category: Sports                 | Comments: 483 
# Category: Gaming                 | Comments: 948 
# Category: People & Blogs         | Comments: 424 
# Category: Comedy                 | Comments: 367 
# Category: Entertainment          | Comments: 259 
# Category: News & Politics        | Comments: 2069
# Category: Howto & Style          | Comments: 286 
# Category: Science & Technology   | Comments: 1247
# Category: Nonprofits & Activism  | Comments: 101 


save(threads, file = 'data retrieved/threads.RData')