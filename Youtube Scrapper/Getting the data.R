# The data was retrieved on 28/Nov/2024 by running this code specifically. 
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


source(file = 'functions/video scrapper.R')

# ============== FETCHING THE MOST POPULAR VIDEOS OF EACH CATEGORY ==================

# =====> Load the API key
key = readLines('keys/key1')[1]                    # There is more than one key in the file

# =====> Is needed to first fetch video categories
categories <- GET(url = "https://www.googleapis.com/youtube/v3/videoCategories",
                  query = list(part = "snippet", regionCode = "US", key = key))
categories = fromJSON(content(categories, as = "text"), flatten = TRUE)
categories = as.data.frame(cbind('ID' = categories$items$id,
                                 'Category' = categories$items$snippet.title,
                                 'Assignable' = categories$items$snippet.assignable))

# "Assignable" (TRUE) categories are "real" categories that can be assigned or viewed 
# through the video's metadata.

# =====> Fetchiing the actual videos
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
  # Formateamos las cadenas para que el texto esté alineado
  cat(sprintf('Category: %-22s | Videos: %-4d\n', i, nrow(videos[[i]])))
}


# Category: Film & Animation       | Videos: 1   
# Category: Autos & Vehicles       | Videos: 23  
# Category: Music                  | Videos: 28  
# Category: Pets & Animals         | Videos: 3   
# Category: Sports                 | Videos: 10  
# Category: Gaming                 | Videos: 30  
# Category: People & Blogs         | Videos: 30  
# Category: Comedy                 | Videos: 3   
# Category: Entertainment          | Videos: 1   
# Category: News & Politics        | Videos: 30  
# Category: Science & Technology   | Videos: 30  
# Category: Nonprofits & Activism  | Videos: 6   

save(categories, videos, file = 'data retrieved/videos.RData')


# ======== FETCHING THE TOP 100 MOST RELEVANT COMMENTS FOR EACH VIDEO ===========

# Load the necessary data and functions
load('data retrieved/videos.RData')               # Load pre-saved video data
source('functions/comment_scrapper.R')            # Load the function for fetching comments
key = readLines('keys/key2')[1]                   # Load the API key

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
  # Format output for aligned text display
  cat(sprintf('Category: %-22s | Comments: %-4d\n', i, nrow(comments[[i]])))
}

# Example output:
# Category: Film & Animation       | Comments: 100 
# Category: Autos & Vehicles       | Comments: 2300
# Category: Music                  | Comments: 2800
# Category: Pets & Animals         | Comments: 299 
# Category: Sports                 | Comments: 1000
# Category: Gaming                 | Comments: 2861
# Category: People & Blogs         | Comments: 3000
# Category: Comedy                 | Comments: 300 
# Category: Entertainment          | Comments: 100 
# Category: News & Politics        | Comments: 3000
# Category: Science & Technology   | Comments: 2856
# Category: Nonprofits & Activism  | Comments: 600 

# Note: Each video can be analyzed as a separate network within its respective category.

save(comments, file = 'data retrieved/comments.data')


# ====== FETCHING THE THREAD OF THE MOST RELEVANT COMMENT FOR EACH VIDEO =======