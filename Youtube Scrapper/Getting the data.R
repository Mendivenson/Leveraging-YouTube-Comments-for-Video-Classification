# The data was retrieved on 26/Nov/2024 by running this code specifically. 
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
key = readLines('keys/key2')                       # There is more than one key in the file

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
                                howMany = 25, 
                                shorts = F, 
                                categoryID = categories$ID[i])
    
    # Some data is not accessible through the API, so a NULL check is necessary
    if (is.data.frame(video_data)) {
      videos[[categories$Category[[i]]]] = video_data
    } else {
      cat('\n\n\n====> No data was found for the category:', categories$Category[[i]], '\n\n\n')
    }
  }
}

# ----------------------------------------------------------------------------------------------
# | No data was found for the categories "Travel & Events" and "Education"                      |
# | because the API did not return any results                                                  |
# ----------------------------------------------------------------------------------------------

# After the use of the function each category counts with: 
for (i in names(videos)) {
  # Formateamos las cadenas para que el texto esté alineado
  cat(sprintf('Category: %-22s | n: %-4d\n', i, nrow(videos[[i]])))
}


# Category: Film & Animation       | n: 2   
# Category: Autos & Vehicles       | n: 21  
# Category: Music                  | n: 25  
# Category: Pets & Animals         | n: 3   
# Category: Sports                 | n: 10  
# Category: Gaming                 | n: 25  
# Category: People & Blogs         | n: 25  
# Category: Comedy                 | n: 4   
# Category: Entertainment          | n: 1   
# Category: News & Politics        | n: 25  
# Category: Howto & Style          | n: 3   
# Category: Science & Technology   | n: 25  
# Category: Nonprofits & Activism  | n: 4 

save(categories, videos, file = 'data retrieved/videos.RData')
