dir = 'UN/1) Análisis de redes sociales (ARS)/2) Workplace/'
setwd(dir)

# install.packages('httr')
# install.packages('jsonlite')

library(httr)
library(jsonlite)
library(dplyr)
library(here)

key = 'API_KEY'
key = readChar(key, file.info(key)$size)

channel_id <- "UCxX9wt5FWQUAAz4UrysqK9A"  # CS Dojo Channel ID
user_id <- "numberphile"  # Numberphile Username
base <- "https://www.googleapis.com/youtube/v3/"


# Construct the API call
api_params <- 
  paste(paste0("key=", key), 
        paste0("id=", channel_id), 
        "part=snippet,contentDetails,statistics",
        sep = "&")
api_call <- paste0(base, "channels", "?", api_params)
api_result <- GET(api_call)
json_result <- content(api_result, "text", encoding="UTF-8")


category_response <- GET(
  url = "https://www.googleapis.com/youtube/v3/videoCategories",
  query = list(
    part = "snippet",
    regionCode = 'US',
    key = key
  )
)


categories <- fromJSON(content(category_response, as = "text"), flatten = TRUE)
categories$items[, c("id", "snippet.title")]


get_videos_by_category <- function(api_key, video_category_id, page_token = NULL) {
  query_params <- list(
    part = "snippet",
    type = "video",
    videoCategoryId = video_category_id,
    maxResults = 50,
    key = api_key,
    pageToken = page_token,
    regionCode = 'US',
    order = 'relevance'
  )
  
  # Realizar la solicitud
  response <- GET(url = "https://www.googleapis.com/youtube/v3/search", query = query_params)
  fromJSON(content(response, as = "text"), flatten = TRUE)
}

# Obtener los primeros 100 videos de una categoría
video_category_id <- "10"  # Ejemplo: ID de la categoría "Música"
videos_page_1 <- get_videos_by_category(key, video_category_id)
videos_page_2 <- get_videos_by_category(key, video_category_id, videos_page_1$nextPageToken)

# Extraer los IDs de los videos de ambas páginas
video_ids = c(videos_page_1$items$id.videoId, videos_page_2$items$id.videoId)
video_titles = c(videos_page_1$items$snippet.title, videos_page_2$items$snippet.title)
Videos = cbind('ID' = video_ids, 'Título' = video_titles)
# Mostrar los primeros 100 IDs de videos
video_ids


# Entonces necesito verificar o filtrar los top 100 vídeos de habla inglesa:
#   - Solamente comentarios en inglés
#   - Solamente vídeos que reciban comentarios y que no sean pay per view
#   - Solamente los primeros comentarios