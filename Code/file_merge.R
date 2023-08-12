# TODO:
# import all csvs
# clean data, get rid of unnecessary columns
# merge multiple files together
# do API stuff

# Import libraries
package.list <- c("dplyr",
                  "ggplot2",
                  'tidyverse',
                  'lubridate',
                  "httr",
                  "jsonlite"
)
packageLoad <- function(packages){
  for (i in packages) {
    if (!require(i, character.only = TRUE)) {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}
packageLoad(package.list)

movies_df <- read.csv("./Data/diary.csv") %>%
  select(c(Name, Rating, Rewatch, Watched.Date))

movies_df$Rewatch <- ifelse(movies_df$Rewatch == "Yes", "Yes", "No")

library(httr)
library(dplyr)

# Function to fetch movie details using TMDb API
get_movie_details <- function(movie_id) {
  base_url <- paste0("https://api.themoviedb.org/3/movie/", movie_id)
  credits_url <- paste0(base_url, "/credits")
  api_key <- "d17f44f1c5c44eba600f834d1663f6f9"
  
  response <- GET(credits_url,
                  query = list(api_key = api_key))
  
  movie_credits <- content(response, "parsed")
  return(movie_credits)
}

filter_movies_by_time <- function(time_frame) {
  today <- Sys.Date()
  start_date <- switch(
    time_frame,
    "all time" = as.Date("1900-01-01"),
    "past year" = today - years(1),
    "past 6 months" = today - months(6)
  )
  
  filtered_movies <- movies_df %>%
    filter(Date >= start_date) %>%
    mutate(Date = as.Date(Date))
  
  return(filtered_movies)
}



# Create empty DataFrames for cast, crew, and genre
cast_df <- data.frame(Name = character(0), Cast = character(0))
crew_df <- data.frame(Name = character(0), Department = character(0), Job = character(0), CrewName = character(0))
genre_df <- data.frame(Name = character(0), Genre = character(0))

# Fetch movie details for each movie title in the DataFrame
for (i in 1:nrow(movies_df)) {
  movie_name <- movies_df$Name[i]
  search_url <- "https://api.themoviedb.org/3/search/movie"
  api_key <- "d17f44f1c5c44eba600f834d1663f6f9"
  
  response <- GET(search_url,
                  query = list(api_key = api_key, query = movie_name))
  
  search_results <- content(response, "parsed")
  
  if (length(search_results$results) > 0) {
    movie_id <- search_results$results[[1]]$id
    cat("Fetching details for movie:", movie_name, "\n")
    
    movie_credits <- get_movie_details(movie_id)
    
    # Extract cast information
    cast <- character(0)
    if (!is.null(movie_credits$cast)) {
      cast <- sapply(movie_credits$cast, function(actor) actor$name)
    }
    
    cast_entry <- data.frame(Name = rep(movie_name, length(cast)), Cast = cast)
    cast_df <- bind_rows(cast_df, cast_entry)
    
    # Extract crew information
    if (!is.null(movie_credits$crew)) {
      crew_data <- movie_credits$crew
      for (crew_entry in crew_data) {
        department <- crew_entry$department
        job <- crew_entry$job
        crew_name <- crew_entry$name
        
        crew_entry <- data.frame(Name = rep(movie_name, 1), Department = department, Job = job, CrewName = crew_name)
        crew_df <- bind_rows(crew_df, crew_entry)
      }
    }
    
    # Extract genre information
    genre <- character(0)
    if (!is.null(movie_info$genres)) {
      genres <- sapply(movie_info$genres, function(genre) genre$name)
      genre <- genres
    }
    
    genre_entry <- data.frame(Name = rep(movie_name, length(genre)), Genre = genre)
    genre_df <- bind_rows(genre_df, genre_entry)
  }
}

dir_df <- crew_df %>% 
  filter(Job == "Director")
















  
  
