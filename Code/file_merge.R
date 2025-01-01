# TODO:


# Import libraries


package.list <- c("dplyr",
                  "ggplot2",
                  'tidyverse',
                  'lubridate',
                  "httr",
                  "jsonlite",
                  "plotly"
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

# Set the root directory
setwd("C:/Users/Molly/Documents/GitHub/Letterboxd")

# Print the current working directory to confirm
print(getwd())


#Source config code for api key

source('./Code/config.R')

#Read in the letterboxd diary

library(httr)
library(dplyr)

# Function to fetch movie details using TMDb API
get_movie_details <- function(movie_name, api_key) {
  search_url <- "https://api.themoviedb.org/3/search/movie"
  
  # Search for the movie by name
  response <- GET(search_url, query = list(api_key = api_key, query = movie_name))
  search_results <- content(response, "parsed")
  
  # Check if results are found
  if (!is.null(search_results$results) && length(search_results$results) > 0) {
    # Get the first movie ID from the results
    movie_id <- search_results$results[[1]]$id
    
    # Fetch movie credits using the movie ID
    credits_url <- paste0("https://api.themoviedb.org/3/movie/", movie_id, "/credits")
    credits_response <- GET(credits_url, query = list(api_key = api_key))
    
    # Return the movie details if the request is successful
    if (status_code(credits_response) == 200) {
      return(content(credits_response, "parsed"))
    }
  }
  
  # Return NULL if no movie is found or if an error occurs
  return(NULL)
}



movies_df <- read.csv("./Data/diary.csv")

# Filter movies in the dataframe based on API results
movies_df <- movies_df %>%
  rowwise() %>%
  mutate(Movie_Details = list(get_movie_details(Name, api_key))) %>%
  filter(!is.null(Movie_Details)) %>%
  ungroup()

# Remove the Movie_Details column if not needed
movies_df <- select(movies_df, -Movie_Details)

movies_df$Watched.Date <- as.Date(movies_df$Watched.Date)

# Filter for movies watched in 2024
movies_df <- movies_df %>%
  filter(Watched.Date >= as.Date("2024-01-01") & Watched.Date <= as.Date("2024-12-31"))




# Create empty DataFrames for cast, crew, and genre
cast_df <- data.frame(Name = character(0), Cast = character(0))
crew_df <- data.frame(Name = character(0), Department = character(0), Job = character(0), CrewName = character(0))
release_df <- data.frame(Name = character(0), Release.Date = character(0))
genre_df <- data.frame(Name = character(0), Genre = character(0), stringsAsFactors = FALSE)


# Loop through movies_df
for (i in 1:nrow(movies_df)) {
  movie_name <- movies_df$Name[i]
  search_url <- "https://api.themoviedb.org/3/search/movie"
  api_key <- api_key
  
  response <- GET(search_url,
                  query = list(api_key = api_key, query = movie_name))
  
  search_results <- content(response, "parsed")
  
  if (length(search_results$results) > 0) {
    movie_id <- search_results$results[[1]]$id
    cat("Fetching details for movie:", movie_name, "\n")
    
    # Fetch movie details directly using /movie/{movie_id} endpoint
    movie_url <- paste0("https://api.themoviedb.org/3/movie/", movie_id)
    movie_response <- GET(movie_url, query = list(api_key = api_key))
    
    if (status_code(movie_response) == 200) {
      movie_details <- content(movie_response, "parsed")
    
    # Fetch movie credits
    credits_url <- paste0("https://api.themoviedb.org/3/movie/", movie_id, "/credits")
    credits_response <- tryCatch(
      expr = GET(credits_url, query = list(api_key = api_key)),
      error = function(e) e
    )
    
    if (inherits(credits_response, "error")) {
      cat("Failed to fetch movie credits for:", movie_name, "\n")
    } else if (status_code(credits_response) == 200) {
      movie_credits <- content(credits_response, "parsed")
      
      # Extract release date as character string
      release_date_str <- as.character(search_results$results[[1]]$release_date)
      
      # Update release date in movies_df
      movies_df$Release.Date[movies_df$Name == movie_name] <- release_date_str
      
      # Extract runtime information
      runtime_minutes <- NULL
      if (!is.null(movie_details$runtime)) {
        runtime_minutes <- as.numeric(movie_details$runtime)
      }
      
      # Update runtime in minutes in movies_df
      movies_df$Runtime[movies_df$Name == movie_name] <- runtime_minutes
      
      
      # Extract cast information
      cast <- character(0)
      if (!is.null(movie_credits$cast)) {
        cast <- sapply(movie_credits$cast, function(actor) actor$name)
      }
      
      cast_entry <- data.frame(Name = rep(movie_name, length(cast)), Cast = cast, stringsAsFactors = FALSE)
      cast_df <- bind_rows(cast_df, cast_entry)
      
      # Extract crew information
      if (!is.null(movie_credits$crew)) {
        crew_data <- movie_credits$crew
        for (crew_entry in crew_data) {
          department <- crew_entry$department
          job <- crew_entry$job
          crew_name <- crew_entry$name
          
          crew_entry <- data.frame(Name = rep(movie_name, 1), Department = department, Job = job, CrewName = crew_name, stringsAsFactors = FALSE)
          crew_df <- bind_rows(crew_df, crew_entry)
        }
      }
      
      # Extract genre information
      genres <- character(0)
      if (!is.null(movie_details$genres)) {
        genres <- sapply(movie_details$genres, function(genre) genre$name)
      }
      
      # Create a data frame for the current movie and append to genre_df
      genre_entry <- data.frame(Name = rep(movie_name, length(genres)), Genre = genres, stringsAsFactors = FALSE)
      genre_df <- dplyr::bind_rows(genre_df, genre_entry)
    } else {
      print(paste("No genre information found for movie:", movie_name))
        }
      }
  }
}
  
# Filter crew_df for directors
dir_df <- crew_df %>% 
  filter(Job == "Director")
















  
  
