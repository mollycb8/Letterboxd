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

source('./Code/config.R')

movies_df <- read.csv("./Data/diary.csv") %>%
  select(c(Name, Rating, Rewatch, Watched.Date)) %>%
  filter(between(as.Date(Watched.Date), as.Date(paste0(year(Sys.Date()) - 1, "-01-01")), as.Date(paste0(year(Sys.Date()) - 1, "-12-31"))))

movies_df$Rewatch <- ifelse(movies_df$Rewatch == "Yes", "Yes", "No")



# Function to fetch movie details using TMDb API
get_movie_details <- function(movie_id) {
  base_url <- paste0("https://api.themoviedb.org/3/movie/", movie_id)
  credits_url <- paste0(base_url, "/credits")
  api_key <- api_key
  
  response <- GET(credits_url,
                  query = list(api_key = api_key))
  
  movie_credits <- content(response, "parsed")
  return(movie_credits)
}


# Create empty DataFrames for cast, crew, and genre
cast_df <- data.frame(Name = character(0), Cast = character(0))
crew_df <- data.frame(Name = character(0), Department = character(0), Job = character(0), CrewName = character(0))
genre_df <- data.frame(Name = character(0), Genre = character(0))
release_df <- data.frame(Name = character(0), Release.Date = character(0))

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
      if (!is.null(movie_credits$genres)) {
        genres <- sapply(movie_credits$genres, function(genre) genre$name)
        genre <- genres
      }
      
      genre_entry <- data.frame(Name = rep(movie_name, length(genre)), Genre = genre)
      genre_df <- bind_rows(genre_df, genre_entry)
      
    } else {
      cat("Failed to fetch movie credits for:", movie_name, "\n")
    }
  }
}

# Now cast_df, crew_df, genre_df, and release_df contain the added Release.Date column associated with the Name column from movies_df

# Filter crew_df for directors
dir_df <- crew_df %>% 
  filter(Job == "Director")




















  
  
