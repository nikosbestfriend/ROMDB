#' @title Get OMDB Item Ratings
#' @name get_omdb_item_ratings
#' @description This function searches Item Ratings.
#' @author Alberto Almuiña 
#' @param omdb_id String with the omdb_id for a movie/series.
#' @param API_KEY OMBD Api Key. Default: Get the Api Key from system environment. Use Sys.setenv('API_KEY' = 'XXXXX'). More information in: http://www.omdbapi.com/apikey.aspx
#' @return
#' Return a tibble with the movie/series ratings.
#' @export
#' @examples
#' \dontrun{
#' get_omdb_item_ratings(odmb_id = c('tt0120338', 'tt0114746'))
#' @changes propsed by Christen Black
#'}

#These changes now allow you to put in a vector of id's instead of one at a time
get_omdb_item_ratings <- function(omdb_ids, API_KEY = Sys.getenv('API_KEY')) {
  
  # Initialize an empty tibble to store the results
  all_ratings <- tibble(OMDB_ID = character(), Source = character(), Value = character())
  
  # Loop through each omdb_id
  for (omdb_id in omdb_ids) {
    
    # Build the API URL
    url <- str_glue("http://www.omdbapi.com/?apikey={API_KEY}")
    
    # Make the API request
    res <- RETRY('GET', url = url,
                 query = list(i = omdb_id,
                              r = 'json'),
                 quiet = TRUE) %>% content
    
    # Check if the API returned a valid response
    if (is.null(res) || !("Ratings" %in% names(res))) {
      message(str_glue("No valid response for OMDB ID: {omdb_id}"))
      next  # Skip this movie if there's no valid response
    }
    
    # Get the ratings from the response
    ratings <- res$Ratings
    
    # If there are no ratings, skip to the next movie
    if (length(ratings) == 0) {
      next
    }
    
    # Filter to get only the Metacritic rating
    metacritic_rating <- map_df(seq(length(ratings)), function(x) {
      if (ratings[[x]]$Source == "Metacritic") {
        tibble(OMDB_ID = omdb_id,  # Include the OMDB ID for reference
               Source = ratings[[x]]$Source,
               Value = ratings[[x]]$Value)
      }
    })
    
    # Remove NA values
    metacritic_rating <- na.omit(metacritic_rating)
    
    # If no Metacritic rating is found, skip to the next movie
    if (nrow(metacritic_rating) == 0) {
      message(str_glue("No Metacritic rating found for OMDB ID: {omdb_id}"))
      next
    }
    
    # Append the Metacritic rating to the results tibble
    all_ratings <- bind_rows(all_ratings, metacritic_rating)
  }
  
  # Return the final tibble with all ratings
  return(all_ratings)
}

