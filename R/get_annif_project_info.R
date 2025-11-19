#' Get a list of available vocabularies
#'
#' This function retrieves information about all available vocabularies
#' in the Annif service, including their IDs, languages, sizes, and
#' loading status.
#'
#' @return A data frame (tibble) containing information about available
#' @examples
#' \donttest{
#' # Get all available vocabularies
#' vocabs <- get_annif_vocabulary_info()
#' print(vocabs)
#' }
#'
#' @importFrom httr GET accept content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @export
get_annif_vocabulary_info <- function() {
  # Define the URL for the vocabularies endpoint
  url <- "https://api.annif.org/v1/vocabs"

  tryCatch({
    # Make the API request using httr::GET
    response <- httr::GET(
      url = url,
      httr::accept("application/json")
    )

    # Check for successful response
    if (httr::status_code(response) != 200) {
      stop(sprintf("API request failed with status %s: %s",
                   httr::status_code(response),
                   httr::content(response, "text", encoding = "UTF-8")))
    }

    # Parse the JSON response
    response_data <- jsonlite::fromJSON(
      httr::content(response, "text", encoding = "UTF-8"),
      simplifyDataFrame = FALSE
    )

    # Extract and process the vocabs list
    vocabs_list <- response_data$vocabs

    # Create a tibble with proper list columns
    vocabs_df <- tibble::tibble(
      vocab_id = sapply(vocabs_list, function(x) x$vocab_id),
      languages = lapply(vocabs_list, function(x) x$languages),
      size = sapply(vocabs_list, function(x) x$size),
      loaded = sapply(vocabs_list, function(x) x$loaded)
    )

    return(vocabs_df)

  }, error = function(e) {
    warning("Failed to retrieve vocabulary list: ", e$message)
    return(NULL)
  })
}
