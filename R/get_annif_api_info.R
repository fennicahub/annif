#' Get basic information about the Annif REST API
#'
#' This function retrieves basic information such as the title and version
#' of the Annif API service. It connects to the Annif REST API endpoint
#' and returns metadata about the API service.
#' @examples
#' # Get API information
#' api_info <- get_annif_api_info()
#' print(api_info)
#' @importFrom httr GET accept content status_code
#' @importFrom jsonlite fromJSON
#' @export
get_annif_api_info <- function() {
  # Define the base URL for the Annif API
  base_url <- "https://api.annif.org/v1/"

  tryCatch({
    # Make the API request using httr::GET
    response <- httr::GET(
      url = base_url,
      httr::accept("application/json")
    )

    # Check for successful response
    if (httr::status_code(response) != 200) {
      stop(sprintf("API request failed with status %s: %s",
                   httr::status_code(response),
                   httr::content(response, "text", encoding = "UTF-8")))
    }

    # Parse the JSON response
    api_info <- jsonlite::fromJSON(
      httr::content(response, "text", encoding = "UTF-8")
    )

    return(api_info)

  }, error = function(e) {
    warning("Failed to retrieve API information: ", e$message)
    return(NULL)
  })
}
