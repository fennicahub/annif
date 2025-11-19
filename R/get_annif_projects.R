#' Get a list of available projects
#'
#' This function retrieves information about all available projects
#' in the Annif service, including their configuration, training status,
#' and associated vocabulary information.
#'
#' @return A data frame (tibble) containing information about available
#' @examples
#' \donttest{
#' # Get all available projects
#' projects <- get_annif_projects()
#' print(projects)
#' }
#'
#' @importFrom httr GET accept content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom lubridate as_datetime
#' @export
get_annif_projects <- function() {
  # Define the URL for the projects endpoint
  url <- "https://api.annif.org/v1/projects"

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

    # Extract and process the projects list
    projects_list <- response_data$projects

    # Create a tibble with flattened structure
    projects_df <- tibble::tibble(
      project_id = sapply(projects_list, function(x) x$project_id),
      name = sapply(projects_list, function(x) x$name),
      language = sapply(projects_list, function(x) x$language),
      backend_id = sapply(projects_list, function(x) x$backend$backend_id),
      vocab_id = sapply(projects_list, function(x) x$vocab$vocab_id),
      vocab_languages = lapply(projects_list, function(x) x$vocab$languages),
      vocab_size = sapply(projects_list, function(x) x$vocab$size),
      vocab_loaded = sapply(projects_list, function(x) x$vocab$loaded),
      vocab_language = sapply(projects_list, function(x) x$vocab_language),
      is_trained = sapply(projects_list, function(x) x$is_trained),
      modification_time = sapply(projects_list, function(x) x$modification_time)
    )

    # Convert modification_time to POSIXct if lubridate is available
    if (requireNamespace("lubridate", quietly = TRUE)) {
      projects_df$modification_time <- lubridate::as_datetime(projects_df$modification_time)
    }

    return(projects_df)

  }, error = function(e) {
    warning("Failed to retrieve project list: ", e$message)
    return(NULL)
  })
}
