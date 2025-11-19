#' Get detailed information about a specific project with expanded structure
#'
#' This function retrieves comprehensive information about a specified project
#' and returns it as a tibble with expanded nested structures, showing all
#' the individual components of nested objects.
#'
#' @param project_id A character string representing the project identifier
#' (e.g., "yso-fi", "yso-en", "hogwarts").
#'
#' @return A tibble with multiple rows showing expanded project information,
#' where nested structures are broken out into individual rows:
#' \itemize{
#'   \item \code{backend} - Backend information as named list
#'   \item \code{is_trained} - Whether project is trained
#'   \item \code{language} - Project language
#'   \item \code{modification_time} - Last modification timestamp
#'   \item \code{name} - Human-readable name of the project
#'   \item \code{project_id} - Unique identifier for the project
#'   \item \code{vocab} - Vocabulary information as named list
#'   \item \code{vocab_language} - Vocabulary language used by project
#' }
#'
#' @examples
#' \donttest{
#' # Get expanded project information
#' project_info <- get_annif_project_id("yso-fi")
#' print(project_info)
#' }
#'
#' @importFrom httr GET accept content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom lubridate as_datetime
#' @export
get_annif_project_id <- function(project_id) {
  # Validate input
  if (missing(project_id) || is.null(project_id) || project_id == "") {
    stop("Project ID must be specified")
  }

  if (!is.character(project_id)) {
    stop("Project ID must be a character string")
  }

  # Define the URL for the specific project endpoint
  url <- paste0("https://api.annif.org/v1/projects/", project_id)

  tryCatch({
    # Make the API request using httr::GET
    response <- httr::GET(
      url = url,
      httr::accept("application/json")
    )

    # Check for successful response
    if (httr::status_code(response) == 200) {
      # Parse the JSON response with flattening but don't simplify data frame
      # This will create the multi-row structure you want
      response_data <- jsonlite::fromJSON(
        httr::content(response, "text", encoding = "UTF-8"),
        simplifyDataFrame = TRUE,
        flatten = TRUE
      )

      # Convert to tibble - this should create the multi-row structure
      project_df <- tibble::as_tibble(response_data)

      # Convert modification_time to POSIXct if available
      if (requireNamespace("lubridate", quietly = TRUE) &&
          "modification_time" %in% colnames(project_df)) {
        project_df$modification_time <- lubridate::as_datetime(project_df$modification_time)
      }

      return(project_df)

    } else if (httr::status_code(response) == 404) {
      # Handle project not found
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      error_msg <- tryCatch({
        error_data <- jsonlite::fromJSON(error_content, simplifyDataFrame = FALSE)
        if (!is.null(error_data$detail)) {
          error_data$detail
        } else {
          "Project not found"
        }
      }, error = function(e) {
        "Project not found"
      })

      stop("Project '", project_id, "' not found: ", error_msg)

    } else {
      # Handle other HTTP errors
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      stop(sprintf("API request failed with status %s: %s",
                   httr::status_code(response),
                   error_content))
    }

  }, error = function(e) {
    # Handle network errors or other issues
    if (grepl("Project.*not found", e$message)) {
      stop(e$message)
    } else {
      warning("Failed to retrieve project information: ", e$message)
      return(NULL)
    }
  })
}
