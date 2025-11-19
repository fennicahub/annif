#' Suggest subjects for a given text using the Annif REST API
#'
#' This function sends a text to the Annif API for automatic subject indexing
#' and returns suggested subjects with their labels, URIs, scores, and notations.
#'
#' @param project_id Character string representing the project identifier
#'   (e.g., "yso-en", "yso-fi", "hogwarts").
#' @param text Character string containing the input text for which subjects
#'   are to be suggested.
#' @param limit Integer specifying the maximum number of results to return.
#'   Default is 10.
#' @param threshold Numeric value between 0 and 1 specifying the minimum score
#'   threshold for results. Default is 0 (return all results).
#' @param language Character string specifying the language of subject labels.
#'   Default is NULL (uses project default).
#'
#' @return A tibble containing suggested subjects with the following columns:
#'
#' @examples
#' suggestions <- annif_subject_indexing(
#'   project_id = "yso-en",
#'   text = "Jean Sibelius orchestra music and composer"
#' )
#' print(suggestions)
#'
#' @importFrom httr POST status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @export
annif_subject_indexing <- function(project_id, text, limit = 10, threshold = 0, language = NULL) {
  # Validate required parameters
  if (missing(project_id) || is.null(project_id) || project_id == "") {
    stop("Project ID must be specified")
  }

  if (missing(text) || is.null(text) || text == "") {
    stop("Text must be specified")
  }

  if (!is.character(project_id) || !is.character(text)) {
    stop("Project ID and text must be character strings")
  }

  if (!is.numeric(limit) || limit < 1) {
    stop("Limit must be a positive integer")
  }

  if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {
    stop("Threshold must be a numeric value between 0 and 1")
  }

  # Define the correct API endpoint
  url <- paste0("https://api.annif.org/v1/projects/", project_id, "/suggest")

  # Prepare the request body
  body <- list(
    text = text,
    limit = as.integer(limit),
    threshold = as.numeric(threshold)
  )

  # Add language parameter only if specified
  if (!is.null(language) && language != "") {
    body$language <- language
  }

  tryCatch({
    # Make the API request
    response <- httr::POST(
      url = url,
      body = body,
      encode = "form",
      httr::accept("application/json")
    )

    # Check for successful response
    if (httr::status_code(response) == 200) {
      # Parse the JSON response
      json_data <- jsonlite::fromJSON(
        httr::content(response, "text", encoding = "UTF-8"),
        simplifyDataFrame = TRUE
      )

      # Convert results to tibble
      suggestions_df <- tibble::as_tibble(json_data$results)

      # Reorder columns for consistency
      preferred_order <- c("label", "uri", "score", "notation")
      existing_cols <- intersect(preferred_order, colnames(suggestions_df))
      suggestions_df <- suggestions_df[, existing_cols]

      return(suggestions_df)

    } else if (httr::status_code(response) == 404) {
      stop("Project '", project_id, "' not found")

    } else if (httr::status_code(response) == 400) {
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      stop("Bad request: ", error_content)

    } else if (httr::status_code(response) == 503) {
      stop("Service temporarily unavailable")

    } else {
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      stop("API request failed with status ", httr::status_code(response), ": ", error_content)
    }

  }, error = function(e) {
    # Handle connection errors and other issues
    if (grepl("Project.*not found", e$message) ||
        grepl("Bad request", e$message) ||
        grepl("Service temporarily unavailable", e$message)) {
      stop(e$message)
    } else {
      warning("Failed to get subject suggestions: ", e$message)
      return(NULL)
    }
  })
}
