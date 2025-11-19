#' Learn from manually indexed documents
#'
#' This function sends feedback to the Annif API to improve project models
#' by learning from manually assigned subjects. This helps the system
#' improve its suggestions over time based on human feedback.
#'
#' @param project_id Character string representing the project identifier
#'   (e.g., "yso-en", "yso-fi").
#' @param documents A list of documents with their correct subjects.
#' @param verbose Logical indicating whether to print debug information (default: FALSE)
#'
#' @return Invisibly returns TRUE if successful, FALSE otherwise. The function
#'   primarily produces side effects (model learning) rather than returning data.
#'
#' @examples
#' \donttest{
#' # Create a document with correct subjects for learning
#' learning_doc <- list(
#'   text = "A quick brown fox jumped over the lazy dog.",
#'   subjects = list(
#'     list(
#'       uri = "http://www.yso.fi/onto/yso/p2228",
#'       label = "red fox"
#'     ),
#'     list(
#'       uri = "http://www.yso.fi/onto/yso/p5319",
#'       label = "dog"
#'     )
#'   ),
#'   metadata = list(
#'     source = "example",
#'     language = "en"
#'   )
#' )
#'
#' # Send learning feedback to the model
#' success <- annif_learn(
#'   project_id = "yso-en",
#'   documents = list(learning_doc)
#' )
#'
#' if (success) {
#'   cat("Learning feedback successfully submitted\n")
#' } else {
#'   cat("Failed to submit learning feedback\n")
#' }
#' }
#'
#' @importFrom httr POST status_code content accept
#' @importFrom jsonlite toJSON
#' @export
annif_learn <- function(project_id, documents, verbose = FALSE) {
  # Input validation
  if (missing(project_id) || is.null(project_id) || project_id == "") {
    stop("Project ID must be specified")
  }

  if (missing(documents) || is.null(documents) || length(documents) == 0) {
    stop("Documents must be specified and non-empty")
  }

  if (!is.list(documents)) {
    stop("Documents must be a list of document objects")
  }

  # Validate document structure
  for (i in seq_along(documents)) {
    doc <- documents[[i]]

    if (is.null(doc$text) || doc$text == "") {
      stop("Document ", i, " must have non-empty text")
    }

    if (is.null(doc$subjects) || length(doc$subjects) == 0) {
      stop("Document ", i, " must have at least one subject")
    }

    # Validate subjects
    for (j in seq_along(doc$subjects)) {
      subject <- doc$subjects[[j]]
      if (is.null(subject$uri) || subject$uri == "") {
        stop("Subject ", j, " in document ", i, " must have a URI")
      }
      if (is.null(subject$label) || subject$label == "") {
        stop("Subject ", j, " in document ", i, " must have a label")
      }
    }
  }

  # Construct URL
  url <- paste0("https://api.annif.org/v1/projects/", project_id, "/learn")

  if (verbose) {
    cat("URL:", url, "\n")
    cat("Request body:\n")
    print(jsonlite::toJSON(documents, auto_unbox = TRUE, pretty = TRUE))
  }

  tryCatch({
    # Make POST request
    response <- httr::POST(
      url = url,
      httr::accept("application/json"),
      httr::content_type("application/json"),
      body = jsonlite::toJSON(documents, auto_unbox = TRUE, pretty = FALSE)
    )

    if (verbose) {
      cat("Status code:", httr::status_code(response), "\n")
    }

    # Check for successful response (204 No Content)
    if (httr::status_code(response) == 204) {
      if (verbose) {
        cat("Learning successful - model updated\n")
      }
      return(invisible(TRUE))

    } else if (httr::status_code(response) == 403) {
      stop("Operation not allowed: Learning may be disabled for this project or requires authentication")

    } else if (httr::status_code(response) == 404) {
      stop("Project '", project_id, "' not found")

    } else if (httr::status_code(response) == 503) {
      stop("Service temporarily unavailable")

    } else {
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      stop("API request failed with status ", httr::status_code(response), ": ", error_content)
    }

  }, error = function(e) {
    if (verbose) {
      cat("Error details:", e$message, "\n")
    }
    warning("Learning failed: ", e$message)
    return(invisible(FALSE))
  })
}
