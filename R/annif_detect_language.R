#' Detect the language of a text using Annif API
#'
#' This function uses the Annif API to detect the language of a given text
#' from the supported languages: English ("en"), Finnish ("fi"),
#' Sami ("se"), and Swedish ("sv").
#'
#' @param text Character string containing the text to analyze
#' @param languages Character vector of candidate language codes. Must be a subset of:
#'   c("en", "fi", "se", "sv"). If NULL, all supported languages are used.
#'
#' @return A tibble containing language detection results
#' @examples
#' \donttest{
#' # Detect language with all supported languages
#' result <- annif_detect_language(
#'   text = "A quick brown fox jumped over the lazy dog."
#' )
#' print(result)
#'
#' # Detect with specific subset of languages
#' result <- annif_detect_language(
#'   text = "Tämä on suomenkielinen teksti."
#' )
#' print(result)
#'
#' # Get the top detected language
#' if (!is.null(result)) {
#'   top_language <- result[which.max(result$score), ]
#'   cat("Detected language:", top_language$language,
#'       "(confidence:", round(top_language$score, 3), ")\n")
#' }
#' }
#'
#' @importFrom httr POST status_code content accept
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom tibble tibble
#' @export
annif_detect_language <- function(text, languages = NULL) {
  # Supported languages
  supported_langs <- c("en", "fi", "se", "sv")

  # Input validation
  if (missing(text) || is.null(text) || text == "") {
    stop("Text must be specified and non-empty")
  }

  if (!is.character(text) || length(text) != 1) {
    stop("Text must be a single character string")
  }

  # Set default languages if not provided
  if (is.null(languages)) {
    languages <- supported_langs
  }

  # Validate language codes
  if (!is.character(languages)) {
    stop("Languages must be a character vector")
  }

  invalid_langs <- setdiff(languages, supported_langs)
  if (length(invalid_langs) > 0) {
    stop("Unsupported language codes: ", paste(invalid_langs, collapse = ", "),
         ". Supported languages are: ", paste(supported_langs, collapse = ", "))
  }

  if (length(languages) == 0) {
    stop("At least one language must be specified")
  }

  # Prepare request body
  request_body <- list(
    text = text,
    languages = as.list(languages)
  )

  # Define URL
  url <- "https://api.annif.org/v1/detect-language"

  tryCatch({
    # Make POST request
    response <- httr::POST(
      url = url,
      httr::accept("application/json"),
      httr::content_type("application/json"),
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE, pretty = FALSE)
    )

    # Check for successful response
    if (httr::status_code(response) == 200) {
      # Parse the JSON response
      response_data <- jsonlite::fromJSON(
        httr::content(response, "text", encoding = "UTF-8"),
        simplifyDataFrame = FALSE
      )

      # Convert to tibble and handle NULL languages
      results_list <- list()
      for (result in response_data$results) {
        lang <- if (!is.null(result$language)) result$language else NA_character_
        results_list[[length(results_list) + 1]] <- tibble::tibble(
          language = lang,
          score = result$score
        )
      }

      # Combine all results and remove NA languages
      results_df <- do.call(rbind, results_list)
      results_df <- results_df[!is.na(results_df$language), ]

      # Sort by score (descending)
      results_df <- results_df[order(-results_df$score), ]

      return(results_df)

    } else if (httr::status_code(response) == 400) {
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      stop("Bad request: ", error_content)

    } else {
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      stop("API request failed with status ", httr::status_code(response), ": ", error_content)
    }

  }, error = function(e) {
    # Handle connection errors and other issues
    if (grepl("Bad request", e$message)) {
      stop(e$message)
    } else {
      warning("Failed to detect language: ", e$message)
      return(NULL)
    }
  })
}
