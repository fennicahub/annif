#' Suggest subjects for a batch of documents via Annif REST API
#'
#' This function processes up to 32 documents in a single batch request to the
#' Annif API for automatic subject indexing.
#'
#' @param project_id Character string representing the project identifier
#' @param texts Character vector of document texts to analyze (maximum 32 documents)
#' @param doc_ids Optional character vector of document IDs
#' @param limit Integer specifying maximum number of results per document (default: 10)
#' @param threshold Numeric value between 0 and 1 for minimum score (default: 0)
#' @param language Character string for language of subject labels (default: NULL)
#' @param verbose Logical indicating whether to print debug information (default: FALSE)
#' @return A tibble containing suggested subjects for all documents
#' @export
annif_batch_indexing <- function(project_id,
                                 texts,
                                 doc_ids = NULL,
                                 limit = 10,
                                 threshold = 0,
                                 language = NULL,
                                 verbose = FALSE) {

  # Input validation
  if (missing(project_id) || is.null(project_id) || project_id == "") {
    stop("Project ID must be specified")
  }

  if (missing(texts) || is.null(texts) || length(texts) == 0) {
    stop("Texts must be specified and non-empty")
  }

  if (length(texts) > 32) {
    stop("Maximum 32 documents allowed per batch request")
  }

  # Handle document IDs
  if (is.null(doc_ids)) {
    doc_ids <- paste0("doc-", seq_along(texts))
  } else if (length(doc_ids) != length(texts)) {
    stop("doc_ids must be the same length as texts")
  }

  # Construct request body - simpler approach
  documents <- lapply(seq_along(texts), function(i) {
    list(
      text = texts[i],
      document_id = doc_ids[i]
    )
  })

  request_body <- list(documents = documents)

  # Construct URL
  base_url <- paste0("https://api.annif.org/v1/projects/", project_id, "/suggest-batch")

  # Build query parameters
  query_params <- list(
    limit = as.integer(limit),
    threshold = as.numeric(threshold)
  )

  if (!is.null(language) && language != "") {
    query_params$language <- language
  }

  # Build complete URL with query parameters
  url <- httr::modify_url(base_url, query = query_params)

  if (verbose) {
    cat("URL:", url, "\n")
    cat("Request body:\n")
    print(jsonlite::toJSON(request_body, auto_unbox = TRUE, pretty = TRUE))
  }

  tryCatch({
    # Make POST request
    response <- httr::POST(
      url = url,
      httr::accept("application/json"),
      httr::content_type("application/json"),
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE, pretty = FALSE)
    )

    if (verbose) {
      cat("Status code:", httr::status_code(response), "\n")
      cat("Response headers:\n")
      print(httr::headers(response))
    }

    # Check for successful response
    if (httr::status_code(response) == 200) {
      response_content <- httr::content(response, "text", encoding = "UTF-8")

      if (verbose) {
        cat("Response content:\n")
        cat(response_content, "\n")
      }

      response_data <- jsonlite::fromJSON(response_content, simplifyDataFrame = FALSE)

      # Process results
      results_list <- list()

      for (doc_response in response_data) {
        doc_id <- doc_response$document_id
        doc_results <- doc_response$results

        if (length(doc_results) > 0) {
          for (result in doc_results) {
            results_list[[length(results_list) + 1]] <- tibble::tibble(
              document_id = doc_id,
              label = result$label %||% NA_character_,
              uri = result$uri %||% NA_character_,
              notation = result$notation %||% NA_character_,
              score = result$score %||% NA_real_
            )
          }
        }
      }

      if (length(results_list) > 0) {
        final_results <- do.call(rbind, results_list)
      } else {
        final_results <- tibble::tibble(
          document_id = character(),
          label = character(),
          uri = character(),
          notation = character(),
          score = numeric()
        )
      }

      return(final_results)

    } else {
      error_content <- httr::content(response, "text", encoding = "UTF-8")
      stop("HTTP ", httr::status_code(response), ": ", error_content)
    }

  }, error = function(e) {
    if (verbose) {
      cat("Error details:", e$message, "\n")
    }
    warning("Batch indexing failed: ", e$message)
    return(NULL)
  })
}
