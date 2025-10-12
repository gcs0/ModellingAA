#' Create N-gram Similarity Matrix for Authorship Attribution
#'
#' This function creates n-gram similarity matrices between documents and authors
#' using quanteda for text processing and parallel computation for efficiency.
#' Based on cosine similarity of n-gram vectors with leave-one-out author profiling.
#'
#' @param corpus A quanteda corpus object containing the documents
#' @param doc_df A data frame with document metadata including doc_id and author columns
#' @param doc_id_col String name of document ID column (default: "doc_id")
#' @param author_col String name of author column (default: "Author")  
#' @param ngram_range Integer vector specifying n-gram range (default: 1:2)
#' @param min_termfreq Minimum term frequency for dfm_trim (default: 5)
#' @param num_cores Number of cores for parallel processing (default: detectCores() - 2)
#' @param progress Logical whether to show progress bar (default: TRUE)
#' @param remove_punct Logical whether to remove punctuation (default: TRUE)
#' @param remove_numbers Logical whether to remove numbers (default: TRUE)
#'
#' @return A matrix with document IDs as rows and author names as columns,
#'         containing cosine similarity scores between documents and author profiles
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' ngram_matrix <- create_ngram_similarity_matrix(
#'   corpus = train_corpus,
#'   doc_df = train_df,
#'   doc_id_col = "doc_id",
#'   author_col = "Author"
#' )
#' 
#' # Custom parameters
#' ngram_matrix <- create_ngram_similarity_matrix(
#'   corpus = train_corpus,
#'   doc_df = train_df,
#'   ngram_range = 1:3,
#'   min_termfreq = 10,
#'   num_cores = 4
#' )
#' }
#'
#' @export
create_ngram_similarity_matrix <- function(corpus,
                                         doc_df,
                                         doc_id_col = "doc_id",
                                         author_col = "Author",
                                         ngram_range = 1:2,
                                         min_termfreq = 5,
                                         num_cores = NULL,
                                         progress = TRUE,
                                         remove_punct = TRUE,
                                         remove_numbers = TRUE) {
  
  # Load required libraries
  if (!requireNamespace("quanteda", quietly = TRUE)) {
    stop("Package 'quanteda' is required for this function.")
  }
  if (!requireNamespace("Matrix", quietly = TRUE)) {
    stop("Package 'Matrix' is required for this function.")
  }
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("Package 'future' is required for parallel processing.")
  }
  if (!requireNamespace("foreach", quietly = TRUE)) {
    stop("Package 'foreach' is required for parallel processing.")
  }
  if (!requireNamespace("progressr", quietly = TRUE)) {
    stop("Package 'progressr' is required for progress tracking.")
  }
  
  library(quanteda)
  library(Matrix)
  library(future)
  library(foreach)
  library(progressr)
  
  # Input validation
  if (!inherits(corpus, "corpus")) {
    stop("corpus must be a quanteda corpus object")
  }
  
  if (!is.data.frame(doc_df)) {
    stop("doc_df must be a data frame")
  }
  
  if (!(doc_id_col %in% names(doc_df))) {
    stop(paste("Column", doc_id_col, "not found in doc_df"))
  }
  
  if (!(author_col %in% names(doc_df))) {
    stop(paste("Column", author_col, "not found in doc_df"))
  }
  
  if (!is.numeric(ngram_range) || length(ngram_range) != 2) {
    stop("ngram_range must be a numeric vector of length 2 (e.g., 1:2)")
  }
  
  # Set up parallel processing
  if (is.null(num_cores)) {
    num_cores <- max(1, parallel::detectCores() - 2)
  }
  
  message(sprintf("Creating n-gram similarity matrix using %d cores...", num_cores))
  
  # Prepare tokens and dfm
  message("Preparing n-gram tokens and document-feature matrix...")
  toks <- tokens(corpus, remove_punct = remove_punct, remove_numbers = remove_numbers) %>%
    tokens_ngrams(n = ngram_range)
  
  dfm_all <- dfm(toks) %>% 
    dfm_trim(min_termfreq = min_termfreq)
  
  # Get authors and document IDs from doc_df
  doc_names <- quanteda::docnames(corpus)
  authors <- doc_df[[author_col]][match(doc_names, doc_df[[doc_id_col]])]
  
  if (any(is.na(authors))) {
    stop("Some documents in corpus not found in doc_df. Check doc_id matching.")
  }
  
  unique_authors <- sort(unique(authors))
  message(sprintf("Processing %d documents across %d authors...", 
                  length(doc_names), length(unique_authors)))
  
  # Convert to sparse matrix
  doc_mat <- as(dfm_all, "dgCMatrix")
  doc_author_indices <- split(seq_along(authors), authors)
  
  # Precompute full author vectors
  grouped_dfm <- dfm_group(dfm_all, groups = authors)
  grouped_mat <- as(grouped_dfm, "dgCMatrix")
  
  # Set up parallel processing
  plan(multisession, workers = num_cores)
  
  # Progress bar setup
  if (progress) {
    handlers(global = TRUE)
    handlers("progress")
  }
  
  # Main computation with progress tracking
  message("Computing similarities...")
  
  similarity_results <- if (progress) {
    progressr::with_progress({
      p <- progressr::progressor(steps = nrow(doc_mat))
      
      foreach(i = 1:nrow(doc_mat), .combine = rbind, .packages = "Matrix") %dopar% {
        p()  # increment progress bar
        
        doc_vec <- doc_mat[i, ]
        doc_author <- authors[i]
        doc_norm <- sqrt(sum(doc_vec^2))
        
        # Leave-one-out: remove current document from its author's profile
        group_vec <- grouped_mat[doc_author, ]
        updated_group_vec <- group_vec - doc_vec
        
        # Handle case where author has only one document
        if (length(doc_author_indices[[doc_author]]) == 1) {
          updated_group_vec <- Matrix(0, nrow = 1, ncol = ncol(doc_mat), sparse = TRUE)
          colnames(updated_group_vec) <- colnames(grouped_mat)
        }
        
        # Update the grouped matrix
        final_group_mat <- grouped_mat
        final_group_mat[doc_author, ] <- updated_group_vec
        
        # Compute cosine similarities
        row_norms <- sqrt(rowSums(final_group_mat^2))
        sims <- (final_group_mat %*% as.numeric(doc_vec)) / (row_norms * doc_norm)
        sims <- as.numeric(sims)
        sims[is.na(sims)] <- 0
        
        setNames(sims, unique_authors)
      }
    })
  } else {
    foreach(i = 1:nrow(doc_mat), .combine = rbind, .packages = "Matrix") %dopar% {
      doc_vec <- doc_mat[i, ]
      doc_author <- authors[i]
      doc_norm <- sqrt(sum(doc_vec^2))
      
      group_vec <- grouped_mat[doc_author, ]
      updated_group_vec <- group_vec - doc_vec
      
      if (length(doc_author_indices[[doc_author]]) == 1) {
        updated_group_vec <- Matrix(0, nrow = 1, ncol = ncol(doc_mat), sparse = TRUE)
        colnames(updated_group_vec) <- colnames(grouped_mat)
      }
      
      final_group_mat <- grouped_mat
      final_group_mat[doc_author, ] <- updated_group_vec
      
      row_norms <- sqrt(rowSums(final_group_mat^2))
      sims <- (final_group_mat %*% as.numeric(doc_vec)) / (row_norms * doc_norm)
      sims <- as.numeric(sims)
      sims[is.na(sims)] <- 0
      
      setNames(sims, unique_authors)
    }
  }
  
  # Clean up parallel processing
  plan(sequential)
  
  # Convert to data frame and set proper row names
  ngram_matrix <- as.data.frame(similarity_results)
  rownames(ngram_matrix) <- doc_names;
  
  message(sprintf("N-gram similarity matrix created: %d documents x %d authors", 
                  nrow(ngram_matrix), ncol(ngram_matrix)))  
  
  return(ngram_matrix)