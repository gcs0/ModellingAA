prepare_doc_author_comparison <- function(doc_df_name,
                                          author_df_name,
                                          ngram_df_name,
                                          feature_cols_name,
                                          doc_id_col = "doc_id",
                                          doc_author_col = "Author",
                                          author_id_col = "Author",
                                          output_name = NULL,
                                          envir = .GlobalEnv) {
  # Fetch objects from their names
  doc_df <- get(doc_df_name, envir = envir)
  author_df <- get(author_df_name, envir = envir)
  ngram_df <- get(ngram_df_name, envir = envir)
  feature_cols <- get(feature_cols_name, envir = envir)
  
  # Run comparison using previously defined function
  result_df <- compare_doc_to_authors(
    doc_df = doc_df,
    author_df = author_df,
    feature_cols = feature_cols,
    ngram_sim_matrix = ngram_df,
    doc_id_col = doc_id_col,
    doc_author_col = doc_author_col,
    author_id_col = author_id_col
  )
  
  # Assign to environment if output name is specified
  if (!is.null(output_name)) {
    assign(output_name, result_df, envir = envir)
    message(sprintf("Result saved as '%s' in the environment.", output_name))
  }
  
  return(result_df)
}


