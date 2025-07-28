compare_doc_to_authors <- function(doc_df, 
                                   author_df, 
                                   feature_cols,
                                   ngram_sim_matrix,
                                   doc_id_col = "doc_id",
                                   doc_author_col = "Author",
                                   author_id_col = "Author") {
  # Check if author identifiers exist
  if (!(author_id_col %in% colnames(author_df)) && is.null(rownames(author_df))) {
    stop("Author data must have rownames or an 'Author' column.")
  }
  
  # Ensure rownames are set for author_df
  if (author_id_col %in% colnames(author_df)) {
    rownames(author_df) <- author_df[[author_id_col]]
  }
  
  author_names <- rownames(author_df)
  author_feats <- author_df[, feature_cols]
  
  result_list <- vector("list", nrow(doc_df))
  
  for (i in seq_len(nrow(doc_df))) {
    doc <- doc_df[i, ]
    doc_id <- as.character(doc[[doc_id_col]])
    doc_author <- doc[[doc_author_col]]
    doc_feats <- as.numeric(doc[feature_cols])
    
    # Compute absolute difference: |doc_feat - author_feat|
    diffs <- sweep(author_feats, 2, doc_feats, FUN = function(author_val, doc_val) abs(doc_val - author_val))
    
    # Retrieve n-gram similarity scores
    if (!(doc_id %in% rownames(ngram_sim_matrix))) {
      stop(paste("Document ID", doc_id, "not found in ngram_sim_matrix"))
    }
    
    ngram_sims <- as.numeric(ngram_sim_matrix[doc_id, author_names])
    
    # Build result row
    tmp <- data.frame(
      doc_id = rep(doc_id, length(author_names)),
      doc_Author = rep(doc_author, length(author_names)),
      author_Author = author_names,
      diffs,
      ngram_sim = ngram_sims,
      row.names = NULL
    )
    
    result_list[[i]] <- tmp
  }
  
  # Combine all
  final_df <- do.call(rbind, result_list)
  return(final_df)
}
