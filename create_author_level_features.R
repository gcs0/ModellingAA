create_author_level_features <- function(df, corpus, 
                                         author_col = "Author",
                                         scaler = NULL,
                                         features_to_scale = NULL,
                                         punct_rel_func = add_punct_rel_freq,
                                         token_settings = list(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE)) {
  library(dplyr)
  library(quanteda)
  library(quanteda.textstats)
  
  
  # Step 1: Aggregate numeric features by author
  author_df <- df %>%
    group_by(.data[[author_col]]) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  
  # Defensive:  ensure author_col name preserved
  if (!author_col %in% names(author_df)) names(author_df)[1] <- author_col
  
  # Step 2: Group corpus by author
  grouped_corpus <- corpus_group(corpus, groups = docvars(corpus)[[author_col]])
  
  # Step 3: Compute RelCase (Uppercase / Alphabetic)
  author_df$RelCase <- ifelse(author_df$Alphabetic == 0, 0, 
                              author_df$Uppercase / author_df$Alphabetic)
  
  # Step 4: Add punctuation relative frequency
  author_df <- punct_rel_func(author_df)
  
  # Step 5: Compute CTTR per-author
  toks <- do.call(tokens, c(list(grouped_corpus), token_settings))
  cttr_result <- textstat_lexdiv(toks, measure = "CTTR")
  author_df$Type <- cttr_result
  author_df$CTTR <- author_df$Type$CTTR
  
  # Step 6: Scale POS features by token count (features / n)
  # Identify columns to scale (excluding author_col and n)
  if (!is.null(features_to_scale)) {
    base_feats <- sub("_scaled$", "", features_to_scale)
    cols_to_scale <- setdiff(names(author_df), c(author_col, "n"))
    cols_to_scale <- intersect(cols_to_scale, base_feats)
    
    scaled_cols <- author_df %>%
      mutate(across(
        .cols = all_of(cols_to_scale),
        .fns = ~ .x / n,
        .names = "{.col}_scaled"
      ))
    
    # Get only the scaled columns
    scaled_only <- scaled_cols[, grepl("_scaled$", names(scaled_cols))]
    
    # Remove old scaled columns if they exist, then add new ones
    author_df <- author_df[, !(names(author_df) %in% names(scaled_only))]
    author_df <- cbind(author_df, scaled_only)
  }
  
  # Step 7: Apply Z-score standardization using provided scaler
  if (!is.null(scaler) && !is.null(features_to_scale)) {
    # Get scaler column names
    scaler_cols <- if (! is.null(scaler$mean)) names(scaler$mean) else features_to_scale
    
    # Ensure all required columns exist
    missing_cols <- setdiff(scaler_cols, names(author_df))
    if (length(missing_cols) > 0) {
      for (mc in missing_cols) author_df[[mc]] <- 0
    }
    
    # Apply scaler to feature columns
    scaled_values <- predict(scaler, author_df[, scaler_cols, drop = FALSE])
    
    # Combine:  scaled features + non-feature columns
    other_cols <- setdiff(names(author_df), scaler_cols)
    author_df <- cbind(scaled_values, author_df[, other_cols, drop = FALSE])
  }
  
  # Clean up intermediate Type column
  author_df$Type <- NULL
  
  return(author_df)
}
