create_author_level_features <- function(df, corpus, 
                                         author_col = "From",
                                         scaler = NULL,
                                         features_to_scale = NULL,
                                         punct_rel_func = add_punct_rel_freq,
                                         token_settings = list(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE)) {
  library(dplyr)
  library(quanteda)
  library(quanteda.textstats)
  
  # Group numeric features by author
  author_df <- df %>%
    group_by(.data[[author_col]]) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  
  # Group corpus by author
  grouped_corpus <- corpus_group(corpus, groups = docvars(corpus)[[author_col]])
  
  # Derived features with division by zero protection
  author_df$RelCase <- ifelse(author_df$Alphabetic == 0, 0, author_df$Uppercase / author_df$Alphabetic)
  author_df$RelFunction <- ifelse(author_df$AlphaToken == 0, 0, author_df$Function / author_df$AlphaToken)
  author_df$RelDisc <- ifelse(author_df$AlphaToken == 0, 0, author_df$Disc / author_df$AlphaToken)
  author_df$RelAWL <- ifelse(author_df$AlphaToken == 0, 0, author_df$AWL / author_df$AlphaToken)
  author_df$RelMisspelled <- ifelse(author_df$AlphaToken == 0, 0, author_df$Misspelled / author_df$AlphaToken)
  
  # Add punctuation relative frequency
  author_df <- punct_rel_func(author_df)
  
  # Compute CTTR
  toks <- tokens(grouped_corpus, !!!token_settings)
  cttr_vals <- textstat_lexdiv(toks, measure = "CTTR")
  author_df$CTTR <- cttr_vals$CTTR
  
  # Scale features by n (per-author total tokens)
  if (!("n" %in% names(author_df))) {
    stop("The column `n` (token count) is required for scaling but not found.")
  }
  
  scaled_df <- author_df %>%
    mutate(across(.cols = -c(all_of(author_col), n),
                  .fns = ~ ifelse(n == 0, 0, .x / n),
                  .names = "{.col}_scaled"))
  
  # Keep only scaled columns that match existing features
  if (!is.null(features_to_scale)) {
    scaled_cols <- grep("_scaled$", names(scaled_df), value = TRUE)
    selected_scaled <- scaled_df[, intersect(scaled_cols, paste0(features_to_scale, "_scaled"))]
    author_df <- cbind(author_df[, !(names(author_df) %in% colnames(selected_scaled))], selected_scaled)
  }
  
  # Apply scaler
  if (!is.null(scaler) && !is.null(features_to_scale)) {
    scaled_values <- predict(scaler, author_df[, features_to_scale])
    author_df <- cbind(scaled_values, author_df[, setdiff(names(author_df), features_to_scale)])
  }
  
  return(author_df)
}
