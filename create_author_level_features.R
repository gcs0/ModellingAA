create_author_level_features <- function(df, corpus, 
                                         author_col = "From",
                                         scaler = NULL,
                                         features_to_scale = NULL,
                                         punct_rel_func = add_punct_rel_freq,
                                         token_settings = list(remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE)) {
  library(dplyr)
  library(quanteda)
  library(quanteda.textstats)
  
  # Aggregate numeric features by author
  author_df <- df %>%
    group_by(.data[[author_col]]) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop")
  
  # Defensive: ensure author_col name preserved
  if (!author_col %in% names(author_df)) names(author_df)[1] <- author_col
  
  # Group corpus by author
  grouped_corpus <- corpus_group(corpus, groups = docvars(corpus)[[author_col]])
  
  # Ensure expected base columns exist (create as zeros if missing) to avoid later "unknown column" warnings
  expected_bases <- c("Alphabetic", "Uppercase", "AlphaToken", "Function", "Disc", "AWL", "Misspelled", "n")
  missing_bases <- setdiff(expected_bases, names(author_df))
  if (length(missing_bases) > 0) {
    warning("The following expected base columns are missing; they will be created as zeros: ", paste(missing_bases, collapse = ", "))
    for (col in missing_bases) author_df[[col]] <- 0
  }
  
  # Derived relative features with division-by-zero protection (only compute if base columns exist)
  author_df$RelCase <- ifelse(author_df$Alphabetic == 0, 0, author_df$Uppercase / author_df$Alphabetic)
  author_df$RelFunction <- ifelse(author_df$AlphaToken == 0, 0, author_df$Function / author_df$AlphaToken)
  author_df$RelDisc <- ifelse(author_df$AlphaToken == 0, 0, author_df$Disc / author_df$AlphaToken)
  author_df$RelAWL <- ifelse(author_df$AlphaToken == 0, 0, author_df$AWL / author_df$AlphaToken)
  author_df$RelMisspelled <- ifelse(author_df$AlphaToken == 0, 0, author_df$Misspelled / author_df$AlphaToken)
  
  # Add punctuation relative frequency (user-provided function)
  author_df <- punct_rel_func(author_df)
  
  # Compute CTTR per-author (use do.call to pass token settings reliably)
  toks <- do.call(tokens, c(list(grouped_corpus), token_settings))
  cttr_vals <- textstat_lexdiv(toks, measure = "CTTR")
  # Align CTTR values to author_df rows
  if ("CTTR" %in% names(cttr_vals)) {
    author_df$CTTR <- cttr_vals$CTTR
  } else if ("CTTR" %in% colnames(cttr_vals)) {
    author_df$CTTR <- cttr_vals[, "CTTR"]
  } else {
    stop("Could not extract CTTR values from textstat_lexdiv() result")
  }
  
  # Ensure we have 'n' (token count) column (created earlier or filled with zeros)
  if (!("n" %in% names(author_df))) stop("The column `n` (token count) is required for scaling but not found.")
  
  # Normalize features_to_scale argument: accept either base names ("ADJ") or scaled names ("ADJ_scaled")
  base_feats <- NULL
  if (!is.null(features_to_scale)) {
    base_feats <- sub("_scaled$", "", features_to_scale)
    # Only keep those base features that exist (we created missing_bases above, but other user features may be truly absent)
    present_base_feats <- intersect(base_feats, names(author_df))
    if (length(present_base_feats) == 0) {
      warning("None of the requested features_to_scale were found as base columns in author_df.")
    }
  }
  
  # Create per-token scaled columns for present base features (name them "<feature>_scaled")
  if (!is.null(base_feats) && length(base_feats) > 0) {
    present_base_feats <- intersect(base_feats, names(author_df))
    for (col in present_base_feats) {
      scaled_name <- paste0(col, "_scaled")
      author_df[[scaled_name]] <- ifelse(author_df$n == 0, 0, author_df[[col]] / author_df$n)
    }
  }
  
  # If an external scaler is provided, align columns and apply it safely
  if (!is.null(scaler) && !is.null(features_to_scale)) {
    # Determine the scaled column names that we should pass to scaler
    # Preferred: if scaler (caret preProcess) has $mean (or $std), use those names
    scaler_cols <- NULL
    if (!is.null(attr(scaler, "mean")) && is.numeric(attr(scaler, "mean"))) {
      scaler_cols <- names(attr(scaler, "mean"))
    } else if (!is.null(scaler$mean) && is.numeric(scaler$mean)) {
      scaler_cols <- names(scaler$mean)
    } else if (!is.null(scaler$std) && is.numeric(scaler$std)) {
      scaler_cols <- names(scaler$std)
    } else {
      # Fall back to expecting "<base>_scaled" names from features_to_scale
      scaler_cols <- paste0(sub("_scaled$", "", features_to_scale), "_scaled")
    }
    
    # Make sure required scaled columns exist in author_df; if missing, create them as zeros so predict() won't fail
    missing_scaled <- setdiff(scaler_cols, names(author_df))
    if (length(missing_scaled) > 0) {
      warning("The scaler expects these columns which are missing; creating them as zeros: ", paste(missing_scaled, collapse = ", "))
      for (mc in missing_scaled) author_df[[mc]] <- 0
    }
    
    # Subset in the correct order
    scaler_input <- author_df[, scaler_cols, drop = FALSE]
    # Ensure numeric matrix/data.frame for predict
    scaler_input <- as.data.frame(lapply(scaler_input, as.numeric), check.names = FALSE)
    
    # Apply scaler with informative error handling
    scaled_values <- tryCatch({
      predict(scaler, newdata = scaler_input)
    }, error = function(e) {
      stop("Error applying provided scaler: ", conditionMessage(e), 
           "\nExpected columns: ", paste(scaler_cols, collapse = ", "),
           "\nProvided columns present: ", paste(intersect(scaler_cols, names(author_df)), collapse = ", "))
    })
    
    # If predict returns matrix convert to data.frame
    if (is.matrix(scaled_values)) scaled_values <- as.data.frame(scaled_values, check.names = FALSE)
    if (!is.data.frame(scaled_values)) scaled_values <- as.data.frame(scaled_values)
    
    # Replace the scaled columns in author_df with transformed versions (preserve other columns)
    other_cols <- setdiff(names(author_df), names(scaled_values))
    author_df <- cbind(author_df[, other_cols, drop = FALSE], scaled_values)
  }
  
  return(author_df)
}
