# 02_features.R

# Source punctuation functions
source("puncmarkFunc.R")

compute_base_features <- function(corpus, dfm, Df) {
  Df$Alphabetic <- str_count(corpus, "[A-Za-z]")
  Df$AlphaToken <- ntoken(tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE))
  Df$Token <- ntoken(tokens(corpus, remove_punct = FALSE, remove_numbers = TRUE, remove_url = TRUE))
  Df$Uppercase <- str_count(corpus, "[A-Z]")
  Df$RelCase <- ifelse(Df$Alphabetic == 0, 0, Df$Uppercase / Df$Alphabetic)

  
  cttr_result <- textstat_lexdiv(tokens(corpus, remove_punct = TRUE, remove_numbers = TRUE, remove_url = TRUE), measure = "CTTR")
  Df$CTTR <- pmin(cttr_result$CTTR, 12)
  Df<- add_punct_counts(corpus, Df)
  Df<- add_punct_rel_freq(Df)
  return(Df)
}

add_pos_scaled_features <- function(corpus, df) {
  telSpac <- spacy_parse(corpus, pos = TRUE, multithread = TRUE)
  telSpac <- telSpac %>%
    count(doc_id, pos) %>%
    pivot_wider(names_from = pos, values_from = n, values_fill = 0) %>%
    arrange(as.integer(str_extract(doc_id, "\\d+")))
  
  telSpac$n <- rowSums(telSpac[-1], na.rm = TRUE)
  df_with_features <- cbind(df, telSpac[-1])
  
  telSpacScaled <- telSpac %>%
    mutate(across(.cols = -c(doc_id, n), .fns = ~ .x / n, .names = "{.col}_scaled"))
  
  df <- cbind(df, telSpacScaled[, grepl("_scaled$", names(telSpacScaled))])
  return(df)
}
