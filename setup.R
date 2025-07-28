# 01_setup.R
library(quanteda)
library(quanteda.textstats)
library(tidyverse)
library(glmnet)
library(doParallel)
library(caret)
library(data.table)
library(future)
library(doFuture)
library(progressr)
library(foreach)
library(pROC)
library(broom)
library(spacyr)
library(Metrics)

registerDoParallel(cores = 8)

spacy <- reticulate::import("spacy")
nlp <- spacy$load("en_core_web_sm")
nlp$max_length <- 3000000

set.seed(1923)

# Define global variables to be shared across modules
features <- NULL  # Define your features here or in a separate file


# Utility function for filtering authors by document count
filter_authors_by_count <- function(df, author_col = "From", min_docs = 20, max_docs = 30) {
  auth <- df %>%
    group_by(.data[[author_col]]) %>%
    filter(n() >= min_docs & n() <= max_docs) %>%
    pull(.data[[author_col]]) %>%
    unique()
  df %>% filter(.data[[author_col]] %in% auth)
}

# Utility function for computing relative features
add_relative_feature <- function(df, numerator_col, denominator_col, new_col) {
  # Check for division by zero
  if (any(df[[denominator_col]] == 0, na.rm = TRUE)) {
    warning(paste("Division by zero detected in", denominator_col, "- setting ratio to 0 for these cases"))
    df[[new_col]] <- ifelse(df[[denominator_col]] == 0, 0, df[[numerator_col]] / df[[denominator_col]])
  } else {
    df[[new_col]] <- df[[numerator_col]] / df[[denominator_col]]
  }
  return(df)
}
