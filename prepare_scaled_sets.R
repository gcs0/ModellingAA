# 05_prepare_scaled_sets.R

prepare_scaled_sets <- function(train_df, valid_df, test_df, features, corpus_list, punct_rel_func) {
  # train scaler
  scaler <- preProcess(train_df[, features], method = c("center", "scale"))
  
  # Scale and append other vars for all sets
  P2train_df <- cbind(predict(scaler, train_df[, features]), train_df[, setdiff(names(train_df), features)])
  P2valid_df <- cbind(predict(scaler, valid_df[, features]), valid_df[, setdiff(names(valid_df), features)])
  P2test_df  <- cbind(predict(scaler, test_df[, features]), test_df[, setdiff(names(test_df), features)])
  
  # Ensure doc_id column is present
  P2train_df$doc_id <- P2train_df$docid.corpusFilteredAll.
  train_df$doc_id   <- train_df$docid.corpusFilteredAll.
  valid_df$doc_id   <- valid_df$docid.corpusFilteredAll.
  test_df$doc_id    <- test_df$docid.corpusFilteredAll.
  
  # Author-level features (assumes create_author_level_features is already defined)
  P2author_train_df <- create_author_level_features(
    df = train_df,
    corpus = corpus_list$train_corpus,
    author_col = "From",
    scaler = scaler,
    features_to_scale = features,
    punct_rel_func = punct_rel_func
  )
  
  P2author_valid_df <- create_author_level_features(
    df = valid_df,
    corpus = corpus_list$valid_corpus,
    author_col = "From",
    scaler = scaler,
    features_to_scale = features,
    punct_rel_func = punct_rel_func
  )
  
  P2author_test_df <- create_author_level_features(
    df = test_df,
    corpus = corpus_list$test_corpus,
    author_col = "From",
    scaler = scaler,
    features_to_scale = features,
    punct_rel_func = punct_rel_func
  )
  
  return(list(
    scaler = scaler,
    P2train_df = P2train_df,
    P2valid_df = P2valid_df,
    P2test_df = P2test_df,
    P2author_train_df = P2author_train_df,
    P2author_valid_df = P2author_valid_df,
    P2author_test_df = P2author_test_df
  ))
}
