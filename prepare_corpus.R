# 04_prepare_corpus.R

prepare_split_corpora <- function(full_corpus, split_dfs, docid_col = "docid.corpusFilteredAll.") {
  stopifnot(all(c("train_df", "valid_df", "test_df") %in% names(split_dfs)))
  
  list(
    train_corpus = corpus(full_corpus[docnames(full_corpus) %in% split_dfs$train_df[[docid_col]]]),
    valid_corpus = corpus(full_corpus[docnames(full_corpus) %in% split_dfs$valid_df[[docid_col]]]),
    test_corpus  = corpus(full_corpus[docnames(full_corpus) %in% split_dfs$test_df[[docid_col]]])
  )
}
