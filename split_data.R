# 03_split_data.R

split_authors <- function(df, author_col = "From", seed = 1923, min_docs = 20, max_docs = 30, split_ratio = c(0.7, 0.289, 0.011)) {
  set.seed(seed)
  
  # Filter authors based on doc counts
  valid_authors <- df %>%
    group_by(.data[[author_col]]) %>%
    filter(n() >= min_docs & n() <= max_docs) %>%
    pull(.data[[author_col]]) %>%
    unique()
  
  df <- df[df[[author_col]] %in% valid_authors, ]
  
  authors <- unique(df[[author_col]])
  shuffled_authors <- sample(authors)
  total_size <- floor(0.95 * length(shuffled_authors))
  
  train_size <- floor(split_ratio[1] * total_size)
  valid_size <- floor(split_ratio[2] * total_size)
  test_size  <- total_size - train_size - valid_size
  
  train_authors <- shuffled_authors[1:train_size]
  valid_authors <- shuffled_authors[(train_size + 1):(train_size + valid_size)]
  test_authors  <- shuffled_authors[(train_size + valid_size + 1):(train_size + valid_size + test_size)]
  
  list(
    train_df = df %>% filter(.data[[author_col]] %in% train_authors),
    valid_df = df %>% filter(.data[[author_col]] %in% valid_authors),
    test_df  = df %>% filter(.data[[author_col]] %in% test_authors)
  )
}
