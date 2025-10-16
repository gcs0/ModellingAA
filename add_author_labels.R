# add_author_labels.R

# Function to add labels to comparison_df
add_author_labels <- function(comparison_df) {
  comparison_df$label <- ifelse(comparison_df$doc_Author == comparison_df$author_Author, 1, 0)
  return(comparison_df)
}

# Example usage
# Assuming comparison_df is a dataframe with columns doc_Author and author_Author
comparison_df <- data.frame(
  doc_Author = c("Author A", "Author B", "Author C"),
  author_Author = c("Author A", "Author D", "Author C")
)

# Adding labels
labeled_df <- add_author_labels(comparison_df)

# View the result
print(labeled_df)