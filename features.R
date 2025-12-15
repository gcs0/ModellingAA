# 02_features.R

# Source punctuation functions
source("puncmarkFunc.R")

compute_base_features <- function(corpus, Df) {
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
  # Build a stable doc_id vector aligned to df rows
  doc_id_all <- if (inherits(corpus, "corpus")) {
    quanteda::docnames(corpus)
  } else if (!is.null(names(corpus)) && length(names(corpus)) == length(corpus)) {
    names(corpus)
  } else {
    as.character(seq_along(corpus))
  }
  # Ensure lengths match
  if (length(doc_id_all) != nrow(df)) {
    stop("Length of corpus (", length(doc_id_all), ") does not match nrow(df) (", nrow(df), ").")
  }
  
  # Parse with explicit doc_id/text frame to make doc_id mapping explicit
  parse_in <- data.frame(doc_id = doc_id_all, text = as.character(corpus), stringsAsFactors = FALSE)
  tel <- spacyr::spacy_parse(parse_in, pos = TRUE, multithread = TRUE)
  
  # Count POS per doc
  counts <- tel |>
    dplyr::filter(pos != "SPACE") |>
    dplyr::count(doc_id, pos, name = "n")  
  # Ensure all doc_ids are present (including docs with zero tokens or only NA POS)
  counts <- tidyr::complete(
    counts,
    doc_id = doc_id_all,
    pos,
    fill = list(n = 0L)
  )
  
  # Wide POS matrix, ordered by the original corpus order
  telSpac <- counts |>
    tidyr::pivot_wider(names_from = pos, values_from = n, values_fill = 0) |>
    dplyr::mutate(doc_id = as.character(doc_id)) |>
    dplyr::arrange(match(doc_id, doc_id_all))
  
  # Sanity check: must match df rows now
  if (nrow(telSpac) != nrow(df)) {
    stop("Internal: POS matrix rows (", nrow(telSpac), ") do not match df rows (", nrow(df), ").")
  }
  
  # Total tokens (across POS) per doc
  telSpac <- telSpac |>
    dplyr::mutate(n = rowSums(dplyr::across(-doc_id), na.rm = TRUE))
  
  # Scaled POS features, handling n == 0
  telSpacScaled <- telSpac |>
    dplyr::mutate(
      dplyr::across(
        -c(doc_id, n),
        ~ ifelse(n > 0, .x / n, 0),
        .names = "{.col}_scaled"
      )
    )
  
  # Bind features to df, dropping doc_id (row order already matches)
  df <- dplyr::bind_cols(
    df,
    telSpac[, setdiff(names(telSpac), "doc_id"), drop = FALSE],
    telSpacScaled[, grepl("_scaled$", names(telSpacScaled)), drop = FALSE]
  )
  df
}
