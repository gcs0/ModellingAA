add_punct_counts <- function(corpus, df) {
  df$Dot     <- str_count(corpus, fixed("."))
  df$GS      <- str_count(corpus, fixed("<")) + str_count(corpus, fixed(">"))
  df$Coma    <- str_count(corpus, fixed(","))
  df$Slash   <- str_count(corpus, fixed("/"))
  df$QMark   <- str_count(corpus, fixed("?"))
  df$DQuote  <- str_count(corpus, fixed("\""))
  df$SQuote  <- str_count(corpus, fixed("'"))
  df$Dash    <- str_count(corpus, fixed("-"))
  df$EMark   <- str_count(corpus, fixed("!"))
  df$Colon   <- str_count(corpus, fixed(":"))
  df$SemiC   <- str_count(corpus, fixed(";"))
  df$And     <- str_count(corpus, fixed("&"))
  df$Bracket <- str_count(corpus, fixed("(")) + str_count(corpus, fixed(")"))
  
  return(df)
}

add_punct_rel_freq <- function(df) {
  punks <- c("Dot", "GS", "Coma", "Slash", "QMark", "DQuote", "SQuote", "Dash",
             "EMark", "Colon", "SemiC", "And", "Bracket")
  
  for (feature in punks) {
    scaled_col <- paste0("Rel", feature)
    df[[scaled_col]] <- df[[feature]] / df$Token
  }
  
  return(df)
}
