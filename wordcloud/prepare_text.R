# function to clean text...text column should be labeled 'text'
prepare_text <- function(data, remove_stopwords = TRUE) {
  if (is.null(data$text)) stop("Text column to process must be named 'text'...\n")
  # url 
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  if (remove_stopwords == TRUE) {
    data |> 
      mutate(text = tolower(text), 
             text = stringr::str_replace_all(text, url_pattern, ""),
             text = stringr::str_replace_all(text, "@\\w+", ""), # remove hashtags
             text = trimws(stringr::str_replace_all(text, "#\\w+", ""))) |> # remove @handles
      tidytext::unnest_tokens(word, text) |> 
      dplyr::anti_join(tidytext::stop_words) |>
      summarise(word = paste(word, collapse = " ")) |> 
      pull(word)
  } else {
    data |> 
      mutate(text = tolower(text), 
             text = stringr::str_replace_all(text, url_pattern, ""),
             text = stringr::str_replace_all(text, "@\\w+", ""), # remove hashtags
             text = trimws(stringr::str_replace_all(text, "#\\w+", ""))) |> # remove @handles
      tidytext::unnest_tokens(word, text) |> 
      summarise(word = paste(word, collapse = " ")) |> 
      pull(word)
  }
}