# Using python's wordcloud function since R's wordclouds aren't great
# text should be preprocessed into single string via `prepare_text()` function
generate_wordcloud <- function(text, max_words, title = NULL) {
  
  if (is.vector(text) == FALSE) stop("Text must be in vector format...\n")
  library(reticulate)
  # import packages 
  plt <- import("matplotlib.pyplot")
  wc <- import("wordcloud")
  
  max_words <- as.integer(max_words)
  wc = wc$WordCloud(width = 1600L, height = 500L, max_words = max_words)$generate(text)
  # remove axis
  plt$axis("off")
  plt$imshow(wc, interpolation = "bilinear")
  if (is.null(title)) {
    plt$show()
  } else {
    plt$title(title, loc='left', fontsize=25, pad=20)
    plt$show()
  }
}