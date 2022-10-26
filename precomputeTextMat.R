precomputeTextMat <- function(data, bag_words = TRUE, stop_words = TRUE, stylometry = TRUE, punctuation = TRUE) {
  ## data is a vector of text reviews, one for each customer/review
  ## true_row is the text sample of the target that the vendor has in their proprietary data
  
  df <- data.frame()
  for (i in 1:length(data)) {
    rvw <- iconv(data[i], "UTF-8", "ASCII", sub = "")
    df <- stylometry(i,rvw, df, bag_words = bag_words, stop_words = stop_words, stylometry  = stylometry , punctuation = punctuation)
  }
  df
}
