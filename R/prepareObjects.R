# Package Objects used for Sentiment Analysis.

prepareObjects <- function(data) {
  spl <- split(data, data$isRetweet)
  main <- spl[['FALSE']]
  retwts <- spl[['TRUE']]
  pol <- lapply(main$text, function(x) {
    txt <- gsub("(\\.|!|\\?)+\\s+|(\\++)", " ", x)
    txt <- gsub(" http[^[:blank:]]+", "", txt)
    p_list <- qdap::polarity(txt)
  })
  value <- list(original = main, retweets = retwts, polarity = pol)
  value
}
