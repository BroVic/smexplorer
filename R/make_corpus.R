# Prepare Text for Use in Sentiment Analysis

#' @import tm
make_corpus <- function(GText, stem = TRUE) {
  corp <- VCorpus(VectorSource(GText))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removeWords, stopwords("english"))
  if(stem)
    corp <- tm_map(corp, stemDocument)
  names(corp) <- names(GText)
  corp
}
