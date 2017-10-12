# Prepare Text for Use in Sentiment Analysis

#' @import tm
make_corpus <- function(GText, stem = TRUE) {
  corp <- VCorpus(VectorSource(GText))
  corp <- tm_map(removePunctuation(corp))
  corp <- tm_map(stripWhitespace(corp))
  corp <- tm_map(content_transformer(tolower(corp)))
  corp <- tm_map(removeWords(corp), stopwords("english"))
  if(stem)
    corp <- tm_map(stemDocument(corp))
  names(corp) <- names(GText)
  corp
}
