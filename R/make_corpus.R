#' Prepare Text for Use in Sentiment Analysis
#'
#' \code{make_corpus} Builds a corpus.
#'
#' @param GText A character vector
#' @param stem Use stemming. Defaults to \code{TRUE}
#' @return A corpus
#' @import tm
#' @importFrom dplyr %>%
make_corpus <- function(GText, stem = TRUE) {
  corp <- VCorpus(VectorSource(GText)) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords("english"))
  if(stem)
    corp <- tm_map(corp, stemDocument)

  names(corp) <- names(GText)
  corp
}
