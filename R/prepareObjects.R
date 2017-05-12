#' Package Objects used for Sentiment Analysis.
#'
#' \code{prepareObjects} Internal objects used in assessing polarity.
#' @importFrom dplyr %>%
prepareObjects <- function(data) {
  spl <- split(data, data$isRetweet)
  main <- spl[['FALSE']]
  pol <- lapply(main$text, function(txt) {
    gsub("(\\.|!|\\?)+\\s+|(\\++)", " ", txt) %>%
      gsub(" http[^[:blank:]]+", "", .) %>%
      qdap::polarity(.)
  })
  value <- list(split = spl, original = main, polarity = pol)
  value
}
