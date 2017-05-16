# Let's R CMD check run smoothly
globalVariables(".")

#' Package Objects used for Sentiment Analysis.
#'
#' \code{prepareObjects} Internal objects used in assessing polarity.
#'
#' @param data A dataframe
#' @return A list of 3 containing
#'     - a split data frame (along the lines of factor \code{isRetweet})
#'     - \code{main}: the data frame wherein \code{isRetweet == FALSE}
#'     - polarity for text in \code{main}
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
