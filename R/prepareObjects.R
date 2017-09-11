# Let's R CMD check run smoothly
globalVariables(".")

# Package Objects used for Sentiment Analysis.
#
# \code{prepareObjects} Internal objects used in assessing polarity.
#
# @param data A dataframe generated from downloaded Twitter data
# @return A list of length 3 containing:
#     - \code{main}: the data frame of original tweets
#     (\code{isRetweet == FALSE})
#     - \code{RT}: the data frame of retweets (\code{isRetweet == TRUE})
#     - a vector representing the polarity of \emph{text} in \code{main}
#
#' @importFrom dplyr %>%
prepareObjects <- function(data) {
  spl <- split(data, data$isRetweet)
  main <- spl[['FALSE']]
  retwts <- spl[['TRUE']]
  `[[.qdap_hash` <- `[[.data.frame`
  pol <- lapply(main$text, function(txt) {
    gsub("(\\.|!|\\?)+\\s+|(\\++)", " ", txt) %>%
      gsub(" http[^[:blank:]]+", "", .) %>%
      qdap::polarity(.)
  })
  value <- list(original = main, retweets = retwts, polarity = pol)
  value
}
