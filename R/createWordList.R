# Compilation of Lists on either Side of Emotive Spectrum
#
# \code{createWordList} Generates list of positive and negative words.
#
# @param x An atomic vector of polarities
#' @importFrom dplyr %>%
createWordList <- function(x) {
  pwt <- sapply(x, function(p) {
    words = c(positiveWords = paste(p[[1]]$pos.words[[1]], collapse = ' '),
              negativeWords = paste(p[[1]]$neg.words[[1]], collapse = ' '))
    gsub('-', '', words)
  }) %>%
    apply(1, paste, collapse = ' ') %>%
    tm::stripWhitespace() %>%
    strsplit(' ') %>%
    sapply(table)
  pwt
}
