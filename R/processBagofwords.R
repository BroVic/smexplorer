# Let's R CMD check run smoothly
globalVariables(".")

#' Treatment for Text ahead of Use for Plots.
#'
#' \code{processBagofwords} Process text prior to visualisation.
#' @param x A data frame
#' @param table A list with different sign values for polarity
#' @importFrom dplyr %>%
processBagofWords <- function(x, table) {
  pt <- sapply(x, function(subdata) {
    paste(tolower(subdata$text), collapse = ' ') %>%
      gsub(' http|@)[^[:blank:]]+', '', .) %>%
      gsub('[[:punct:]]', '', .)
  }) %>%
    structure(names = c('negative', 'neutral', 'positive'))
  pt['negative'] <- tm::removeWords(pt['negative'],
                                    names(table$negativeWords))
  pt['positive'] <- tm::removeWords(pt['positive'],
                                    names(table$positiveWords))
  pt
}
