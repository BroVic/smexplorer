#' Treatment for Text ahead of Use for Plots.
#'
#' \code{processBagofwords} Process text prior to visualisation.
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
