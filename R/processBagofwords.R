# Treatment for Text ahead of Use for Plots.

processBagofWords <- function(x, table) {
  pt <- sapply(x, function(subdata) {
    sdt <- paste(tolower(subdata$text), collapse = ' ')
    sdt <- gsub(' http|@)[^[:blank:]]+', '', sdt)
    sdt <- gsub('[[:punct:]]', '', sdt)
  })
  pt <- structure(pt, names = c('negative', 'neutral', 'positive'))
  pt['negative'] <- tm::removeWords(pt['negative'],
                                    names(table$negativeWords))
  pt['positive'] <- tm::removeWords(pt['positive'],
                                    names(table$positiveWords))
  pt
}
