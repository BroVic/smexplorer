# Compilation of Lists on either Side of Emotive Spectrum

createWordList <- function(x) {
  pwt <- sapply(x, function(p) {
    words <- c(positiveWords = paste(p[[1]]$pos.words[[1]], collapse = ' '),
              negativeWords = paste(p[[1]]$neg.words[[1]], collapse = ' '))
    words <- gsub('-', '', words)
  })
  pwt <- apply(pwt, 1, function(x) paste(x, collapse = ' '))
  pwt <- tm::stripWhitespace(pwt)
  pwt <- strsplit(pwt, ' ')
  pwt <- sapply(pwt, table)
}
