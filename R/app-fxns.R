## app-fxns.R
## Suite of internal functions that are used within the Shiny app object

## Colours for plotting wordclouds
## These colours for different categories of tweets based on the 'sign'
## of the emotional valence
.color <- function() {
  col <- RColorBrewer::brewer.pal(3, 'Paired')
  col
}

## Compilation of lists on either side of emotive spectrum
.createWordList <- function(x) {
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

## Prepare text for Use in sentiment analysis
#' @import tm
.make_corpus <- function(GText, stem = TRUE) {
  corp <- VCorpus(VectorSource(GText))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removeWords, stopwords("english"))
  if(stem)
    corp <- tm_map(corp, stemDocument)
  names(corp) <- names(GText)
  corp
}

## Let's R CMD check run smoothly
globalVariables(c("x", "isRetweet", "created"))

## Kernel density plots based on ggplot2 package.
##
## data: The datafrane to be plotted from
## entry: The character input value i.e. the search term
## daily: logical - TRUE if plot represents daily data
## input: A character vector of date input for daily data
#' @import ggplot2
.plotDensity <- function(data = x, entry = character(),
                         daily = FALSE, input = NULL)
{
  if (daily) {
    title <- paste0("Distribution of tweets mentioning \"",
                    entry, "\" (Daily Results)")
    xlabel <- paste("Time on", format(as.POSIXct(input), "%a %d %b %Y"))
  }
  else {
    title <- paste0("Distribution of tweets mentioning \"", entry, "\"")
    xlabel <- "Date"
  }
  gg <- ggplot(data, aes(created)) +
    geom_density(aes(fill = isRetweet), alpha = .7) +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
    ggtitle(title) +
    xlab(xlabel)
  gg
}

## Puts together intermediary data structures that are going to be
## used for the sentiment analysis.
## The function returns a list 'value' conataining 3 data structures:
## 1. main   - A data frame of original tweets
## 2. retwts - A data frame of retweets
## 3. pol    - A list of polarities for the text of original tweets
.prepareObjects <- function(data) {
  spl <- split(data, data$isRetweet)
  main <- spl[['FALSE']]
  retwts <- spl[['TRUE']]
  pol <- lapply(main$text, function(x) {
    txt <- gsub("(\\.|!|\\?)+\\s+|(\\++)", " ", x)
    txt <- gsub(" http[^[:blank:]]+", "", txt)
    p_list <- qdap::polarity(txt)
  })
  value <- list(original = main, retweets = retwts, polarity = pol)
  value
}

## Pre-treatment for text ahead of its use in plots.
.processBagofWords <- function(x, table) {
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
