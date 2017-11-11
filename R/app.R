#' SMExplorer Shiny application object

#' @return A Shiny application object
#' @import shiny
#' @importFrom network %v%
app <- shinyApp(
  ui = fluidPage(
    titlePanel(title = "SMExplorer",
               windowTitle = "Shiny app for social media metrics"),

    sidebarLayout(
      sidebarPanel(

        width = 3,

        tags$div(title = "Click here to submit authentication credentials
                 and start a new session",
                 strong(actionLink(inputId = "oauth",
                                   label = "Register a new session"))),

        hr(),


        tags$div(title = "Type in what you're searching for here",
                 textInput("searchTerm", label = "Search", placeholder = " ")
                 ),

        div(style = "background: dark-grey",
            actionButton("goButton", label = "Go!")),

        hr(),

        tags$div(title = "Choose the type of output you want to view",
                 selectInput("outputstyle",
                             label = "Select output type",
                             choices = c("Density plot",
                                         "Platforms",
                                         "Emotions plot",
                                         "Wordcloud",
                                         "Network"))
                 ),

                 conditionalPanel(
                   condition = "input.outputstyle == 'Density plot'",

                   radioButtons(
                     "densityPeriod", NULL, choices = c("Extended", "Daily")
                     ),

                   conditionalPanel(
                     condition = "input.densityPeriod == 'Extended'",
                     dateRangeInput("daterange",
                                    label = "Date Range",
                                    start = as.POSIXct(Sys.Date() - 8),
                                    end = as.POSIXct(Sys.Date() - 1),
                                    max = as.POSIXct(Sys.Date()),
                                    format = "dd M yyyy",
                                    separator = "to")),

                   conditionalPanel(
                     condition = "input.densityPeriod == 'Daily'",
                     dateInput("oneday",
                               label = "Date: ",
                               value = as.POSIXct(Sys.Date()) - 1,
                               max = as.POSIXct(Sys.Date()),
                               format = "D dd M yyyy"))
                   ),

        conditionalPanel(condition = "input.outputstyle == 'Platforms'"),

        conditionalPanel(condition = "input.outputstyle == 'Emotions plot'",
                         checkboxInput("emotiveExtremes",
                                       label = "View emotive extremes",
                                       value = FALSE)),

        conditionalPanel(condition = "input.outputstyle == 'Wordcloud'"),

        conditionalPanel(condition = "input.outputstyle == 'Network'"),

        selectInput("numLoaded",
                    label = "Download limit",
                    width = "72px",
                    choices = c(50, 100, 200, 500, 1000, 2000, 5000)),

        hr(),

        em(a(href = "https://github.com/NESREA/SMExplorer/issues/new",
             target = "_blank",
             "Report an issue"))
      ),

      mainPanel(
        div(title = "Plots will be displayed here.",
            plotOutput("twtDensity")),

        div(style = "display:inline-block; vertical-align:top;
            padding-top:20px; font-size: small;",
            textOutput("twtnum", inline = TRUE)),


        div(tableOutput("mostEmotive")),

        width = 9

      )
    )
  ),

  server = function(input, output) {

    dataInput <- reactive({
      if (input$oauth) {
        twitteR::setup_twitter_oauth(consumer_key,
                                     consumer_secret,
                                     access_token,
                                     access_secret)
      }
      input$goButton
      tweets <- isolate(
        twitteR::searchTwitter(as.character(input$searchTerm),
                               n = input$numLoaded,
                               since = as.character(input$daterange[1]),
                               until = as.character(input$daterange[2]))
      )
      df <- twitteR::twListToDF(tweets)
      df$text <- stringr::str_replace_all(df$text, "[^[:graph:]]", " ")
      df
    })


    output$twtDensity <- renderPlot({

      main_objects <- .prepareObjects(dataInput())
      orig <- main_objects$original
      pol <- main_objects$polarity
      RT <- main_objects$retweets
      polWordTable <- .createWordList(pol)

      ## Options for the various outputs
      if (input$outputstyle == "Density plot") {
        if (input$densityPeriod == "Extended") {
          checkPeriod <- dataInput()
          dW <- .plotDensity(data = checkPeriod,
                            entry = input$searchTerm,
                            daily = FALSE)
          print(dW)
        } else if (input$densityPeriod == "Daily") {
          tmp <- dataInput()
          index <- which(
            lubridate::mday(tmp$created) == lubridate::day(input$oneday)
            )
          checked_day <- tmp[index, ]
          densDay <- .plotDensity(checked_day,
                                 entry = input$searchTerm,
                                 daily = TRUE,
                                 input = input$oneday)
          print(densDay)
        }
      }
      else if (input$outputstyle == "Platforms") {
        temp_data <- dataInput()
        temp_data$statusSource <- substr(temp_data$statusSource,
                                         regexpr('>',
                                                 temp_data$statusSource) + 1,
                                         regexpr('</a>',
                                                 temp_data$statusSource) - 1)
        dotchart(sort(table(temp_data$statusSource)))
        mtext('Number of tweets posted by platform')
      }
      else if (input$outputstyle == "Emotions plot") {
        par(mfrow = c(1, 2))
        invisible(
          lapply(1:2, function(i) {
            dotchart(sort(polWordTable[[i]]), cex = .8)
            mtext(names(polWordTable)[i])
          }))
      }
      else if (input$outputstyle == "Wordcloud") {
        orig$emotionalValence <- sapply(pol, function(x) x$all$polarity)
        polSplit <- split(orig, sign(orig$emotionalValence))

        polText <- .processBagofWords(polSplit, polWordTable)

        corp <- .make_corpus(polText)
        col3 <- color()
        wordcloud::comparison.cloud(as.matrix(TermDocumentMatrix(corp)),
                                    max.words = 150,
                                    min.freq = 1,
                                    random.order = FALSE,
                                    rot.per = 0,
                                    colors = col3,
                                    vfont = c("sans serif", "plain"))
      }
      else if (input$outputstyle == "Network") {
        col3 <- color()
        RT$sender <- tolower(substr(RT$text, 5, regexpr(":", RT$text) - 1))
        edglst <- as.data.frame(table(RT$sender, tolower(RT$screenName)),
                                responseName = "n" )
        edglst <- edglst[edglst$n != 0, ]
        rtnet <- network::network(edglst,
                                  matrix.type = 'edgelist',
                                  directed = TRUE,
                                  ignore.eval = FALSE,
                                  names.eval = 'num')
        vlabs <- rtnet %v% "vertex.names"
        vlabs[sna::degree(rtnet, cmode = 'outdegree') == 0] <- NA
        plot(rtnet,
             label = vlabs,
             label.pos = 5,
             label.cex = .8,
             vertex.cex = log(sna::degree(rtnet)) + .5,
             vertex.col = col3[1],
             edge.lwd = 'num',
             edge.col = 'gray70',
             main = paste0("Retweet Network on the term '",
                           input$searchTerm, "'"))
      }
    })

    output$mostEmotive <- renderTable({
      if (input$emotiveExtremes && input$outputstyle == "Emotions plot")
      {
        main_objects <- .prepareObjects(dataInput())
        pol <- main_objects$polarity
        orig <- main_objects$original
        polWordTable <- .createWordList(pol)
        orig$emotionalValence <- sapply(pol, function(x) x$all$polarity)

        ## Render the table
        extremes <- data.frame(
          mostPositive = orig$text[which.max(orig$emotionalValence)],
          mostNegative = orig$text[which.min(orig$emotionalValence)]
        )
        print(extremes)
      }
    })

    output$twtnum <- renderText({
      note <- "tweets loaded."
      temp <- dataInput()
      paste(nrow(temp), note)

    })

  }

)

## Colours for plotting wordclouds

## These colours for different categories of tweets based on the 'sign'
##  of the emotional valence
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
