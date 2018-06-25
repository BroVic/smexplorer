## server.R

#' @import shiny
#' @importFrom graphics dotchart
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics plot
#' @importFrom lubridate day
#' @importFrom lubridate mday
#' @importFrom network %v%
#' @importFrom network network
#' @importFrom sna degree
#' @importFrom stringr str_replace_all
#' @importFrom tm TermDocumentMatrix
#' @importFrom twitteR searchTwitter
#' @importFrom twitteR setup_twitter_oauth
#' @importFrom twitteR twListToDF
#' @importFrom wordcloud comparison.cloud
server <- function(input, output, session) {

  observe({
    setup_twitter_oauth(consumer_key,
                        consumer_secret,
                        access_token,
                        access_secret)
  })

  dataInput <- reactive({

    input$goButton
    num <- input$numLoaded

    tweets <- isolate(
      searchTwitter(
        as.character(input$searchTerm),
        n = num,
        since = as.character(input$daterange[1]),
        until = as.character(input$daterange[2])
      )
    )
    df <- twListToDF(tweets)
    df$text <-
      str_replace_all(df$text, "[^[:graph:]]", " ")
    df
  })

  ## We want to make sure that the date input
  ## widgets are displaying current dates
  observe({
    updateDateRangeInput(
      session,
      "daterange",
      label = "Date Range",
      start = Sys.Date() - 7,
      end = Sys.Date(),
      max = Sys.Date()
    )

    updateDateInput(
      session,
      "oneday",
      label = "Date: ",
      value = NULL,
      max = as.POSIXct(Sys.Date())
    )
  })

  output$twtDensity <- renderPlot({
    try({
      main_objects <- .prepareObjects(dataInput())
      orig <- main_objects$original
      pol <- main_objects$polarity
      RT <- main_objects$retweets
      polWordTable <- .createWordList(pol)
    })
    ## Options for the various outputs
    if (input$outputstyle == "Density plot") {
      if (input$densityPeriod == "Extended") {
        checkPeriod <- dataInput()
        try({
          dW <- .plotDensity(data = checkPeriod,
                             entry = input$searchTerm,
                             daily = FALSE)
          print(dW)
        })
      }
      else if (input$densityPeriod == "Daily") {
        tmp <- dataInput()
        index <-
          which(mday(tmp$created) == day(input$oneday))
        checked_day <- tmp[index, ]
        try({
          densDay <- .plotDensity(
            checked_day,
            entry = input$searchTerm,
            daily = TRUE,
            input = input$oneday
          )
          print(densDay)
        })
      }
    }
    else if (input$outputstyle == "Platforms") {
      temp_data <- dataInput()
      temp_data$statusSource <- substr(
        temp_data$statusSource,
        regexpr('>',
                temp_data$statusSource) + 1,
        regexpr('</a>',
                temp_data$statusSource) - 1
      )
      try({
        dotchart(sort(table(temp_data$statusSource)))
        mtext('Number of tweets posted by platform')
      })
    }
    else if (input$outputstyle == "Emotions plot") {
      try({
        par(mfrow = c(1, 2))
        invisible(lapply(1:2, function(i) {
          dotchart(sort(polWordTable[[i]]), cex = .8)
          mtext(names(polWordTable)[i])
        }))
      })
    }
    else if (input$outputstyle == "Wordcloud") {
      try({
        orig$emotionalValence <- sapply(pol, function(x) {
          x$all$polarity
        })
        polSplit <- split(orig, sign(orig$emotionalValence))
        polText <- .processBagofWords(polSplit, polWordTable)
        corp <- .make_corpus(polText)
        col3 <- .color()
        comparison.cloud(
          as.matrix(TermDocumentMatrix(corp)),
          max.words = 150,
          min.freq = 1,
          random.order = FALSE,
          rot.per = 0,
          colors = col3,
          vfont = c("sans serif", "plain")
        )
      })
    }
    else if (input$outputstyle == "Network") {
      col3 <- .color()
      RT$sender <-
        tolower(substr(RT$text, 5, regexpr(":", RT$text) - 1))
      edglst <-
        as.data.frame(table(RT$sender, tolower(RT$screenName)),
                      responseName = "n")
      edglst <- edglst[edglst$n != 0, ]
      try({
        rtnet <- network(
          edglst,
          matrix.type = 'edgelist',
          directed = TRUE,
          ignore.eval = FALSE,
          names.eval = 'num'
        )
        vlabs <- rtnet %v% "vertex.names"
        vlabs[degree(rtnet, cmode = 'outdegree') == 0] <- NA
        plot(
          rtnet,
          label = vlabs,
          label.pos = 5,
          label.cex = .8,
          vertex.cex = log(degree(rtnet)) + .5,
          vertex.col = col3[1],
          edge.lwd = 'num',
          edge.col = 'gray70',
          main = paste0("Retweet Network on the term '",
                        input$searchTerm, "'")
        )
      })
    }
  })

  output$mostEmotive <- renderTable({
    if (input$emotiveExtremes && input$outputstyle == "Emotions plot")
    {
      try({
        main_objects <- .prepareObjects(dataInput())
        pol <- main_objects$polarity
        orig <- main_objects$original
        polWordTable <- .createWordList(pol)
        orig$emotionalValence <-
          sapply(pol, function(x)
            x$all$polarity)

        ## Render the table
        good <- orig$text[which.max(orig$emotionalValence)]
        bad <- orig$text[which.min(orig$emotionalValence)]
        extremes <-
          data.frame(mostPositive = good,
                     mostNegative = bad)
        print(extremes)
      })
    }
  })

  output$twtnum <- renderText({
    note <- "tweets loaded."
    temp <- dataInput()
    paste(nrow(temp), note)
  })
}
