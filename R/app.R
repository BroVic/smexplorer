#' SMExplorer Shiny application object
#'
#' @return A Shiny application object
#' @import lubridate
#' @import shiny
#' @importFrom network %v%
app <- shinyApp(
  ui = fluidPage(
    theme = shinythemes::shinytheme("superhero"),

    titlePanel(title = "SMExplorer",
               windowTitle = "Shiny app for social media metrics"),

    sidebarLayout(
      sidebarPanel(

        width = 3,

        tags$div(title = "Click here to submit authentication credentials
                 and start a new session",
                 strong(actionLink(inputId = "oauth",
                                   label = "Register new session"))),

        hr(),


        tags$div(title = "Type in what you're searching for here",
                 textInput("searchTerm", label = "Search", placeholder = " ")
                 ),

        tags$div(title = "Choose the type of output you want to view",
                 selectInput("outputstyle",
                             label = "Select output type",
                             choices = c("Density plot (week)",
                                         "Density plot (day)",
                                         "Platforms",
                                         "Emotions plot",
                                         "Wordcloud",
                                         "Network"))
                 ),

        tags$div(title = "Request data for a given time period",
                 conditionalPanel(
                   condition = "input.outputstyle == 'Density plot (week)'",
                   dateRangeInput("daterange",
                                  label = "Date Range: ",
                                  start = Sys.Date() - 8,
                                  end = Sys.Date() - 1,
                                  max = Sys.Date(),
                                  format = "d M",
                                  separator = "--")
                   )
                 ),

        tags$div(title = "Request data for a particular day",
                 conditionalPanel(
                   condition = "input.outputstyle == 'Density plot (day)'",
                   dateInput("singledate",
                             label = "Date: ",
                             value = Sys.Date() - 1,
                             max = Sys.Date(),
                             format = "D dd M yyyy")
                   )
                 ),

        conditionalPanel(
          condition = "input.outputstyle == 'Platforms'"
        ),

        conditionalPanel(
          condition = "input.outputstyle == 'Emotions plot'",
          checkboxInput("emotiveExtremes",
                        label = "View emotive extremes",
                        value = FALSE)
        ),

        conditionalPanel(
          condition = "input.outputstyle == 'Wordcloud'"
        ),

        conditionalPanel(
          condition = "input.outputstyle == 'Network'"
        ),

        div(style = "border: 1px dotted black; background: dark-grey;
            width: 52px",
            actionButton("goButton", label = "Go!")),

        hr(),

        em(a(href = "https://github.com/NESREA/SMExplorer/issues/new",
             "Report an issue"))
      ),

      mainPanel(
        div(title = "Plots will be displayed here.",
            plotOutput("twtDensity")),

        div(style = "display:inline-block; vertical-align:top;
            padding-top:20px; font-size: small;",
            textOutput("twtnum", inline = TRUE)),

        div(
          style = "display:inline-block; vertical-align:top;",
          selectInput(
            "numLoaded",
            label = "",
            width = "70px",
            choices = c(50, 100, 200, 500, 1000, 2000, 5000))),

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

      main_objects <- prepareObjects(dataInput())
      orig <- main_objects$original
      pol <- main_objects$polarity
      RT <- main_objects$retweets
      polWordTable <- createWordList(pol)

      # options for the various plots
      if (input$outputstyle == "Density plot (week)") {
        checkPeriod <- dataInput()
        dW <- plotDensity(data = checkPeriod,
                          entry = input$searchTerm,
                          daily = FALSE)
        dW
      }
      else if (input$outputstyle == "Density plot (day)") {
        checkday <- dplyr::filter(dataInput(),
                                  mday(created) == day(input$singledate))
        densDay <- plotDensity(checkday,
                               entry = input$searchTerm,
                               daily = TRUE,
                               input = input$singledate)
        densDay
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

        polText <- processBagofWords(polSplit, polWordTable)

        corp <- make_corpus(polText)
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
        RT <- dplyr::mutate(RT,
                            sender = substr(text, 5, regexpr(':', text) - 1))
        edglst <- as.data.frame(cbind(sender = tolower(RT$sender),
                                      receiver = tolower(RT$screenName)))
        edglst <- dplyr::count(edglst, sender, receiver)
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
        main_objects <- prepareObjects(dataInput())
        pol <- main_objects$polarity
        orig <- main_objects$original
        polWordTable <- createWordList(pol)
        orig$emotionalValence <- sapply(pol, function(x) x$all$polarity)

        # Render the table
        extremes <- data.frame(
          mostPositive = orig$text[which.max(orig$emotionalValence)],
          mostNegative = orig$text[which.min(orig$emotionalValence)]
        )
        print(extremes)
      }
    })

    output$twtnum <- renderText({
      note <- "tweets loaded. Select a value to extend the download limit: "
      temp <- dataInput()
      paste(nrow(temp), note)

    })

  }

)
