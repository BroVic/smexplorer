## ui.R

## User Interface for SMExplorer Shiny app
#' @import shiny
ui <- shinyUI(fluidPage(
  titlePanel(title = "SMExplorer",
             windowTitle = "Shiny app for social media metrics"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      tags$div(title = "Click here to submit authentication credentials
               and start a new session",
               strong(
                 actionLink(inputId = "oauth",
                            label = "Register a new session")
               )),

      hr(),


      tags$div(
        title = "Type in what you're searching for here",
        textInput("searchTerm", label = "Search", placeholder = " ")
      ),

      div(style = "background: dark-grey",
          actionButton("goButton", label = "Go!")),

      hr(),

      tags$div(
        title = "Choose the type of output you want to view",
        selectInput(
          "outputstyle",
          label = "Select output type",
          choices = c(
            "Density plot",
            "Platforms",
            "Emotions plot",
            "Wordcloud",
            "Network"
          )
        )
      ),

      conditionalPanel(
        condition = "input.outputstyle == 'Density plot'",

        radioButtons("densityPeriod", NULL, choices = c("Extended", "Daily")),

        conditionalPanel(
          condition = "input.densityPeriod == 'Extended'",
          dateRangeInput(
            "daterange",
            label = "Date Range",
            start = Sys.Date() - 7,
            end = Sys.Date(),
            max = Sys.Date(),
            format = "dd M yyyy",
            separator = "to"
          )
        ),

        conditionalPanel(
          condition = "input.densityPeriod == 'Daily'",
          dateInput(
            "oneday",
            label = "Date: ",
            value = NULL,
            max = as.POSIXct(Sys.Date()),
            format = "D dd M yyyy"
          )
        )
      ),

      conditionalPanel(condition = "input.outputstyle == 'Platforms'"),

      conditionalPanel(
        condition = "input.outputstyle == 'Emotions plot'",
        checkboxInput("emotiveExtremes",
                      label = "View emotive extremes",
                      value = FALSE)
      ),

      conditionalPanel(condition = "input.outputstyle == 'Wordcloud'"),

      conditionalPanel(condition = "input.outputstyle == 'Network'"),

      selectInput(
        "numLoaded",
        label = "Download limit",
        width = "72px",
        choices = c(50, 100, 200, 500, 1000, 2000, 5000)
      ),

      hr(),

      em(
        a(href = "https://github.com/NESREA/SMExplorer/issues/new",
          target = "_blank",
          "Feedback?")
      )
    ),

    mainPanel(
      div(title = "Plots will be displayed here.",
          plotOutput("twtDensity")),

      div(
        style = "display:inline-block; vertical-align:top;
        padding-top:20px; font-size: small;",
        textOutput("twtnum", inline = TRUE)
      ),


      div(tableOutput("mostEmotive")),

      width = 9

    )
    )
  ))
