#' SMExplorer Shiny application object
#'
#' @importFrom shiny shinyApp
#' @importFrom shiny fluidPage
#' @importFrom shiny numericInput
#' @importFrom shiny plotOutput
#' @importFrom shiny renderPlot
app <- shiny::shinyApp(
  ui = shiny::fluidPage(
    shiny::numericInput("n", "n", 1),
    shiny::plotOutput("plot")
  ),
  server = function(input, output) {
    output$plot <- shiny::renderPlot(plot(head(cars,  input$n)))
  }
)
