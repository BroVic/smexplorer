#' Explore Social Media Data.
#'
#' \code{explore} Launches a new Shiny application.
#'
#' This is a wrapper function for \code{shiny::runApp}, which is a core
#' function for launching Shiny applications.
#'
#' @return This function does not return. See \code{\link{runApp}}
#'
#' @importFrom shiny runApp
#' @importFrom shiny shinyApp
#'
#' @export
explore <- function()
{
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

