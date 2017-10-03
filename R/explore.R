#' Explore Social Media Data.
#'
#' \code{explore} Launches a new Shiny application.
#'
#' This is a wrapper function for shiny::runApp, which is the core
#' function used to launch Shiny applications.
#'
#' @return This function does not return. See \code{\link{runApp}}
#' @export
explore <- function()
{
  shiny::runApp(app,
                launch.browser = TRUE,
                display.mode = "normal",
                quiet = TRUE)
}

