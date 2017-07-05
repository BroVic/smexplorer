library(shiny)

context("Shiny app object")

test_that("Object is a Shiny app object", {
          expect_identical(
            class(app),
            class(shinyApp(ui = NULL, server = function(input, output){})))

          expect_identical(
            class(app),
            "shiny.appobj")
          })
