context("App launcher")

test_that("Caller provides correct input", {
  err <- "unused argument"
  expect_error(explore(1), err)
  expect_error(explore("a"), err)
  expect_error(explore(T), err)
  expect_error(explore(NA), err)
  expect_error(explore(NULL), err)
})
