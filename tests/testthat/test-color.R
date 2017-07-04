context("Colours used in the wordcloud")

test_that(desc = "Colours for the wordcloud are correct", {
  expect_equal(color()[1], "#A6CEE3")
  expect_equal(color()[2], "#1F78B4")
  expect_equal(color()[3], "#B2DF8A")
})
