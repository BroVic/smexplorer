library(tm)
context("Internal functions")

# Simulated data
data("crude")     # sample data from 'tm' package
df <- as.data.frame(matrix(1:320, ncol = 16))
colnames(df) <- c("text",
                  "favorited",
                  "favoriteCount",
                  "replyToSN",
                  "created",
                  "truncated",
                  "replyToSID",
                  "id",
                  "replyToUID",
                  "statusSource",
                  "screenName",
                  "retweetCount",
                  "isRetweet",
                  "retweeted",
                  "longitude",
                  "latitude")

for (i in 1:20) {
  df$text[i] <- substr(crude[[i]]$content, 1, 140)
}

logi <- c(T, F)
df['isRetweet'] <- rep(logi, nrow(df)/length(logi))
retVal <- suppressWarnings(.prepareObjects(df))

og <- retVal$original
og$ev <- sapply(retVal$polarity, function(x) x$all$polarity)
dt <- split(og, sign(og$ev))
pt <- .createWordList(retVal$polarity)
bow <- .processBagofWords(dt, pt)

# Tests
test_that(desc = "Colours for the wordcloud are correct", {
  expect_equal(.color()[1], "#A6CEE3")
  expect_equal(.color()[2], "#1F78B4")
  expect_equal(.color()[3], "#B2DF8A")
})

test_that(desc = "Main objects are of the correct type and class", {
  expect_equal(typeof(retVal), "list")
  expect_equal(length(retVal), 3)
  expect_equal(class(retVal[1]), "list")
  expect_equal(class(retVal[2]), "list")
  expect_equal(class(retVal[3]), "list")
  expect_equal(class(retVal[[1]]), "data.frame")
  expect_equal(class(retVal[[2]]), "data.frame")
  expect_equal(class(retVal[[3]]), "list")
})

test_that("Word polarities are properly tabulated", {
  expect_equal(typeof(pt), "list")
  expect_equal(class(pt), "list")
  expect_equal(length(pt), 2)
  expect_equal(typeof(pt[[1]]), "integer")
  expect_equal(names(pt[1]), "positiveWords")
  expect_equal(names(pt[2]), "negativeWords")
})

test_that("Density plot is properly rendered", {
  expect_equal(class(.plotDensity(df))[1], "gg")
  expect_equal(class(.plotDensity(df))[2], "ggplot")
  expect_equal(length(.plotDensity(df)), 9)
})

test_that("Bag-of-words is built", {
  expect_equal(class(bow), "character")
  expect_equal(length(bow), 3)
})

rm(df, logi, retVal, og, dt, pt, bow)
