# Let's R CMD check run smoothly
globalVariables(c("x", "isRetweet"))

#' Kernel density plots based on ggplot2 package.
#'
#' \code{plotDensity} Generates a density plot.
#'
#' @param data The datafrane to be plotted from
#' @param entry The character input value i.e. the search term
#' @param daily logical - TRUE if plot represents daily data
#' @import ggplot2
plotDensity <- function(data = x, entry = character(), daily = FALSE) {
  if (daily)
    title <- paste0("Distribution of tweets mentioning \"",
                    entry, "\" (Daily Results)")
  else title <- paste0("Distribution of tweets mentioning \"", entry, "\"")
  gg <- ggplot(data, aes(created)) +
    geom_density(aes(fill = isRetweet), alpha = .7) +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
    ggtitle(title) +
    xlab("Date")
  gg
}
