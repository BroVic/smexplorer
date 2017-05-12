#' Colours for plotting wordclouds
#' 
#' \code{color} Colours for different categories of tweets based on the sign
#'     of the emotional valence
color <- function() {
  col <- RColorBrewer::brewer.pal(3, 'Paired')
  col
}