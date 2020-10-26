NULL

#' Kolmogorov-Smirnov test score
#'
#' Resume a multiple gene expression difference as a Kolmogorov-Smirnov Test based score
#'
#' @importFrom stats ks.test
#' @param x,y vectors of numericals containing the gene expressions to compare.
#'
#' @export
ks_score <- function(x, y, na.rm = FALSE) {
  if (na.rm == TRUE) {
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
  }
  x_score <- ks.test(x, y, alternative = "greater")[["statistic"]]
  y_score <- ks.test(x, y, alternative = "less")[["statistic"]]
  as.numeric(x_score - y_score)
}
