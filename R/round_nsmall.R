NULL

#' @export
round_nsmall <- function(x, digits = 2) {
  format(round(x, digits), nsmall = digits)
}

