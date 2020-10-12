NULL

#' Parse roman numerals
#'
#' @importFrom utils as.roman
#' @param x A character vector of arabic or roman numerals.
#'
#' @export
parse_roman <- function(x) {
  as.numeric(as.roman(x))
}
