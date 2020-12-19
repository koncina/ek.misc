NULL

#' Top-expressor score
#'
#' (See the CAF signature repository for more details)
#'
#' @param z_scores vector of numericals containing the z-scores of gene signatures.
#' @param scale logical telling if the score should be returned as a raw value or scaled
#' @export
top_expressor_score <- function(z_scores, scale = FALSE) {

  stopifnot(length(scale) == 1 && is.logical(scale))
  score <- sum(sign(z_scores))

  if (isTRUE(scale)) score <- score / length(z_scores)
  score
}

#' Identify top-expressors based on the top-expressor score
#'
#' (See the CAF signature repository for more details)
#'
#' @param z_scores vector of numericals containing the z-scores of gene signatures.
#' @param threshold numerical telling the proportion of signature genes that should be highly expressed
#' @export
get_top_expressor <- function(z_scores,
                              threshold = 0.5) {
  stopifnot(length(threshold) == 1 && is.numeric(threshold))

  score <- top_expressor_score(z_scores, FALSE)
  threshold <- (2 * threshold - 1) * length(z_scores)
  score > threshold
}
