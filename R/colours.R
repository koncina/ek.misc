NULL

#' The colours associated to the the consensus molecular subtypes (CMS) as used in the original publication.
#'
#' @export
colours_cms <- function() {
  c(CMS1 = "#E89B36", CMS2 = "#0473AB", CMS3 = "#CF78A3", CMS4 = "#0D9D76", NOLBL = "black")
}

#' Blue, white and red colours used to plot heatmaps.
#'
#' @export
colours_bwr <- function() {
  c(blue = "#377EB8", "white", red = "#E41A1C")
}

#' Colour code used for the INTER grant application to plot the expression of genes of interest in epithelial vs CAF cells.
#'
#' @export
colours_caf <- function(epithelium = "epithelium",
                        nf = "NF", caf = "CAF",
                        mcaf = "mCAF",
                        cycaf1 = "cyCAF-1", cycaf2 = "cyCAF-2") {
  set_names(c("#dcd6ca", "#4CAEF3", "#0A2083",
              "#fc8d62", "#66c2a5", "#8da0cb"),
            c(epithelium, nf, caf,
              mcaf, cycaf1, cycaf2))
}

