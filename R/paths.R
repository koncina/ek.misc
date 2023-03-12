
#' @importFrom fs dir_create path_home
#'
#' @export
path_report <- function(filename) {

  f <- file.path(path_home("Documents"),
                 "reports",
                 file.path(get_rel_path(remove = 0), filename))

  dir_create(dirname(f))
  f
}

#' @importFrom fs dir_create
#' @importFrom rappdirs user_data_dir
#'
#' @export
path_data <- function(filename) {
  f <- file.path(user_data_dir("data"),
                 get_rel_path(remove = 0), filename)

  dir_create(dirname(f))
  f
}

#' @importFrom here here
#' @importFrom fs path_join
#'
get_rel_path <- function(remove = 1) {
  h <- here()
  h <- path_split(h)[[1]]
  h <- h[(which(h == "projects") + 1):(length(h) - remove)]
  path_join(h)
}

#' @importFrom purrr accumulate
#' @importFrom fs path_split path_rel file_exists
#'
#' @export
rel_project <- function(marker_file = ".projects", from = TRUE) {
  x <- accumulate(path_split(getwd())[[1]], file.path)

  if (isTRUE(from)) {
    path_rel(x[length(x)],
             x[max(which(file_exists(file.path(x, ".projects"))))])
  } else {
    x[max(which(file_exists(file.path(x, ".projects"))))]
  }
}

#' #' @importFrom rappdirs user_data_dir
#' #'
#' path_shared <- function(share_folder, filename) {
#'   f <- file.path(user_data_dir(share_folder), filename)
#'   dir_create(dirname(f))
#'   f
#' }
