NULL

#' @importFrom fs path_split
#' @importFrom purrr flatten_chr
#' @importFrom stringr str_extract
#'
#' @export
created <- function(rmd = NULL) {
  if (is.null(rmd)) rmd <- current_input(dir = TRUE)
  creation_date <- path_split(rmd)
  creation_date <- flatten_chr(creation_date)
  creation_date <- creation_date[length(creation_date) - 1]
  str_extract(creation_date, "^\\d{4}-\\d{2}-\\d{2}")
  #as.Date(creation_date)
}

#' @importFrom fs file_info
#' @importFrom knitr current_input
#'
#' @export
last_change <- function(rmd = NULL) {
  if (is.null(rmd)) rmd <- current_input(dir = TRUE)
  x <- as.Date(file_info(rmd)[['change_time']], format = "%m/%d/%Y")
  message(x)
  x
  #possibly(parse_datetime, otherwise = c(change_time = NA_character_))(possibly(file_info, otherwise = c(change_time = NA_character_))()[['change_time']])
}

#' Cluster a tibble
#'
#' Coerce the specified variables to a matrix, perform the clustering and return the tibble rearranged
#' according to the clustered ordering.
#'
#' @importFrom readr parse_date parse_datetime
#' @importFrom fs file_info
#' @importFrom knitr current_input
#' @importFrom purrr possibly
#'
#' @param created NULL or character which will be parsed as the creation date
#'
#' @export
last_updated <- function(created = NULL) {
  if (is.null(created)) created <- created()
  created <- parse_date(created)

  dates <- c(created = created,
             last_changed = last_change(),
             created = Sys.Date()
  )

  dates <-  format(dates, "%d/%m/%Y")
  dates <- paste0(c("", "(last change: ", "generated: "), dates, c("", "", ")"))
  paste(dates, collapse = ", ")
}

#' Cluster a tibble
#'
#' Coerce the specified variables to a matrix, perform the clustering and return the tibble rearranged
#' according to the clustered ordering.
#'
#' @importFrom readr parse_date parse_datetime
#' @importFrom fs file_info
#' @importFrom knitr current_input
#' @importFrom purrr possibly
#'
#' @param created NULL or character which will be parsed as the creation date
#'
#' @export
last_updated_latex <- function(created = NULL) {
  if (is.null(created)) created <- created()
  created <- parse_date(created)

  dates <- c(created = created,
             last_changed = last_change(),
             created = Sys.Date()
  )

  dates <-  format(dates, "%d/%m/%y")
  glue::glue("{dates[[1]]} \\small (last change on {dates[[2]]}, compiled on {dates[[3]]})")
}