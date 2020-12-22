NULL

#' Cancer stage grouping
#'
#' Group the T, N, and M classifications as clinical stages. The used corresponance is provided on page 3 of the \emph{outcomes data form} report (\emph{final version 2.0, 14/06/2012}).
#'
#'
#' t, n and m stage grouping as described in the \emph{outcomes data form} report:
#' \tabular{rrrr}{
#'   stage \tab t \tab n \tab m \cr
#'   I \tab T1, T2 \tab N0 \tab M0\cr
#'   II \tab T3, T4 \tab N0 \tab M0\cr
#'   II \tab A T3 \tab N0 \tab M0\cr
#'   II \tab B T4a \tab N0 \tab M0\cr
#'   II \tab C T4b \tab N0 \tab M0\cr
#'   III \tab Any T \tab N1, N2 \tab M0\cr
#'   III \tab A T1, T2 \tab N1 \tab M0\cr
#'   III \tab A T1 \tab N2a \tab M0\cr
#'   III \tab B T3, T4a \tab N1 \tab M0\cr
#'   III \tab B T2, T3 \tab N2a \tab M0\cr
#'   III \tab B T1, T2 \tab N2b \tab M0\cr
#'   III \tab C T4a \tab N2a \tab M0\cr
#'   III \tab C T3, T4a \tab N2b \tab M0\cr
#'   III \tab C T4b \tab N1, N2 \tab M0\cr
#'   IV A \tab Any T \tab Any N \tab M1a\cr
#'   IV B \tab Any T \tab Any N \tab M1b
#' }
#'
#' @examples
#'
#' tnm_to_stage("t1", "n0", "m0")
#' tnm_to_stage("t1", "n0", "mx")
#' tnm_to_stage("t1", "n0", "mx", strict = FALSE)
#'
#' @param t,n,m TNM (tumor - node - metastasis) staging system
#' @param strict If FALSE returns also approximate staging for missing n or m values.
#' nx or mx will be considered as not detected (n0 and m0) and the stage will be at least the returned value.
#' @param .prefix,.suffix The prefix and/or suffix used if strict is FALSE and n or m are not assessable (nx or mx). Defaults to \code{.suffix="+"} (stage is at least the calculated value)
#'
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect str_extract
#'
#' @export
tnm_to_stage <- function(t, n, m, substage = TRUE, na_as_x = FALSE, mx_as_m0 = FALSE) {
  #, .prefix = "", .suffix = "+"

  t <- tolower(t)
  n <- tolower(n)
  m <- tolower(m)
  na_as_x <- isTRUE(na_as_x)
  mx_as_m0 <- isTRUE(mx_as_m0)
  substage <- isTRUE(substage)
  if (na_as_x) {
    t[is.na(t)] <- "tx"
    n[is.na(n)] <- "nx"
    m[is.na(m)] <- "mx"
  }

  #.prefix <- ifelse((n == "nx" | m == "mx") & !strict, .prefix[1], "")
  #.suffix <- ifelse((n == "nx" | m == "mx") & !strict, .suffix[1], "")

  if (mx_as_m0) {
    m <- gsub("mx", "m0", m)
  }

  stage <- case_when(
    n == "n0" & m == "m0" ~ case_when(
      t == "t0" ~ NA_character_,
      t == "tis" ~ "0",
      t < "t3"   ~ "1",
      t == "t3"  ~ "2a",
      t == "t4a" ~ "2b",
      t == "t4b" ~ "2c",
      t == "t4"  ~ "2"
    ),
    m == "m0" ~ case_when(
      (t %in% c("t1", "t2") & n == "n1") | (t == "t1" & n == "n2a")                                                    ~ "3a",
      (t %in% c("t3", "t4a") & n == "n1") | (t %in% c("t2", "t3") & n == "n2a") | (t %in% c("t1", "t2") & n == "n2b")  ~ "3b",
      (t %in% c("t3", "t4a") & n == "n2b") | (t == "t4a" & n == "n2a") | (t == "t4b" & n %in% c("n1", "n2"))           ~ "3c",
      str_detect(n, "^n[+12][ab]?")                                                                                    ~ "3"
    ),
    m == "m1a" ~ "4a",
    m == "m1b" ~ "4b",
    m == "m1"  ~ "4"
  )

  #out <- paste0(.prefix, stage, .suffix)
  stage[is.na(stage)] <- NA_character_
  if (!substage) stage <- str_extract(stage, "^\\d")
  stage
}

