#' Sequential/parallel matching
#'
#' @param dat_case data.table
#' @param dat_ctrl data.table
#' @param match_exact character vector
#' @param match_inexact character string
#' @param n_controls integer
#' @param split_n integer
#' @param sequential logical
#' @param seed integer
#'
#' @return data.table
#' @export
#' @import data.table
#'
#' @examples abs
matchr <- function(dat_case,
                   dat_ctrl,
                   match_exact   = NULL,
                   match_inexact = NULL,
                   n_controls    = 1L,
                   split_n       = 1L,
                   sequential    = TRUE,
                   seed          = NULL) {

  # Quiet R CMD check notes of the type "no visible binding for global variable..."
  .id <- .match_id <- .split <- .case <- NULL

  # Coerce object to data.table
  dat_case1 <- data.table::as.data.table(dat_case)[, .case := 1]
  dat_ctrl1 <- data.table::as.data.table(dat_ctrl)[, .case := 0]

  # Split cases into groups for sequential/parallel matching in chunks
  dat_case1[, .split := as.character(
    rep(
      1:split_n,
      each = nrow(dat_case1)/split_n,
      length.out = nrow(dat_case1)
    )
  )]
  dat_case1 <- split(dat_case1, by = ".split", keep.by = FALSE)

  # Match controls to cases in each case-chunk
  if (sequential == TRUE) {
    future::plan("sequential")
  } else if (sequential == FALSE) {
    future::plan("multisession", workers = split_n)
  }

  dat1 <- future.apply::future_lapply(
    X = dat_case1,
    FUN = match_controls,
    dat_ctrl = dat_ctrl1,
    match_exact = match_exact,
    match_inexact = match_inexact,
    n_controls = n_controls,
    future.seed = TRUE
  )

  dat1 <- data.table::rbindlist(dat1, idcol = TRUE)
  dat1[, .match_id := .GRP, by =  c(".id", ".match_id")][, .id:= NULL][]
}

