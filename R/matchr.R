#' Sequential and parallel matching
#'
#' Facilitates matching in very large populations, where we either want to
#' find matches in parallel to speed up the process, or find matches
#' sequentially for subsets of the cases, to handle RAM limitations.
#'
#' The function is a wrapper around `match_controls` using the future.apply
#' package to either do the matching sequentially or in parallel.
#'
#' @inheritParams match_controls
#' @param parallel Logical vector of length one. Determines if matching is done
#'   in parallel or sequentially, splitting the case data into `split_n` chunks.
#'   By default the matching is done sequentially.
#' @param split_n Integer vector of length 1. The number of chunks the case data
#'   is split into.
#' @return data.table
#' @examples abs
#' @export
#' @import data.table
matchr <- function(dat_case,
                   dat_ctrl,
                   match_exact   = NULL,
                   match_inexact = NULL,
                   n_controls    = 1L,
                   parallel      = FALSE,
                   split_n       = 1L,
                   seed          = NULL) {

  # Quiet R CMD check notes of type "no visible binding for global variable..."
  .id <- .match_id <- .split <- .case <- NULL

  if (!(class(parallel) == "logical" & length(parallel) == 1)) {
    stop("parallel must be a logical vector or length 1", call. = FALSE)
  }

  if (!is.numeric(split_n)) {
    stop("split_n must be numeric", call. = FALSE)
  }

  if (!length(split_n) == 1) {
    stop("split_n must have length 1", call. = FALSE)
  }

  if (!split_n == abs(round(split_n))) {
    stop("split_n must be an integer", call. = FALSE)
  }

  if (nrow(dat_case) < split_n) {
    stop(
      "split_n can not be higher than nrow(dat_case), the number of cases"
      , call. = FALSE
    )
  }

  dat_case1 <- data.table::as.data.table(dat_case)

  if (parallel == TRUE) {
    future::plan("multisession", workers = split_n)
  } else if (parallel == FALSE) {
    future::plan("sequential")
  }

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
  dat1 <- future.apply::future_lapply(
    X = dat_case1,
    FUN = match_controls,
    dat_ctrl = dat_ctrl,
    match_exact = match_exact,
    match_inexact = match_inexact,
    n_controls = n_controls,
    seed = seed,
    future.seed = TRUE
  )

  dat1 <- data.table::rbindlist(dat1, idcol = TRUE)
  dat1[, .match_id := .GRP, by =  c(".id", ".match_id")][, .id:= NULL][]
}
