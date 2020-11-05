#' Exact and inexact matching
#'
#' Find matches for cases from a pool of potential controls using exact and/or
#' inexact matching criterias.
#'
#' The function works by creating a cross join of cases and a combination of
#' controls and cases (on any specified exact matching variables), from which
#' random controls are chosen. Further inexact matching criterias can then be
#' applied, eg. "var1 < i.var & abs(var2 - i.var2)" where the "i." prefix on
#' variables denote case values.
#'
#' @param dat_case A data.frame. Data on cases.
#' @param dat_ctrl A data.frame. Data on potential controls. Column names,
#'   order and types must be the same as in `dat_case`.
#' @param match_exact Character vector. Variables on which to do exact matching.
#' @param match_inexact String with inexact matching criterias. See details and
#'   examples.
#' @param n_controls Numeric vector of length 1. Number of controls to match to
#'   each case.
#' @param seed Numeric vector of length 1 that can be interpreted as an integer.
#' @return data.table
#' @examples
#' n <- 100
#' set.seed(1)
#' dat <- data.frame(
#'   id = 1:100,
#'   case = c(rep(1, n/10), rep(0, (n * 9 / 10))),
#'   bin_var = rbinom(n, 1, 0.5),
#'   cont_var = rnorm(n, 0, 1)
#' )
#' matches <- match_controls(
#'   dat[dat$case == 1, ],
#'   dat[dat$case == 0, ],
#'   match_exact = "bin_var",
#'   match_inexact = "!i.id == id & abs(cont_var - i.cont_var) < 0.1",
#'   seed = 2
#' )
#' @export
#' @import data.table
match_controls <- function(dat_case,
                           dat_ctrl,
                           match_exact = NULL,
                           match_inexact = NULL,
                           n_controls = 1L,
                           seed = NULL) {

# Quiet R CMD check notes of type "no visible binding for global variable...".
  .id <- .on <- .rand <- .match_id <- .case <- ..n_controls <- NULL

  set.seed(seed)


  if (!is.data.frame(dat_case)) {
    stop("dat_case must have class data.frame", call. = FALSE)
  }

  if (!is.data.frame(dat_ctrl)) {
    stop("dat_ctrl must have class data.frame", call. = FALSE)
  }

  if (!identical(names(dat_case), names(dat_ctrl))) {
    stop(
      "dat_case and dat_ctrl must have the same column names and column order",
      call. = FALSE
    )
  }

  if (!identical(sapply(dat_case, class), sapply(dat_ctrl, class))) {
    stop("dat_case and dat_ctrl must have the same column types", call. = FALSE)
  }

  if (!(is.null(match_exact) | is.character(match_exact))) {
    stop("match_exact must be a character vector", call. = FALSE)
  }

  if (!all(match_exact %in% names(dat_case))) {
    stop(
      "One or more variable in match_exact is not a column in dat_case",
      call. = FALSE
    )
  }

  if (!(is.null(match_inexact) | is.character(match_inexact))) {
    stop("match_inexact must be a character string", call. = FALSE)
  }

  if (!(is.null(match_inexact) | length(match_inexact) == 1)) {
    stop("match_inexact must be of length 1", call. = FALSE)
  }

  if (!is.numeric(n_controls)) {
    stop("n_control must be numeric", call. = FALSE)
  }

  if (!length(n_controls) == 1) {
    stop("n_control must be of length 1", call. = FALSE)
  }

  if (!n_controls == abs(round(n_controls))) {
    stop("n_control must be an integer", call. = FALSE)
  }

  # Coerce and row bind data.
  dat1 <- data.table::rbindlist(
    list(
      data.table::as.data.table(dat_case)[, .case := 1],
      data.table::as.data.table(dat_ctrl)[, .case := 0]
    ),
    use.names = TRUE
    )[, .id := .I]

  if (is.null(match_exact)) {
    dat1[, .on := 1]
    match_exact <- ".on"
  }

  # Left-join all observations to each case where exact matching.
  # conditions are fulfilled
  dat1 <- dat1[dat1[.case == 1], on = match_exact, allow.cartesian = TRUE]
  if (match_exact == ".on") {dat1[, ".on" := NULL]}

  # Restrict to potential controls fulfilling inexact matching conditions.
  if (!is.null(match_inexact)) {
    dat1 <- subset(dat1, eval(parse(text = match_inexact)))
  }

  # Pick random valid controls.
  dat1[, .rand := stats::runif(nrow(dat1))]
  data.table::setorderv(dat1, c("i..id", ".rand"))
  dat1 <- dat1[, lapply(.SD, first, ..n_controls), by = "i..id"
    ][, .match_id := .GRP, by = "i..id"
      ][, .rand := NULL][]

  # Find variable names for cases: all variables with a "i." prefix, the "
  # .match_id" variable, and all variables included in match_exact, if any.
  case_vars <- c(grep("^i\\.|^.match_id$", colnames(dat1), value = TRUE))
  if (!match_exact == ".on") {
    case_vars <- c(case_vars, match_exact)
  }

  # Find variable names for controls: variables in the data excluding the
  # case variables, but including ".match_id" and any match_exact variables.
  ctrl_vars <- c(setdiff(colnames(dat1), case_vars), ".match_id")
  if (!match_exact == ".on") {
    ctrl_vars <- c(ctrl_vars, match_exact)
  }

  # Split case and control data.
  cases <- dat1[, case_vars, with = FALSE
    ][, lapply(.SD, first, 1),  by = "i..id"]
  controls <- dat1[, ctrl_vars, with = FALSE]

  # Rename case variables with "i." prefix.
  names(cases) <- sub("^i\\.", "", names(cases))

  # Combine and sort data on long format.
  dat1 <- data.table::rbindlist(list(cases, controls), use.names = TRUE)
  data.table::setorderv(dat1, c(".match_id", ".case"), c(1, -1))

  # Reorder variables.
  colorder <- c(".match_id", ".case")
  if (!match_exact == ".on") {
    colorder <- c(colorder, match_exact)
  }
  data.table::setcolorder(dat1, colorder)[, .id := NULL][]
}
