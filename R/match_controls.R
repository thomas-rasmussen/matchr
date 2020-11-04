#' Exact and inexact matching
#'
#' Find matches for cases using exact and/or inexact matching criterias.
#'
#' Data on cases and potential controls are provided in a combined data.table
#' with a variable denoting which observations are cases for which controls are
#' to be found among all other observations, including the other cases.
#'
#' The function works by making a cartesian product of controls to the cases,
#' matching on any exact matching criterias. This also explains the form in
#' which inexact matching criterias have to be provided, eg
#' "var1 < i.var & abs(var2 - i.var2)": case values of variables are specified
#' using a i. prefix, a result of how variables are named by data.table when
#' doing joins.
#'
#' @param dat_case A data.table (or object that can be coerced to a data.table)
#'  with cases
#' @param dat_ctrl A data.table (or object that can be coerced to a data.table)
#'  with controls
#' @param match_exact Character vector of variables on which to do exact
#'  matching.
#' @param match_inexact String with inexact matching criterias. See examples.
#' @param n_controls Numeric vector of length 1. Number of controls to match to
#'  each case.
#' @param seed Seed value
#' @return data.table
#' @examples
#' n <- 100
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

# Quiet R CMD check notes of the type "no visible binding for global variable..."
  .id <- .on <- .rand <- .match_id <- .case <- ..n_controls <- NULL

  set.seed(seed)

  if( !identical(names(dat_case), names(dat_ctrl)) ) {
    stop("Case and control input datasets must have the
         same column names and order", call. = FALSE)
  }

  if( !identical(sapply(dat_case, class), sapply(dat_ctrl, class)) ) {
    stop("Case and control input datasets must have the
         same column types", call. = FALSE)
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


  # Coerce object to data.table
  dat_case1 <- data.table::as.data.table(dat_case)[, .case := 1]
  dat_ctrl1 <- data.table::as.data.table(dat_ctrl)[, .case := 0]
  dat1 <- data.table::rbindlist(
    list(dat_case1, dat_ctrl1),
    use.names = TRUE
    )[, .id := .I]

  if (is.null(match_exact)) {
    dat1[, .on := 1]
    match_exact <- ".on"
  }

  # Left-join all observations to each case where exact matching
  # conditions fulfilled
  dat1 <- dat1[dat1[.case == 1], on = match_exact, allow.cartesian = TRUE]
  if (match_exact == ".on") {dat1[, ".on" := NULL]}
  # Restrict to potential controls fulfilling the inexact
  # matching conditions
  if (!is.null(match_inexact)) {
    dat1 <- subset(dat1, eval(parse(text = match_inexact)))
  }
  # Pick n random controls
  dat1[, .rand := stats::runif(nrow(dat1))]
  data.table::setorderv(dat1, c("i..id", ".rand"))
  dat1 <- dat1[, lapply(.SD, first, ..n_controls),  by = "i..id"
    ][, .match_id := .GRP, by = "i..id"
    ][, .rand := NULL][]

  # Find variable names for cases: all variables with a "i." prefix, the ".match_id"
  # variable, and all variables included in match_exact, if any
  cases_vars <- c(grep("^i\\.|^.match_id$", colnames(dat1), value = TRUE))
  if (!match_exact == ".on") {
    cases_vars <- c(cases_vars, match_exact)
  }
  # Find variable names for controls, as variables in the data excluding the
  # case variables, but including ".match_id" and any match_exact variables
  controls_vars <- c(setdiff(colnames(dat1), cases_vars), ".match_id")
  if (!match_exact == ".on") {
    controls_vars <- c(controls_vars, match_exact)
  }
  # Split case and control data
  cases <- dat1[, cases_vars, with = FALSE][, lapply(.SD, first, 1),  by = "i..id"]
  controls <- dat1[, controls_vars, with = FALSE]
  # Rename case variables with a "i." prefix
  old_case_names <- cases_vars
  new_case_names <- sub("^i.", "", old_case_names)
  for(i in seq(old_case_names)){
    data.table::setnames(cases, old_case_names[i], new_case_names[i])
  }

  # Combine and sort data on long format
  dat1 <- data.table::rbindlist(list(cases, controls), use.names = TRUE)
  data.table::setorderv(dat1, c(".match_id", ".case"), c(1, -1))

  # Reorder variables
  colorder <- c(".match_id", ".case")
  if (!match_exact == ".on") {
    colorder <- c(colorder, match_exact)
  }
  data.table::setcolorder(dat1, colorder)[, .id := NULL][]
}
