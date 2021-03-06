% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_controls.R
\name{match_controls}
\alias{match_controls}
\title{Exact and inexact matching}
\usage{
match_controls(
  dat_case,
  dat_ctrl,
  match_exact = NULL,
  match_inexact = NULL,
  n_controls = 1L,
  seed = NULL
)
}
\arguments{
\item{dat_case}{A data.table (or object that can be coerced to a data.table)
with cases}

\item{dat_ctrl}{A data.table (or object that can be coerced to a data.table)
with controls}

\item{match_exact}{Character vector of variables on which to do exact
matching.}

\item{match_inexact}{String with inexact matching criterias. See examples.}

\item{n_controls}{Numeric vector of length 1. Number of controls to match to
each case.}

\item{seed}{Seed value}
}
\value{
data.table
}
\description{
Find matches for cases using exact and/or inexact matching criterias.
}
\details{
Data on cases and potential controls are provided in a combined data.table
with a variable denoting which observations are cases for which controls are
to be found among all other observations, including the other cases.

The function works by making a cartesian product of controls to the cases,
matching on any exact matching criterias. This also explains the form in
which inexact matching criterias have to be provided, eg
"var1 < i.var & abs(var2 - i.var2)": case values of variables are specified
using a i. prefix, a result of how variables are named by data.table when
doing joins.
}
\examples{
n <- 100
dat <- data.frame(
  id = 1:100,
  case = c(rep(1, n/10), rep(0, (n * 9 / 10))),
  bin_var = rbinom(n, 1, 0.5),
  cont_var = rnorm(n, 0, 1)
)
matches <- match_controls(
  dat[dat$case == 1, ],
  dat[dat$case == 0, ],
  match_exact = "bin_var",
  match_inexact = "!i.id == id & abs(cont_var - i.cont_var) < 0.1",
  seed = 2
)
}
