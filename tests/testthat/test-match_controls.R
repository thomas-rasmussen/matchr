n <- 100
set.seed(1)
dat <- data.frame(
  id = 1:n,
  case = c(rep(1, n/10), rep(0, (n * 9 / 10))),
  bin_var = rbinom(n, 1, 0.5),
  cont_var = rnorm(n, 0, 1)
)
dat_case <- dat[dat$case == 1, ]
dat_ctrl <- dat[dat$case == 0, ]

test_that("non data-frame input data results in error", {
  expect_error(
    match_controls(list(dat_case), dat_ctrl),
    "dat_case must have class data.frame"
  )
  expect_error(
    match_controls(dat_case, list(dat_ctrl)),
    "dat_ctrl must have class data.frame"
  )
})

test_that(
  "input data with different columns and/or column order results in error", {

  expect_error(
    match_controls(dat_case[, 1, drop = FALSE], dat_ctrl),
    "dat_case and dat_ctrl must have the same column names and column order"
  )
  expect_error(
    match_controls(dat_case, dat_ctrl[, 1, drop = FALSE]),
    "dat_case and dat_ctrl must have the same column names and column order"
  )
  expect_error(
    match_controls(dat_case[, rev(names(dat_case))], dat_ctrl),
    "dat_case and dat_ctrl must have the same column names and column order"
  )
  expect_error(
    match_controls(dat_case, dat_ctrl[, rev(names(dat_ctrl))]),
    "dat_case and dat_ctrl must have the same column names and column order"
  )

})

test_that("improper match_exact input results in error", {
  expect_error(
    match_controls(dat_case, dat_ctrl, match_exact = not_an_object),
    "object 'not_an_object' not found"
  )
  expect_error(
    match_controls(dat_case, dat_ctrl, match_exact = "not_a_var"),
    "One or more variable in match_exact is not a column in dat_case"
  )
})

test_that("improper match_inexact input results in error", {
  expect_error(
    match_controls(dat_case, dat_ctrl, match_inexact = not_an_object),
    "object 'not_an_object' not found"
  )
  expect_error(
    match_controls(dat_case, dat_ctrl, match_inexact = c("not", "length 1")),
    "match_inexact must be of length 1"
  )
})

test_that("n_controls accepts integer-like input", {
  expect_error(match_controls(dat_case, dat_ctrl, n_controls = 1L),  NA)
  expect_error(match_controls(dat_case, dat_ctrl, n_controls = 2),   NA)
  expect_error(match_controls(dat_case, dat_ctrl, n_controls = 3.0), NA)
})

test_that("n_controls produces error for non-integer input", {
  expect_error(
    match_controls(dat_case, dat_ctrl, n_controls = c(1, 2, 3)),
    "n_control must be of length 1"
  )
  expect_error(
    match_controls(dat_case, dat_ctrl, n_controls = -1),
    "n_control must be an integer"
  )
  expect_error(
    match_controls(dat_case, dat_ctrl, n_controls = "1"),
    "n_control must be numeric"
  )
})
