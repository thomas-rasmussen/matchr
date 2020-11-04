n <- 100
dat <- data.frame(
  id = 1:n,
  case = c(rep(1, n/10), rep(0, (n * 9 / 10))),
  bin_var = rbinom(n, 1, 0.5),
  cont_var = rnorm(n, 0, 1)
)
dat_case <- dat[dat$case == 1, ]
dat_ctrl <- dat[dat$case == 0, ]

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
