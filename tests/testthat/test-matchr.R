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

matchr(dat_case, dat_ctrl, split_n = 2, parallel = TRUE, seed = 2)

test_that("parallel only accepts logical vecctors of length 1", {
  expect_error(matchr(dat_case, dat_ctrl, parallel = TRUE), NA)
  expect_error(matchr(dat_case, dat_ctrl, parallel = FALSE), NA)
  expect_error(
    matchr(dat_case, dat_ctrl, parallel = 1),
    "parallel must be a logical vector or length 1"
  )
  expect_error(
    matchr(dat_case, dat_ctrl, parallel = "FALSE"),
    "parallel must be a logical vector or length 1"
  )
})

test_that("n_split only accepts proper values", {
  expect_error(matchr(dat_case, dat_ctrl, split_n = 2), NA)
  expect_error(matchr(dat_case, dat_ctrl, split_n = 3.0), NA)
  expect_error(matchr(dat_case, dat_ctrl, split_n = 4L), NA)
  expect_error(
    matchr(dat_case, dat_ctrl, split_n = TRUE),
    "split_n must be numeric"
  )
  expect_error(
    matchr(dat_case, dat_ctrl, split_n = "4"),
    "split_n must be numeric"
  )
  expect_error(
    matchr(dat_case, dat_ctrl, split_n = c(1, 2)),
    "split_n must have length 1"
  )
  expect_error(
    matchr(dat_case, dat_ctrl, split_n = 4.5),
    "split_n must be an integer"
  )
  expect_error(
    matchr(dat_case, dat_ctrl, split_n = n + 1),
    "split_n can not be higher than nrow(dat_case), the number of cases",
    fixed = TRUE
  )
})


