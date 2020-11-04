n <- 100
dat <- data.frame(
  id = 1:n,
  case = c(rep(1, n/10), rep(0, (n * 9 / 10))),
  bin_var = rbinom(n, 1, 0.5),
  cont_var = rnorm(n, 0, 1)
)

test_that("Update matchr fucntion", {
  expect_equal(2 * 2, 4)
})
