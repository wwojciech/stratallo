# var_st ----

test_that("var_st works as expected)", {
  N <- c(300, 400, 500, 200)
  S <- c(2, 5, 3, 1)
  x <- c(27, 88, 66, 9)
  A <- N * S
  A0 <- sum(N * S^2)
  result <- var_st(x, A, A0)
  expect_equal(result, 81423.232)
})

# var_stsi ----

test_that("var_stsi works as expected)", {
  N <- c(300, 400, 500, 200)
  S <- c(2, 5, 3, 1)
  x <- c(27, 88, 66, 9)
  result <- var_stsi(x, N, S)
  expect_equal(result, 81423.232)
})

test_that("var_stsi works as expected 2)", {
  N <- c(3000, 4000, 5000, 2000)
  S <- rep(1, 4)
  x <- c(96, 90, 70, 64) # optimum allocation for M = c(100, 90, 70, 80)
  result <- var_stsi(x, N, S)
  expect_equal(result, 677170.635)
})
