N <- c(300, 400, 500, 200)
S <- c(2, 5, 3, 1)
x <- c(27, 88, 66, 9)

# var_tst ----

test_that("var_tst works as expected)", {
  a <- N * S
  b <- sum(N * S^2)
  result <- var_tst(x, a, b)
  expect_equal(result, 81423.232)
})

# var_tst_si ----

test_that("var_tst_si works as expected)", {
  result <- var_tst_si(x, N, S)
  expect_equal(result, 81423.232)
})
