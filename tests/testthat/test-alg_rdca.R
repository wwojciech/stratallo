# Function ----

test_that("rdca is valid function", {
  expect_function(
    rdca,
    args = c("n", "H_counts", "N", "S", "rho", "rho2", "U", "J")
  )
})

# H_counts = 1 ----

test_that("rdca works for H_counts=1, n<n_max", {
  H_counts <- 1
  N <- 10
  S <- 5
  rho <- 8
  n <- 3

  expect_identical(rdca(n, H_counts, N, S, rho, rho^2), n)
})

test_that("rdca errors when H_counts=1, n<n_max, and U=1", {
  H_counts <- 1
  N <- 10
  S <- 5
  rho <- 8
  n <- 3

  expect_error(rdca(n, H_counts, N, S, rho, rho^2, 1))
})

test_that("rdca works for H_counts=1, n=n_max", {
  H_counts <- 1
  N <- 10
  S <- 5
  rho <- 8
  n <- dca_nmax(H_counts, N, S) # 10

  expect_identical(rdca(n, H_counts, N, S, rho, rho^2), n)
})

test_that("rdca errors for H_counts=1, n=n_max, and U=1", {
  H_counts <- 1
  N <- 10
  S <- 5
  rho <- 8
  n <- dca_nmax(H_counts, N, S) # 10

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 1),
    "U must contain no strata from domains in J"
  )
})

# H_counts = 2 ----

test_that("rdca works for H_counts=2, n<n_max", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 14

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expected <- c(3.043478, 10.956522)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca errors for H_counts=2, n<n_max, U=1", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 14

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 1),
    "U must contain no strata from domains in J"
  )
})

test_that("rdca errors for H_counts=2, n<n_max, U=2, too small n", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 14

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 2),
    "U must be such that sum(N[U]) < n || (sum(N[U]) == n && sum(N) == n)"
  )
})

test_that("rdca errors for H_counts=2, n<n_max, U=2", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 25

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 2),
    "U must contain no strata from domains in J"
  )
})

test_that("rdca works for H_counts=2, n=n_max", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- dca_nmax(H_counts, N, S) # 28.28877

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expected <- c(8.28877, 20)
  expect_equal(result, expected, tolerance = 10^-8)
})

test_that("rdca works for H_counts=2, n>n_max", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 29

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expected <- c(9, 20)
  expect_identical(result, expected)
})

test_that("rdca works for H_counts=2, n=sum(N)", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- sum(N)

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expect_identical(result, N)
})

# H_counts = c(1,1) ----

test_that("rdca works for H_counts=c(1,1), n<n_max", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 16

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expected <- c(5.941302, 10.058698)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,1), n<n_max, U=1, J=2", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 16

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1, 2)
  expected <- c(N[1], 6)
  expect_identical(result, expected)
})

test_that("rdca errors for H_counts=c(1,1), n<n_max, U=2, J=1", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 16

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 2, 1),
    "U must be such that sum(N[U]) < n || (sum(N[U]) == n && sum(N) == n)"
  )
})

test_that("rdca works for H_counts=c(1,1), n<n_max, U=2, J=1", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 21

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2, 1)
  expected <- c(1, N[2])
  expect_identical(result, expected)
})

test_that("rdca works for H_counts=c(1,1), n<n_max, J=1", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 21

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 1)
  expected <- c(7.502103, 13.497897)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,1), n<n_max, J=2", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 21

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 2)
  expected <- c(7.502103, 13.497897)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,1), n=n_max", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 30

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expect_identical(result, N)
})

test_that("rdca works for H_counts=c(1,1), n=n_max, U=1, J=2", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 30

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1, 2)
  expect_identical(result, N)
})

test_that("rdca works for H_counts=c(1,1), n=n_max, U=2, J=1", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 30

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2, 1)
  expect_identical(result, N)
})

test_that("rdca works for H_counts=c(1,1), n=n_max, J=1", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 30

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 1)
  expect_identical(result, N)
})

test_that("rdca works for H_counts=c(1,1), n=n_max, J=2", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 30

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 2)
  expect_identical(result, N)
})

# H_counts = c(1,2) ----

test_that("rdca works for H_counts=c(1,2), n<n_max", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 12

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expected <- c(1.754532, 4.553542, 5.691927)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,2), n<n_max, U=1, J=2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 12

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1, 2)
  expected <- c(N[1], 0.8888889, 1.1111111)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca errors for H_counts=c(1,2), n<n_max, U=2, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 12

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 2, 1),
    "U must be such that sum(N[U]) < n || (sum(N[U]) == n && sum(N) == n)"
  )
})

test_that("rdca works for H_counts=c(1,2), n<n_max, U=2, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2, 1)
  expected <- c(3.48051, N[2], 6.51949)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,2), n<n_max, U=3, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- rdca(n, H_counts, N, S, rho, rho^2, 3, 1)
  expected <- c(5.613218, 9.386782, N[3])
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,2), n<n_max, U=2:3, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 36

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2:3, 1)
  expect_identical(result, c(1, N[2:3]))
})

test_that("rdca errors for H_counts=c(1,2), n<n_max, U=c(1,3), J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 36

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, c(1, 3), 1),
    "U must contain no strata from domains in J"
  )
})

test_that("rdca works for H_counts=c(1,2), n<n_max, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 12

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 1)
  expected <- c(1.754532, 4.553542, 5.691927)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,2), n<n_max, J=2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 12

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 2)
  expected <- c(1.754532, 4.553542, 5.691927)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,2), n=n_max", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expected <- c(9.434314, 18.403524, 15)
  expect_equal(result, expected, tolerance = 10^-8)
})

test_that("rdca works for H_counts=c(1,2), n=n_max, U=1, J=2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1, 2)
  expected <- c(N[1], 17.83784, 15)
  expect_equal(result, expected, tolerance = 10^-6)
})

test_that("rdca works for H_counts=c(1,2), n=n_max, U=2, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2, 1)
  expected <- c(8.95845, N[2], 13.87939)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,2), n=n_max, U=3, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- rdca(n, H_counts, N, S, rho, rho^2, 3, 1)
  expected <- c(9.434314, 18.403524, N[3])
  expect_equal(result, expected, tolerance = 10^-8)
})

test_that("rdca works for H_counts=c(1,2), n=n_max, U=2:3, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2:3, 1)
  expected <- c(7.837838, N[2:3])
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,2), n=n_max, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 1)
  expected <- c(10, 14.59459, 18.24324)
  expect_equal(result, expected, tolerance = 10^-6)
})

test_that("rdca works for H_counts=c(1,2), n=n_max, J=2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 2)
  expected <- c(9.434314, 18.403524, 15)
  expect_equal(result, expected, tolerance = 10^-8)
})

test_that("rdca works for H_counts=c(1,2), n>n_max", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expected <- c(9.477467, 18.522533, 15)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,2), n>n_max, U=1, J=2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1, 2)
  expected <- c(N[1], 17.9999999999999964, 15)
  expect_equal(result, expected)
})

test_that("rdca works for H_counts=c(1,2), n>n_max, U=2, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2, 1)
  expected <- c(9.035355, N[2], 13.964645)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,2), n>n_max, U=3, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- rdca(n, H_counts, N, S, rho, rho^2, 3, 1)
  expected <- c(9.477467, 18.522533, N[3])
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,2), n>n_max, U=2:3, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2:3, 1)
  expected <- c(8, N[2:3])
  expect_identical(result, expected)
})

test_that("rdca works for H_counts=c(1,2), n>n_max, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 1)
  expected <- c(10, 14.66667, 18.33333)
  expect_equal(result, expected, tolerance = 10^-6)
})

test_that("rdca works for H_counts=c(1,2), n>n_max, J=2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 2)
  expected <- c(9.477467, 18.522533, 15)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(1,2), n=sum(N)", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expect_identical(result, N)
})

test_that("rdca works for H_counts=c(1,2), n=sum(N), U=1, J=2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1, 2)
  expect_identical(result, N)
})

test_that("rdca works for H_counts=c(1,2), n=sum(N), U=2, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2, 1)
  expected <- c(9.9999999999999982, N[2], 15.0000000000000036)
  expect_equal(result, expected)
})

test_that("rdca works for H_counts=c(1,2), n=sum(N), U=3, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- rdca(n, H_counts, N, S, rho, rho^2, 3, 1)
  expect_identical(result, N)
})

test_that("rdca works for H_counts=c(1,2), n=sum(N), U=2:3, J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2:3, 1)
  expect_identical(result, N)
})

test_that("rdca works for H_counts=c(1,2), n=sum(N), J=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 1)
  expected <- c(N[1], 15.55556, 19.44444)
  expect_equal(result, expected, tolerance = 10^-6)
})

test_that("rdca works for H_counts=c(1,2), n=sum(N), J=2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 2)
  expect_identical(result, N)
})

# H_counts = c(2,2) ----

test_that("rdca works for H_counts=c(2,2), n<n_max", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expected <- c(4.496045, 16.185762, 6.874077, 2.444116)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n<n_max, U=1, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1, 2)
  expected <- c(N[1], 12.567779, 5.482786, 1.949435)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n<n_max, U=2, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2, 2)
  expected <- c(2.187241, N[2], 5.763511, 2.049248)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca errors for H_counts=c(2,2), n<n_max, U=1:2, J=2, too small n", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 1:2, 2),
    "U must be such that sum(N[U]) < n || (sum(N[U]) == n && sum(N) == n)"
  )
})

test_that("rdca works for H_counts=c(2,2), n<n_max, U=1:2, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 35

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1:2, 2)
  expected <- c(N[1:2], 3.688525, 1.311475)
  expect_equal(result, expected, tolerance = 10^-6)
})

test_that("rdca works for H_counts=c(2,2), n<n_max, U=3, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- rdca(n, H_counts, N, S, rho, rho^2, 3, 1)
  expected <- c(3.1768807, 11.4367705, 15, 0.3863488)
  expect_equal(result, expected, tolerance = 10^-8)
})

test_that("rdca errors for H_counts=c(2,2), n<n_max, U=4, J=1, too small n", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 4, 1),
    "U must be such that sum(N[U]) < n || (sum(N[U]) == n && sum(N) == n)"
  )
})

test_that("rdca works for H_counts=c(2,2), n<n_max, U=4, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 45

  result <- rdca(n, H_counts, N, S, rho, rho^2, 4, 1)
  expected <- c(0.9730751, 3.5030703, 0.5238546, N[4])
  expect_equal(result, expected, tolerance = 10^-8)
})

test_that("rdca errors for H_counts=c(2,2), n<n_max, U=3:4, J=1, too small n", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 53 # n_max = 54.60419

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 3:4, 1),
    "U must be such that sum(N[U]) < n || (sum(N[U]) == n && sum(N) == n)"
  )
})

test_that("rdca errors for H_counts=c(2,2), n<n_max, U=1:3, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 53 # n_max = 54.60419

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 1:3, 1),
    "U must contain no strata from domains in J"
  )
})

test_that("rdca works for H_counts=c(2,2), n<n_max, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 1)
  expected <- c(4.496045, 16.185762, 6.874077, 2.444116)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n<n_max, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 2)
  expected <- c(4.496045, 16.185762, 6.874077, 2.444116)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n=n_max", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expected <- c(8.49268, 20, 15, 11.11151)
  expect_equal(result, expected, tolerance = 10^-6)
})

test_that("rdca works for H_counts=c(2,2), n=n_max, U=1, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1, 2)
  expected <- c(N[1], 19.40291, 15.00000, 10.20128)
  expect_equal(result, expected, tolerance = 10^-6)
})

test_that("rdca works for H_counts=c(2,2), n=n_max, U=2, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2, 2)
  expected <- c(8.49268, N[2], 15, 11.11151)
  expect_equal(result, expected, tolerance = 10^-6)
})

test_that("rdca works for H_counts=c(2,2), n=n_max, U=1:2, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1:2, 2)
  expected <- c(N[1:2], 15, 9.604187)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n=n_max, U=3, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- rdca(n, H_counts, N, S, rho, rho^2, 3, 1)
  expected <- c(8.49268, N[2], 15, 11.11151)
  expect_equal(result, expected, tolerance = 10^-6)
})

test_that("rdca works for H_counts=c(2,2), n=n_max, U=4, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- rdca(n, H_counts, N, S, rho, rho^2, 4, 1)
  expected <- c(2.738359, 9.858092, 2.007737, N[4])
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca errors for H_counts=c(2,2), n=n_max, U=3:4, J=1, too small n", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 3:4, 1),
    "U must be such that sum(N[U]) < n || (sum(N[U]) == n && sum(N) == n)"
  )
})

test_that("rdca works for H_counts=c(2,2), n=n_max, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 1)
  expected <- c(9.384822, 20, 18.604450, 6.614916)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n=n_max, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 2)
  expected <- c(6.020169, 21.672609, 15, 11.911409)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n>n_max", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expected <- c(9.621571, 20, 15, 25.378429)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n>n_max, U=1, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1, 2)
  expected <- c(N[1], 19.87602, 15, 25.12398)
  expect_equal(result, expected, tolerance = 10^-6)
})

test_that("rdca works for H_counts=c(2,2), n>n_max, U=2, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2, 2)
  expected <- c(9.621571, N[2], 15, 25.378429)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n>n_max, U=1:2, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1:2, 2)
  expected <- c(N[1:2], 15, 24.9999999999999964)
  expect_equal(result, expected)
})

test_that("rdca works for H_counts=c(2,2), n>n_max, U=3, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- rdca(n, H_counts, N, S, rho, rho^2, 3, 1)
  expected <- c(9.621571, 20, N[3], 25.378429)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n>n_max, U=4, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- rdca(n, H_counts, N, S, rho, rho^2, 4, 1)
  expected <- c(5.019114, 18.068809, 6.912078, N[4])
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n>n_max, U=3:4, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- rdca(n, H_counts, N, S, rho, rho^2, 3:4, 1)
  expected <- c(3.26087, 11.73913, N[3:4])
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n>n_max, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 1)
  expected <- c(10, 20, 29.5082, 10.4918)
  expect_equal(result, expected, tolerance = 10^-6)
})

test_that("rdca works for H_counts=c(2,2), n>n_max, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 2)
  expected <- c(6.122328, 22.040379, 15, 26.837293)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n=sum(N)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  result <- rdca(n, H_counts, N, S, rho, rho^2)
  expected <- c(N[1:3], 39.9999999999999929)
  expect_equal(result, expected)
})

test_that("rdca works for H_counts=c(2,2), n=sum(N), U=1, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1, 2)
  expect_identical(result, N)
})

test_that("rdca works for H_counts=c(2,2), n=sum(N), U=2, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  result <- rdca(n, H_counts, N, S, rho, rho^2, 2, 2)
  expect_identical(result, N)
})

test_that("rdca works for H_counts=c(2,2), n=sum(N), U=1:2, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  result <- rdca(n, H_counts, N, S, rho, rho^2, 1:2, 2)
  expected <- c(N[1:2], 15, 39.9999999999999929)
  expect_equal(result, expected)
})

test_that("rdca works for H_counts=c(2,2), n=sum(N), U=3, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  result <- rdca(n, H_counts, N, S, rho, rho^2, 3, 1)
  expected <- c(9.9999999999999982, 20, N[3], 40)
  expect_equal(result, expected)
})

test_that("rdca works for H_counts=c(2,2), n=sum(N), U=4, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  result <- rdca(n, H_counts, N, S, rho, rho^2, 4, 1)
  expected <- c(9.9999999999999982, 20, 15.0000000000000036, N[4])
  expect_equal(result, expected)
})

test_that("rdca works for H_counts=c(2,2), n=sum(N), U=3:4, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  result <- rdca(n, H_counts, N, S, rho, rho^2, 3:4, 1)
  expect_identical(result, N)
})

test_that("rdca errors for H_counts=c(2,2), n=sum(N), U=1:3, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 1:3, 1),
    "U must contain no strata from domains in J"
  )
})

test_that("rdca errors for H_counts=c(2,2), n=sum(N), U=1:4, J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 1:4, 1),
    "U must contain no strata from domains in J"
  )
})

test_that("rdca errors for H_counts=c(2,2), n=sum(N), U=1:3, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 1:3, 1),
    "U must contain no strata from domains in J"
  )
})

test_that("rdca errors for H_counts=c(2,2), n=sum(N), U=1:4, J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  expect_error(
    rdca(n, H_counts, N, S, rho, rho^2, 1:4, 1),
    "U must contain no strata from domains in J"
  )
})

test_that("rdca works for H_counts=c(2,2), n=sum(N), J=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 1)
  expected <- c(10, 20, 40.57377, 14.42623)
  expect_equal(result, expected, tolerance = 10^-7)
})

test_that("rdca works for H_counts=c(2,2), n=sum(N), J=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  result <- rdca(n, H_counts, N, S, rho, rho^2, J = 2)
  expected <- c(6.521739, 23.478261, 15, 40)
  expect_equal(result, expected, tolerance = 10^-8)
})

# pop9d278s ----

test_that("rdca works for pop9d278s, n<n_max", {
  p <- pop9d278s
  result <- rdca(1000, p$H_counts, p$N, p$S, p$rho, p$rho2)
  expect_snapshot(result)
})

test_that("rdca works for pop9d278s, n<n_max, U=100, J=c(1,5)", {
  p <- pop9d278s
  result <- rdca(1000, p$H_counts, p$N, p$S, p$rho, p$rho2, 100, c(1, 5))
  expect_snapshot(result)
})

test_that("rdca works for pop9d278s, n<n_max, U=c(2, 150), J=2", {
  p <- pop9d278s
  result <- rdca(1000, p$H_counts, p$N, p$S, p$rho, p$rho2, c(2, 150), 2)
  expect_snapshot(result)
})

test_that("rdca works for pop9d278s, n<n_max, U=1:36, J=3:8", {
  p <- pop9d278s
  result <- rdca(6100, p$H_counts, p$N, p$S, p$rho, p$rho2, 1:36, 3:8)
  expect_snapshot(result)
})

test_that("rdca works for pop9d278s, n<n_max, U=1:37, J=4:8", {
  p <- pop9d278s
  result <- rdca(6100, p$H_counts, p$N, p$S, p$rho, p$rho2, 1:36, 4:8)
  expect_snapshot(result)
})

test_that("rdca works for pop9d278s, n<n_max, J=c(1,4)", {
  p <- pop9d278s
  result <- rdca(1000, p$H_counts, p$N, p$S, p$rho, p$rho2, J = c(1, 4))
  expect_snapshot(result)
})

test_that("rdca works for pop9d278s, n=n_max", {
  p <- pop9d278s
  result <- rdca(p$n_max, p$H_counts, p$N, p$S, p$rho, p$rho2)
  expect_snapshot(result)
})

test_that("rdca works for pop9d278s, n=n_max, J=c(2,8)", {
  p <- pop9d278s
  result <- rdca(p$n_max, p$H_counts, p$N, p$S, p$rho, p$rho2, J = c(2, 8))
  expect_snapshot(result)
})

test_that("rdca works for pop9d278s, n>n_max", {
  p <- pop9d278s
  result <- rdca(20000, p$H_counts, p$N, p$S, p$rho, p$rho2)
  expect_snapshot(result)
})

test_that("rdca works for pop9d278s, n>n_max, J=5", {
  p <- pop9d278s
  result <- rdca(20000, p$H_counts, p$N, p$S, p$rho, p$rho2, J = 5)
  expect_snapshot(result)
})

# pop2d4s ----

test_that("rdca works for pop2d4s, n=sum(N)", {
  p <- pop2d4s
  result <- rdca(sum(p$N), p$H_counts, p$N, p$S, p$rho, p$rho2)
  expect_identical(result, pop2d4s$N)
})

test_that("rdca works for pop2d4s, n=sum(N), J=1", {
  p <- pop2d4s
  result <- rdca(sum(p$N), p$H_counts, p$N, p$S, p$rho, p$rho2, J = 1)
  expect_equal(result, c(140, 110, 143.8802, 181.1198), tolerance = 10^-7)
})

test_that("rdca works for pop2d4s, n=sum(N), J=2", {
  p <- pop2d4s
  result <- rdca(sum(p$N), p$H_counts, p$N, p$S, p$rho, p$rho2, J = 2)
  expect_equal(result, c(198.11321, 51.88679, 135, 190), tolerance = 10^-7)
})
