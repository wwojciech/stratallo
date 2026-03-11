# Function ----

test_that("dca0 is valid function", {
  expect_function(
    dca0,
    args = c("n", "H_counts", "N", "S", "rho", "rho2", "details")
  )
})

# H_counts = 1 ----

test_that("dca0 works for H_counts=c(1), n<n_max", {
  H_counts <- c(1)
  N <- 10
  S <- 5
  rho <- 8
  n <- 3

  expect_identical(dca0(n, H_counts, N, S, rho, rho^2), n)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = 9.114583,
    k = 0.48,
    v = 1,
    s = 0.48,
    x = n
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca0 works for H_counts=c(1), n=n_max", {
  H_counts <- c(1)
  N <- 10
  S <- 5
  rho <- 8
  n <- dca_nmax(H_counts, N, S) # 10

  expect_identical(dca0(n, H_counts, N, S, rho, rho^2), n)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = 0,
    k = 1.6,
    v = 1,
    s = 1.6,
    x = n
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

# H_counts = 2 ----

test_that("dca0 works for H_counts=2, n<n_max", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 14

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(3.043478, 10.956522)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = 29.82143,
    k = 0.4869565,
    v = 1,
    s = 0.4869565,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca0 works for H_counts=2, n=n_max", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- dca_nmax(H_counts, N, S) # 28.28877

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(6.149733, 22.139037)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = 0,
    k = 0.9839572,
    v = 1,
    s = 0.9839572,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca0 works for H_counts=2, n>n_max", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 29

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(6.304348, 22.695652)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = -0.7165948,
    k = 1.008696,
    v = 1,
    s = 1.008696,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca0 works for H_counts=2, n=sum(N)", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- sum(N)

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(6.521739, 23.478261)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = -1.666667,
    k = 1.043478,
    v = 1,
    s = 1.043478,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

# H_counts = c(1,1) ----

test_that("dca0 works for H_counts=c(1,1), n<n_max", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 16

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(5.941302, 10.058698)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = 106.7395,
    k = -0.263501,
    v = c(-0.5704128, -0.8213582),
    s = c(0.1503044, 0.2164287),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca0 works for H_counts=c(1,1), n=n_max", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 30

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- N
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = 0,
    k = -0.4991845,
    v = c(-0.506791, -0.862069),
    s = c(0.2529822, 0.4303315),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

# H_counts = c(1,2) ----

test_that("dca0 works for H_counts=c(1,2), n<n_max", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 12

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(1.754532, 4.553542, 5.691927)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = 734.3011,
    k = 0.107562,
    v = c(0.4126600, 0.9108851),
    s = c(0.04438653, 0.09797661),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca0 works for H_counts=c(1,2), n=n_max", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(10, 14.59459, 18.24324)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = -1.421085e-14,
    k = -0.4032519,
    v = c(-0.6273552, -0.7787332),
    s = c(0.2529822, 0.31402576),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca0 works for H_counts=c(1,2), n>n_max", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(10.06395, 14.63824, 18.29780)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = -0.9929328,
    k = -0.4049989,
    v = c(-0.6286441, -0.7776931),
    s = c(0.2546001, 0.3149648),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca0 works for H_counts=c(1,2), n=sum(N)", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(10.87450, 15.16689, 18.95861)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = -12.56518,
    k = -0.426826,
    v = c(-0.6445375, -0.7645727),
    s = c(0.2751054, 0.3263395),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

# H_counts = c(2,2) ----

test_that("dca0 works for H_counts=c(2,2), n<n_max", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(4.496045, 16.185762, 6.874077, 2.444116)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = 429.8772,
    k = -0.1641283,
    v = c(-0.6930063, -0.7209315),
    s = c(0.1137419, 0.1183253),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca0 works for H_counts=c(2,2), n=n_max", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(6.149733, 22.139037, 19.413013, 6.902405)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = 0,
    k = -0.3686028,
    v = c(-0.4220730, -0.90656182),
    s = c(0.1555773, 0.3341612),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca0 works for H_counts=c(2,2), n>n_max", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(6.597173, 23.749824, 29.252215, 10.400788)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = -79.26825,
    k = -0.5304648,
    v = c(-0.3146236, -0.9492165),
    s = c(0.1668968, 0.5035260),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca0 works for H_counts=c(2,2), n=sum(N)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expected <- c(6.851509, 24.665431, 39.454717, 14.028344)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expected_details <- list(
    lambda = -119.7109,
    k = -0.7009141,
    v = c(-0.2472928, -0.9689408),
    s = c(0.1733310, 0.6791443),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

# pop9d278s ----

test_that("dca0 works for pop9d278s, n<n_max", {
  H_counts <- pop9d278s$H_counts
  N <- pop9d278s$N
  S <- pop9d278s$S
  rho <- pop9d278s$rho
  n <- 1000

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expect_snapshot(result)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expect_snapshot(result_details)
})

test_that("dca0 works for pop9d278s, n=n_max", {
  H_counts <- pop9d278s$H_counts
  N <- pop9d278s$N
  S <- pop9d278s$S
  rho <- pop9d278s$rho
  n <- pop9d278s$n_max

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expect_snapshot(result)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expect_snapshot(result_details)
})

test_that("dca0 works for pop9d278s, n>n_max", {
  H_counts <- pop9d278s$H_counts
  N <- pop9d278s$N
  S <- pop9d278s$S
  rho <- pop9d278s$rho
  n <- 20000

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expect_snapshot(result)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expect_snapshot(result_details)
})

test_that("dca0 works for pop9d278s, n=sum(N)", {
  H_counts <- pop9d278s$H_counts
  N <- pop9d278s$N
  S <- pop9d278s$S
  rho <- pop9d278s$rho
  n <- sum(N)

  result <- dca0(n, H_counts, N, S, rho, rho^2)
  expect_snapshot(result)

  # Check details.
  result_details <- dca0(n, H_counts, N, S, rho, rho^2, TRUE)
  expect_snapshot(result_details)
})
