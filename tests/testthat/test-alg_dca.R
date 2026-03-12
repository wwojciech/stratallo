# Function ----

test_that("dca is valid function", {
  expect_function(
    dca,
    args = c("n", "H_counts", "N", "S", "rho", "rho2", "U", "details")
  )
})

# H_counts = 1 ----

test_that("dca works for H_counts=1, n<n_max", {
  H_counts <- 1
  N <- 10
  S <- 5
  rho <- 8
  n <- 3

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expect_identical(result, n)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca errors for H_counts=1, n<n_max, and U=1", {
  H_counts <- 1
  N <- 10
  S <- 5
  rho <- 8
  n <- 3

  expect_error(dca(n, H_counts, N, S, rho, rho^2, 1))
})

test_that("dca works for H_counts=1, n=n_max", {
  H_counts <- 1
  N <- 10
  S <- 5
  rho <- 8
  n <- dca_nmax(H_counts, N, S) # 10

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expect_identical(result, n)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=1, n=n_max, and U=1", {
  H_counts <- 1
  N <- 10
  S <- 5
  rho <- 8
  n <- dca_nmax(H_counts, N, S) # 10

  expect_identical(dca(n, H_counts, N, S, rho, rho^2, 1), n)
  expect_identical(dca(n, H_counts, N, S, rho, rho^2, 1, TRUE), n)
})

# H_counts = 2 ----

test_that("dca works for H_counts=2, n<n_max", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 14

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(3.043478, 10.956522)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=2, n<n_max, U=1", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 14

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expected <- c(N[1], 4)
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = 101.25,
    k = 0.1777778,
    v = 1,
    s = 0.1777778,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca errors for H_counts=2, n<n_max, U=2, too small n", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 14

  expect_error(
    dca(n, H_counts, N, S, rho, rho^2, 2),
    "U must be such that n > sum(N[U]) || (n == sum(N[U]) && n == sum(N))"
  )
})

test_that("dca works for H_counts=2, n<n_max, U=2", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 25

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expected <- c(5, N[2])
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = 3.90625,
    k = 0.8,
    v = 1,
    s = 0.8,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=2, n=n_max", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- dca_nmax(H_counts, N, S) # 28.28877

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(6.149733, 22.139037)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=2, n=n_max, U=1", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- dca_nmax(H_counts, N, S) # 28.28877

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expected <- c(N[1], 18.28877)
  expect_equal(result, expected, tolerance = 10^-8)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = 2.368421,
    k = 0.8128342,
    v = 1,
    s = 0.8128342,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=2, n=n_max, U=2", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- dca_nmax(H_counts, N, S) # 28.28877

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expected <- c(8.28877, N[2])
  expect_equal(result, expected, tolerance = 10^-8)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = 0.8064516,
    k = 1.326203,
    v = 1,
    s = 1.326203,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=2, n>n_max", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 29

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(6.304348, 22.695652)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=2, n>n_max, U=1", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 29

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expected <- c(N[1], 19)
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = 1.332237,
    k = 0.8444444,
    v = 1,
    s = 0.8444444,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=2, n>n_max, U=2", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- 29

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expected <- c(9, N[2])
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = 0.4340278,
    k = 1.44,
    v = 1,
    s = 1.44,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=2, n=sum(N)", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(6.521739, 23.478261)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=2, n=sum(N), U=1", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expect_identical(result, N)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = 0,
    k = 0.8888889,
    v = 1,
    s = 0.8888889,
    x = N
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=2, n=sum(N), U=2", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expect_identical(result, N)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = 0,
    k = 1.6,
    v = 1,
    s = 1.6,
    x = N
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=2, n=sum(N), U=1:2", {
  H_counts <- 2
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- 8
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:2)
  expect_identical(result, N)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = 0,
    k = 1.6,
    v = 1,
    s = 1.6,
    x = N
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

# H_counts = c(1,1) ----

test_that("dca works for H_counts=c(1,1), n<n_max", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 16

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(5.941302, 10.058698)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=c(1,1), n<n_max, U=1", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 16

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expected <- c(N[1], 6)
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = 252,
    k = 0.1290994,
    v = 1,
    s = 0.1290994,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca errors for H_counts=c(1,1), n<n_max, U=2, too small n", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 16

  expect_error(
    dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE),
    "U must be such that n > sum(N[U]) || (n == sum(N[U]) && n == sum(N))"
  )
})

test_that("dca works for H_counts=c(1,1), n<n_max, U=2", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 25

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expected <- c(5, N[2])
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = 156.25,
    k = 0.1264911,
    v = 1,
    s = 0.1264911,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,1), n=n_max", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 30

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- N
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=c(1,1), n=n_max, U=1", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 30

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expect_equal(result, N, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = 1.421085e-14,
    k = 0.4303315,
    v = 1,
    s = 0.4303315,
    x = N
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,1), n=n_max, U=2", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 30

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expect_identical(result, N)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = -2.842171e-14,
    k = 0.2529822,
    v = 1,
    s = 0.2529822,
    x = N
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,1), n=sum(N)", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(N[1], 20.0000000000000036)
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=c(1,1), n=sum(N), U=1", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expect_identical(result, N)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = -1.421085e-14,
    k = 0.4303315,
    v = 1,
    s = 0.4303315,
    x = N
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,1), n=sum(N), U=2", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expect_identical(result, N)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = -2.842171e-14,
    k = 0.2529822,
    v = 1,
    s = 0.2529822,
    x = N
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-7)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,1), n=sum(N), U=1:2", {
  H_counts <- c(1, 1)
  N <- c(10, 20)
  S <- c(5, 9)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:2)
  expect_identical(result, N)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:2, details = TRUE)
  expect_identical(result, N)
})

# H_counts = c(1,2) ----

# TODO

test_that("dca works for H_counts=c(1,2), n<n_max", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 12

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(1.754532, 4.553542, 5.691927)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=c(1,2), n<n_max, U=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 12

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expected <- c(N[1], 0.8888889, 1.1111111)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = 5134.5,
    k = 0.01912584,
    v = 1,
    s = 0.01912584,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca errors for H_counts=c(1,2), n<n_max, U=2, too small n", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 12

  expect_error(
    dca(n, H_counts, N, S, rho, rho^2, 2),
    "U must be such that n > sum(N[U]) || (n == sum(N[U]) && n == sum(N))"
  )
})

test_that("dca works for H_counts=c(1,2), n<n_max, U=2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 25

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expected <- c(1.659467, N[2], 3.340533)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = 785.3177,
    k = 0.07119599,
    v = c(0.5896617, 0.8076503),
    s = c(0.04198155, 0.05750147),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(1,2), n<n_max, U=3", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 16

  result <- dca(n, H_counts, N, S, rho, rho^2, 3)
  expected <- c(0.4165945, 0.5834055, N[3])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 3, details = TRUE)
  expected_details <- list(
    lambda = 3594.399,
    k = 0.01639047,
    v = c(0.6430016, 0.7658648),
    s = c(0.01053910, 0.01255289),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(1,2), n<n_max, U=1:2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 35

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:2)
  expected <- c(N[1:2], 5)
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:2, details = TRUE)
  expected_details <- list(
    lambda = 450,
    k = 0.0860663,
    v = 1,
    s = 0.0860663,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n<n_max, U=c(1,3)", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 35

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3))
  expected <- c(N[1], 10, N[3])
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3), details = TRUE)
  expected_details <- list(
    lambda = 108,
    k = 0.2151657,
    v = 1,
    s = 0.2151657,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n<n_max, U=2:3", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 40

  result <- dca(n, H_counts, N, S, rho, rho^2, 2:3)
  expected <- c(5, N[2:3])
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2:3, details = TRUE)
  expected_details <- list(
    lambda = 156.25,
    k = 0.1264911,
    v = 1,
    s = 0.1264911,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n=n_max", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(10, 14.59459, 18.24324)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=c(1,2), n=n_max, U=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expected <- c(N[1], 14.59459, 18.24324)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = 0,
    k = 0.3140257,
    v = 1,
    s = 0.3140257,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n=n_max, U=2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expected <- c(8.95845, N[2], 13.87939)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = 18.16634,
    k = -0.3293026,
    v = c(-0.6882206, -0.7255014),
    s = c(0.2266328, 0.2389095),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(1,2), n=n_max, U=3", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- dca(n, H_counts, N, S, rho, rho^2, 3)
  expected <- c(9.434314, 18.403524, N[3])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 3, details = TRUE)
  expected_details <- list(
    lambda = 9.368826,
    k = -0.4623471,
    v = c(-0.5162169, -0.8564579),
    s = c(0.2386714, 0.3959808),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(1,2), n=n_max, U=1:2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:2)
  expected <- c(N[1:2], 12.83784)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:2, details = TRUE)
  expected_details <- list(
    lambda = 37.89474,
    k = 0.220981,
    v = 1,
    s = 0.220981,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n=n_max, U=c(1,3)", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3))
  expected <- c(N[1], 17.83784, N[3])
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3), details = TRUE)
  expected_details <- list(
    lambda = 13.09091,
    k = 0.3838092,
    v = 1,
    s = 0.3838092,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n=n_max, U=2:3", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  result <- dca(n, H_counts, N, S, rho, rho^2, 2:3)
  expected <- c(7.837838, N[2:3])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2:3, details = TRUE)
  expected_details <- list(
    lambda = 43.10345,
    k = 0.1982834,
    v = 1,
    s = 0.1982834,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca errors for H_counts=c(1,2), n=n_max, U=1:3, too small n", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 42.83784

  expect_error(
    dca(n, H_counts, N, S, rho, rho^2, 1:3),
    "U must be such that n > sum(N[U]) || (n == sum(N[U]) && n == sum(N))"
  )
})

test_that("dca works for H_counts=c(1,2), n>n_max", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(10.06395, 14.63824, 18.29780)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=c(1,2), n>n_max, U=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expected <- c(N[1], 14.66667, 18.33333)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = -1.636364,
    k = 0.3155764,
    v = 1,
    s = 0.3155764,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n>n_max, U=2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expected <- c(9.035355, N[2], 13.964645)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = 16.68177,
    k = -0.3317065,
    v = c(-0.6890984, -0.7246678),
    s = c(0.2285784, 0.2403770),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(1,2), n>n_max, U=3", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- dca(n, H_counts, N, S, rho, rho^2, 3)
  expected <- c(9.477467, 18.522533, N[3])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 3, details = TRUE)
  expected_details <- list(
    lambda = 8.614722,
    k = -0.4651039,
    v = c(-0.5155043, -0.8568870),
    s = c(0.2397631, 0.3985414),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(1,2), n>n_max, U=1:2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:2)
  expected <- c(N[1:2], 12.9999999999999964)
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:2, details = TRUE)
  expected_details <- list(
    lambda = 34.61538,
    k = 0.2237724,
    v = 1,
    s = 0.2237724,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n>n_max, U=c(1,3)", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3))
  expected <- c(N[1], 17.9999999999999964, N[3])
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3), details = TRUE)
  expected_details <- list(
    lambda = 12,
    k = 0.3872983,
    v = 1,
    s = 0.3872983,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n>n_max, U=2:3", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 43

  result <- dca(n, H_counts, N, S, rho, rho^2, 2:3)
  expected <- c(8, N[2:3])
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2:3, details = TRUE)
  expected_details <- list(
    lambda = 39.0625,
    k = 0.2023858,
    v = 1,
    s = 0.2023858,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n=sum(N)", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(10.87450, 15.16689, 18.95861)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=c(1,2), n=sum(N), U=1", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expected <- c(N[1], 15.55556, 19.44444)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, TRUE)
  expected_details <- list(
    lambda = -20.57143,
    k = 0.3347023,
    v = 1,
    s = 0.3347023,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n=sum(N), U=2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expected <- c(9.9999999999999982, N[2], 15.0000000000000036)
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, TRUE)
  expected_details <- list(
    lambda = -1.421085e-14,
    k = -0.3614784,
    v = c(-0.6998542, -0.7142857),
    s = c(0.2529822, 0.2581989),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(1,2), n=sum(N), U=3", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- dca(n, H_counts, N, S, rho, rho^2, 3)
  expected <- c(10, 20.0000000000000036, N[3])
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 3, TRUE)
  expected_details <- list(
    lambda = 0,
    k = -0.4991845,
    v = c(-0.506791, -0.862069),
    s = c(0.2529822, 0.4303315),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(1,2), n=sum(N), U=1:2", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:2)
  expected <- c(N[1:2], 15)
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:2, TRUE)
  expected_details <- list(
    lambda = 2.842171e-14,
    k = 0.2581989,
    v = 1,
    s = 0.2581989,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n=sum(N), U=c(1,3)", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3))
  expected <- c(N[1], 20, N[3])
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3), TRUE)
  expected_details <- list(
    lambda = 1.421085e-14,
    k = 0.4303315,
    v = 1,
    s = 0.4303315,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n=sum(N), U=2:3", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- dca(n, H_counts, N, S, rho, rho^2, 2:3)
  expected <- c(10, N[2:3])
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2:3, TRUE)
  expected_details <- list(
    lambda = -2.842171e-14,
    k = 0.2529822,
    v = 1,
    s = 0.2529822,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(1,2), n=sum(N), U=1:3", {
  H_counts <- c(1, 2)
  N <- c(10, 20, 15)
  S <- c(5, 9, 15)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 45

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:3)
  expect_identical(result, N)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:3, TRUE)
  expect_identical(result_details, N)
})

# H_counts = c(2,2) ----

test_that("dca works for H_counts=c(2,2), n<n_max", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(4.496045, 16.185762, 6.874077, 2.444116)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=c(2,2), n<n_max, U=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expected <- c(N[1], 12.567779, 5.482786, 1.949435)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = 598.7632,
    k = 0.1292552,
    v = c(0.6832790, 0.7301573),
    s = c(0.08831735, 0.09437661),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n<n_max, U=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expected <- c(2.187241, N[2], 5.763511, 2.049248)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = 558.1203,
    k = 0.1135965,
    v = c(0.487104, 0.873344),
    s = c(0.05533330, 0.09920881),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n<n_max, U=3", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- dca(n, H_counts, N, S, rho, rho^2, 3)
  expected <- c(3.1768807, 11.4367705, N[3], 0.3863488)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 3, details = TRUE)
  expected_details <- list(
    lambda = 1093.689,
    k = -0.08251719,
    v = c(-0.9739720, -0.2266683),
    s = c(0.08036943, 0.01870403),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n<n_max, U=4", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 45

  result <- dca(n, H_counts, N, S, rho, rho^2, 4)
  expected <- c(0.9730751, 3.5030703, 0.5238546, N[4])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 4, details = TRUE)
  expected_details <- list(
    lambda = 6217.628,
    k = -0.02621661,
    v = c(-0.9389874, -0.3439516),
    s = c(0.024617069, 0.009017245),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n<n_max, U=1:2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 35

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:2)
  expected <- c(N[1:2], 3.688525, 1.311475)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:2, details = TRUE)
  expected_details <- list(
    lambda = 1004.667,
    k = 0.06349153,
    v = 1,
    s = 0.06349153,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n<n_max, U=c(1,3)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 30

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3))
  expected <- c(N[1], 4.8650024, N[3], 0.1349976)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3), details = TRUE)
  expected_details <- list(
    lambda = 3149.882,
    k = -0.03480683,
    v = c(-0.9822138, -0.1877661),
    s = c(0.034187752, 0.006535545),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n<n_max, U=c(1,4)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 52

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 4))
  expected <- c(N[1], 1.6971531, 0.3028469, N[4])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 4), details = TRUE)
  expected_details <- list(
    lambda = 10919.25,
    k = -0.0130159,
    v = c(-0.9162929, -0.4005088),
    s = c(0.011926377, 0.005212982),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n<n_max, U=2:3", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 42

  result <- dca(n, H_counts, N, S, rho, rho^2, 2:3)
  expected <- c(4.708948, N[2:3], 2.291052)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2:3, details = TRUE)
  expected_details <- list(
    lambda = 175.5651,
    k = -0.1627687,
    v = c(-0.7318854, -0.6814277),
    s = c(0.1191280, 0.1109151),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca errors for H_counts=c(2,2), n<n_max, U=2:4, too small n", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 42

  expect_error(
    dca(n, H_counts, N, S, rho, rho^2, 2:4),
    "U must be such that n > sum(N[U]) || (n == sum(N[U]) && n == sum(N))"
  )
})

test_that("dca errors for H_counts=c(2,2), n<n_max, U=3:4, too small n", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 42

  expect_error(
    dca(n, H_counts, N, S, rho, rho^2, 3:4),
    "U must be such that n > sum(N[U]) || (n == sum(N[U]) && n == sum(N))"
  )
})

test_that("dca works for H_counts=c(2,2), n<n_max, U=1:3", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 50

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:3)
  expected <- c(N[1:3], 4.99999999999999911)
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:3, details = TRUE)
  expected_details <- list(
    lambda = 74.66667,
    k = 0.2420615,
    v = 1,
    s = 0.2420615,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n=n_max", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(6.149733, 22.139037, 19.413013, 6.902405)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=c(2,2), n=n_max, U=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expected <- c(N[1], 19.733242, 18.347419, 6.523527)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = 13.68721,
    k = -0.3449221,
    v = c(-0.4020360, -0.9156239),
    s = c(0.1386711, 0.3158189),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=n_max, U=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expected <- c(9.384822, N[2], 18.604450, 6.614916)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = 10.24224,
    k = -0.3986523,
    v = c(-0.5955548, -0.8033147),
    s = c(0.2374193, 0.3202432),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=n_max, U=3", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- dca(n, H_counts, N, S, rho, rho^2, 3)
  expected <- c(6.020169, 21.672609, N[3], 11.911409)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 3, details = TRUE)
  expected_details <- list(
    lambda = 25.15333,
    k = -0.5964313,
    v = c(-0.2553514, -0.9668483),
    s = c(0.1522996, 0.5766586),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=n_max, U=4", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- dca(n, H_counts, N, S, rho, rho^2, 4)
  expected <- c(2.738359, 9.858092, 2.007737, N[4])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 4, details = TRUE)
  expected_details <- list(
    lambda = 1455.997,
    k = -0.07741758,
    v = c(-0.8948304, -0.4464062),
    s = c(0.06927561, 0.03455969),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=n_max, U=1:2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:2)
  expected <- c(N[1:2], 18.150630, 6.453557)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:2, details = TRUE)
  expected_details <- list(
    lambda = 16.3907,
    k = 0.3124315,
    v = 1,
    s = 0.3124315,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n=n_max, U=c(1,3)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3))
  expected <- c(N[1], 19.40291, N[3], 10.20128)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3), details = TRUE)
  expected_details <- list(
    lambda = 31.15814,
    k = -0.5123439,
    v = c(-0.2661293, -0.9639373),
    s = c(0.1363497, 0.4938674),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=n_max, U=c(1,4)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 4))
  expected <- c(N[1], 3.8496196, 0.7545677, N[4])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 4), details = TRUE)
  expected_details <- list(
    lambda = 4247.76,
    k = -0.03000889,
    v = c(-0.9014784, -0.4328240),
    s = c(0.02705237, 0.01298857),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=n_max, U=2:3", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- dca(n, H_counts, N, S, rho, rho^2, 2:3)
  expected <- c(8.49268, N[2:3], 11.11151)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2:3, details = TRUE)
  expected_details <- list(
    lambda = 27.73196,
    k = -0.579252,
    v = c(-0.3709089, -0.9286693),
    s = c(0.2148497, 0.5379335),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca errors for H_counts=c(2,2), n=n_max, U=2:4, too small n", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  expect_error(
    dca(n, H_counts, N, S, rho, rho^2, 2:4),
    "U must be such that n > sum(N[U]) || (n == sum(N[U]) && n == sum(N))"
  )
})

test_that("dca errors for H_counts=c(2,2), n=n_max, U=3:4, too small n", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  expect_error(
    dca(n, H_counts, N, S, rho, rho^2, 3:4),
    "U must be such that n > sum(N[U]) || (n == sum(N[U]) && n == sum(N))"
  )
})

test_that("dca works for H_counts=c(2,2), n=n_max, U=1:3", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- dca_nmax(H_counts, N, S) # 54.60419

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:3)
  expected <- c(N[1:3], 9.604187)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:3, details = TRUE)
  expected_details <- list(
    lambda = 33.7584,
    k = 0.4649607,
    v = 1,
    s = 0.4649607,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n>n_max", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(6.597173, 23.749824, 29.252215, 10.400788)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=c(2,2), n>n_max, U=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expected <- c(N[1], 21.58247, 28.34080, 10.07673)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = -74.23862,
    k = -0.5108699,
    v = c(-0.2968782, -0.9549153),
    s = c(0.1516662, 0.4878375),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expected <- c(15.539703, N[2], 25.421530, 9.038766)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = -55.7011,
    k = -0.5882444,
    v = c(-0.6683053, -0.7438871),
    s = c(0.3931269, 0.4375874),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=3", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- dca(n, H_counts, N, S, rho, rho^2, 3)
  expected <- c(6.122328, 22.040379, N[3], 26.837293)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 3, details = TRUE)
  expected_details <- list(
    lambda = 5.231608,
    k = -1.308454,
    v = c(-0.1183717, -0.9929694),
    s = c(0.154884, 1.299255),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=4", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- dca(n, H_counts, N, S, rho, rho^2, 4)
  expected <- c(5.019114, 18.068809, 6.912078, N[4])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 4, details = TRUE)
  expected_details <- list(
    lambda = 263.2758,
    k = -0.1740076,
    v = c(-0.7297074, -0.6837596),
    s = c(0.1269746, 0.1189794),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=1:2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:2)
  expected <- c(N[1:2], 29.5082, 10.4918)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:2, details = TRUE)
  expected_details <- list(
    lambda = -80.625,
    k = 0.5079322,
    v = 1,
    s = 0.5079322,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=c(1,3)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3))
  expected <- c(N[1], 19.87602, N[3], 25.12398)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3), details = TRUE)
  expected_details <- list(
    lambda = 6.315779,
    k = -1.224303,
    v = c(-0.1140848, -0.9934710),
    s = c(0.1396744, 1.2163096),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=c(1,4)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 4))
  expected <- c(N[1], 14.477914, 5.522086, N[4])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 4), details = TRUE)
  expected_details <- list(
    lambda = 386.1821,
    k = -0.1392343,
    v = c(-0.7307135, -0.6826843),
    s = c(0.10174041, 0.09505309),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=2:3", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- dca(n, H_counts, N, S, rho, rho^2, 2:3)
  expected <- c(9.621571, N[2:3], 25.378429)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2:3, details = TRUE)
  expected_details <- list(
    lambda = 6.145512,
    k = -1.252507,
    v = c(-0.1943371, -0.9809348),
    s = c(0.2434086, 1.2286279),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=c(2,4)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 80

  result <- dca(n, H_counts, N, S, rho, rho^2, c(2, 4))
  expected <- c(7.644246, N[2], 12.355754, N[4])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(2, 4), details = TRUE)
  expected_details <- list(
    lambda = 48.1521,
    k = -0.2874579,
    v = c(-0.6727449, -0.7398745),
    s = c(0.1933858, 0.2126828),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=3:4", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 80

  result <- dca(n, H_counts, N, S, rho, rho^2, 3:4)
  expected <- c(5.434783, 19.565217, N[3:4])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 3:4, details = TRUE)
  expected_details <- list(
    lambda = 153.75,
    k = 0.1374903,
    v = 1,
    s = 0.1374903,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=1:3", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:3)
  expected <- c(N[1:3], 24.9999999999999964)
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:3, details = TRUE)
  expected_details <- list(
    lambda = 6.4,
    k = 1.210307,
    v = 1,
    s = 1.210307,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=2:4", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 80

  result <- dca(n, H_counts, N, S, rho, rho^2, 2:4)
  expected <- c(5, N[2:4])
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2:4, details = TRUE)
  expected_details <- list(
    lambda = 156.25,
    k = 0.1264911,
    v = 1,
    s = 0.1264911,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=c(1,3,4)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 80

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3, 4))
  expected <- c(N[1], 14.9999999999999982, N[3:4])
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3, 4), details = TRUE)
  expected_details <- list(
    lambda = 337.5,
    k = 0.1054093,
    v = 1,
    s = 0.1054093,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n>n_max, U=c(1,2,4)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 80

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 2, 4))
  expected <- c(N[1:2], 10, N[4])
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 2, 4), details = TRUE)
  expected_details <- list(
    lambda = 112.5,
    k = 0.1721326,
    v = 1,
    s = 0.1721326,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N) # 85

  result <- dca(n, H_counts, N, S, rho, rho^2)
  expected <- c(6.851509, 24.665431, 39.454717, 14.028344)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, details = TRUE)
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

test_that("dca works for H_counts=c(2,2), n=sum(N), U=1", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 1)
  expected <- c(N[1], 22.61987, 38.64108, 13.73905)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1, details = TRUE)
  expected_details <- list(
    lambda = -117.2694,
    k = -0.6838691,
    v = c(-0.2324367, -0.9726115),
    s = c(0.1589562, 0.6651389),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 2)
  expected <- c(22.95252, N[2], 31.01863, 11.02885)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2, details = TRUE)
  expected_details <- list(
    lambda = -88.17468,
    k = -0.7888262,
    v = c(-0.7361037, -0.6768688),
    s = c(0.5806579, 0.5339318),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=3", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 3)
  expected <- c(6.152022, 22.147281, N[3], 41.700697)
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 3, details = TRUE)
  expected_details <- list(
    lambda = -0.435023,
    k = -2.024817,
    v = c(-0.07686387, -0.99704160),
    s = c(0.1556352, 2.0188263),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=4", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 4)
  expected <- c(6.244061, 22.478621, 16.277318, N[4])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 4, details = TRUE)
  expected_details <- list(
    lambda = -17.65625,
    k = -0.3216466,
    v = c(-0.4911093, -0.8710979),
    s = c(0.1579636, 0.2801857),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=1:2", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:2)
  expected <- c(N[1:2], 40.57377, 14.42623)
  expect_equal(result, expected, tolerance = 10^-6)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:2, details = TRUE)
  expected_details <- list(
    lambda = -122.9091,
    k = 0.6984068,
    v = 1,
    s = 0.6984068,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=c(1,3)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3))
  expected <- c(N[1], 20, N[3], 40)
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3), details = TRUE)
  expected_details <- list(
    lambda = 4.440892e-16,
    k = -1.941585,
    v = c(-0.07238708, -0.99737661),
    s = c(0.1405457, 1.9364917),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=c(1,4)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 4))
  expected <- c(N[1], 20, 15.0000000000000036, N[4])
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 4), details = TRUE)
  expected_details <- list(
    lambda = -1.421085e-14,
    k = -0.2939724,
    v = c(-0.4780914, -0.8783101),
    s = c(0.1405457, 0.2581989),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=2:3", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 2:3)
  expected <- c(9.9999999999999982, N[2:3], 40)
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2:3, details = TRUE)
  expected_details <- list(
    lambda = 0,
    k = -1.952946,
    v = c(-0.1295387, -0.9915744),
    s = c(0.2529822, 1.9364917),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=c(2,4)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, c(2, 4))
  expected <- c(9.9999999999999982, N[2], 15.0000000000000036, N[4])
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(2, 4), details = TRUE)
  expected_details <- list(
    lambda = -1.421085e-14,
    k = -0.3614784,
    v = c(-0.6998542, -0.7142857),
    s = c(0.2529822, 0.2581989),
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(2L, 2L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=3:4", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 3:4)
  expected <- c(6.521739, 23.478261, N[3:4])
  expect_equal(result, expected, tolerance = 10^-7)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 3:4, details = TRUE)
  expected_details <- list(
    lambda = -66.66667,
    k = 0.1649884,
    v = 1,
    s = 0.1649884,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=1:3", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- 70

  result <- dca(n, H_counts, N, S, rho, rho^2, 1:3)
  expected <- c(N[1:3], 24.9999999999999964)
  expect_equal(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 1:3, details = TRUE)
  expected_details <- list(
    lambda = 6.4,
    k = 1.210307,
    v = 1,
    s = 1.210307,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=2:4", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, 2:4)
  expected <- c(10, N[2:4])
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, 2:4, details = TRUE)
  expected_details <- list(
    lambda = -2.842171e-14,
    k = 0.2529822,
    v = 1,
    s = 0.2529822,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=c(1,3,4)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3, 4))
  expected <- c(N[1], 20, N[3:4])
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 3, 4), details = TRUE)
  expected_details <- list(
    lambda = 0,
    k = 0.1405457,
    v = 1,
    s = 0.1405457,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

test_that("dca works for H_counts=c(2,2), n=sum(N), U=c(1,2,4)", {
  H_counts <- c(2, 2)
  N <- c(10, 20, 15, 40)
  S <- c(5, 9, 15, 2)
  rho <- c(2, 5) * sqrt(c(0.4, 0.6))
  n <- sum(N)

  result <- dca(n, H_counts, N, S, rho, rho^2, c(1, 2, 4))
  expected <- c(N[1:2], 15, N[4])
  expect_identical(result, expected)

  # Check details.
  result_details <- dca(n, H_counts, N, S, rho, rho^2, c(1, 2, 4), details = TRUE)
  expected_details <- list(
    lambda = 2.842171e-14,
    k = 0.2581989,
    v = 1,
    s = 0.2581989,
    x = expected
  )
  elements <- names(expected_details)
  expect_equal(result_details[elements], expected_details, tolerance = 10^-6)

  # Check details: D.matrix.
  expect_identical(dim(result_details$D.matrix), c(1L, 1L))
})

# pop9d278s ----

test_that("dca works for pop9d278s, n<n_max", {
  p <- pop9d278s
  n <- 1000

  result <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2)
  expect_snapshot(result)

  # Check details.
  result_details <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2, details = TRUE)
  expect_snapshot(result_details)
})

test_that("dca works for pop9d278s, n<n_max, U", {
  p <- pop9d278s
  n <- 1500
  U <- c(1, 30, 60, 90, 34, 100, 120)

  result <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2, U)
  expect_snapshot(result)

  # Check details.
  result_details <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2, U, details = TRUE)
  expect_snapshot(result_details)
})

test_that("dca works for pop9d278s, n=n_max", {
  p <- pop9d278s
  n <- p$n_max

  result <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2)
  expect_snapshot(result)

  # Check details.
  result_details <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2, details = TRUE)
  expect_snapshot(result_details)
})

test_that("dca works for pop9d278s, n>n_max", {
  p <- pop9d278s
  n <- 20000

  result <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2)
  expect_snapshot(result)

  # Check details.
  result_details <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2, details = TRUE)
  expect_snapshot(result_details)
})

test_that("dca works for pop9d278s, n>n_max, U (domain blockage)", {
  p <- pop9d278s
  n <- 20000
  U <- 209:238

  result <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2, U)
  expect_snapshot(result)

  # Check details.
  result_details <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2, U, details = TRUE)
  expect_snapshot(result_details)
})

test_that("dca works for pop9d278s, n=sum(N)", {
  p <- pop9d278s
  n <- sum(p$N)

  result <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2)
  expect_snapshot(result)

  # Check details.
  result_details <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2, details = TRUE)
  expect_snapshot(result_details)
})

test_that("dca works for pop9d278s, n=sum(N), U", {
  p <- pop9d278s
  n <- sum(p$N)
  U <- 1:13

  result <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2, U)
  expect_snapshot(result)

  # Check details.
  result_details <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2, U, details = TRUE)
  expect_snapshot(result_details)
})

test_that("dca works for pop9d278s, n=sum(N), U (domain blockage)", {
  p <- pop9d278s
  n <- sum(p$N)
  U <- 239:278

  result <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2, U)
  expect_snapshot(result)

  # Check details.
  result_details <- dca(n, p$H_counts, p$N, p$S, p$rho, p$rho2, U, details = TRUE)
  expect_snapshot(result_details)
})
