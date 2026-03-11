p <- pop2d4s

# ASSERTIONS ----

## function ----

test_that("dopt is valid function", {
  expect_function(dopt, args = c("n", "H_counts", "N", "S", "total", "kappa", "return_T"))
})

## n ----

test_that("dopt throws error when n is not valid", {
  expect_error(
    dopt(-1, p$H_counts, p$N, p$S, p$total, p$kappa),
    "n"
  )
  expect_error(
    dopt(0, p$H_counts, p$N, p$S, p$total, p$kappa),
    "n"
  )
  expect_error(
    dopt(sum(p$N) + 1, p$H_counts, p$N, p$S, p$total, p$kappa),
    "n"
  )
  expect_error(
    dopt(c(1, 1), p$H_counts, p$N, p$S, p$total, p$kappa),
    "n"
  )
  expect_error(
    dopt(c(1, NA), p$H_counts, p$N, p$S, p$total, p$kappa),
    "'n'"
  )
  expect_error(
    dopt(NA, p$H_counts, p$N, p$S, p$total, p$kappa),
    "'n'"
  )
  expect_error(
    dopt(NULL, p$H_counts, p$N, p$S, p$total, p$kappa),
    "'n'"
  )
  expect_error(
    dopt(c(1, Inf), p$H_counts, p$N, p$S, p$total, p$kappa),
    "'n'"
  )
  expect_error(
    dopt(Inf, p$H_counts, p$N, p$S, p$total, p$kappa),
    "'n'"
  )
})

## H_counts ----

test_that("dopt throws error when H_counts is not valid", {
  expect_error(
    dopt(350, c(-2, 6), p$N, p$S, p$total, p$kappa),
    "H_counts"
  )
  expect_error(
    dopt(350, c(4, 0), p$N, p$S, p$total, p$kappa),
    "H_counts"
  )
  expect_error(
    dopt(350, c(1.5, 2.5), p$N, p$S, p$total, p$kappa),
    "'H_counts'"
  )
  expect_error(
    dopt(350, c(Inf, 6), p$N, p$S, p$total, p$kappa),
    "'H_counts'"
  )
  expect_error(
    dopt(350, c(4, NA), p$N, p$S, p$total, p$kappa),
    "'H_counts'"
  )
  expect_error(
    dopt(350, NA, p$N, p$S, p$total, p$kappa),
    "'H_counts'"
  )
  expect_error(
    dopt(350, integer(), p$N, p$S, p$total, p$kappa),
    "'H_counts'"
  )
  expect_error(
    dopt(350, NULL, p$N, p$S, p$total, p$kappa),
    "'H_counts'"
  )
})

## N ----

test_that("dopt throws error when N is not valid", {
  expect_error(
    dopt(350, p$H_counts, c(p$N, 4), p$S, p$total, p$kappa),
    "N"
  )
  expect_error(
    dopt(350, p$H_counts, p$N[-1], p$S, p$total, p$kappa),
    "'N'"
  )
  expect_error(
    dopt(350, p$H_counts, c(-1, 2, 3, 4), p$S, p$total, p$kappa),
    "N"
  )
  expect_error(
    dopt(350, p$H_counts, c(1, 0, 3, 4), p$S, p$total, p$kappa),
    "N"
  )
  expect_error(
    dopt(350, p$H_counts, c(1, 2, 3.5, 4), p$S, p$total, p$kappa),
    "'N'"
  )
  expect_error(
    dopt(350, p$H_counts, c(1, 2, 3, Inf), p$S, p$total, p$kappa),
    "'N'"
  )
  expect_error(
    dopt(350, p$H_counts, c(1, NA, 3, 4), p$S, p$total, p$kappa),
    "'N'"
  )
  expect_error(
    dopt(350, p$H_counts, c(NA, NA, NA, NA), p$S, p$total, p$kappa),
    "'N'"
  )
  expect_error(
    dopt(350, p$H_counts, NULL, p$S, p$total, p$kappa),
    "'N'"
  )
})

## S ----

test_that("dopt throws error when S is not valid", {
  expect_error(
    dopt(350, p$H_counts, p$N, c(p$S, 4), p$total, p$kappa),
    "'S'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S[-1], p$total, p$kappa),
    "'S'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, c(-1, 2, 3, 4), p$total, p$kappa),
    "S"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, c(1, 0, 3, 4), p$total, p$kappa),
    "S"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, c(1, 2, 3, Inf), p$total, p$kappa),
    "'S'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, c(1, NA, 3, 4), p$total, p$kappa),
    "'S'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, c(NA, NA, NA, NA), p$total, p$kappa),
    "'S'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, NULL, p$total, p$kappa),
    "'S'"
  )
})

## total  ----

test_that("dopt throws error when total is not valid", {
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, c(p$total, 4), p$kappa),
    "'total'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total[-1], p$kappa),
    "'total'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, c(-1, 2), p$kappa),
    "total"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, c(1, 0), p$kappa),
    "total"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, c(1, Inf), p$kappa),
    "'total'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, c(1, NA), p$kappa),
    "'total'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, c(NA, NA), p$kappa),
    "'total'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, NULL, p$kappa),
    "'total'"
  )
})

## kappa  ----

test_that("dopt throws error when kappa is not valid", {
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, c(p$kappa, 4)),
    "'kappa'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, p$kappa[-1]),
    "'kappa'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, c(-1, 2)),
    "kappa"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, c(1, 0)),
    "kappa"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, c(1, Inf)),
    "'kappa'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, c(1, NA)),
    "'kappa'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, c(NA, NA)),
    "'kappa'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, NULL),
    "'kappa'"
  )
})

## n ----

test_that("dopt throws error when n is not valid", {
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, p$kappa, -1),
    "'return_T'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, p$kappa, 0),
    "'return_T'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, p$kappa, 1),
    "'return_T'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, p$kappa, c(0, 1)),
    "'return_T'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, p$kappa, c(1, NA)),
    "'return_T'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, p$kappa, NA),
    "'return_T'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, p$kappa, Inf),
    "'return_T'"
  )
  expect_error(
    dopt(350, p$H_counts, p$N, p$S, p$total, p$kappa, NULL),
    "'return_T'"
  )
})

# OPTIMUM ALLOCATION ----

## H_counts = 1 ----

test_that("dopt works for H_counts=1, n<sum(N)", {
  H_counts <- 1
  N <- 10
  S <- 5
  total <- 3
  kappa <- 1
  n <- 3

  expect_identical(dopt(n, H_counts, N, S, total, kappa), n)

  # with return_T = TRUE
  result_T <- dopt(n, H_counts, N, S, total, kappa, TRUE)
  expect_equal(result_T$xopt, n)
  expect_equal(result_T$Topt, 64.81481, tolerance = 10^-7)
})

test_that("dopt works for H_counts=1, n=sum(N)", {
  H_counts <- 1
  N <- 10
  S <- 5
  total <- 3
  kappa <- 1
  n <- N

  expect_equal(
    dopt(n, H_counts, N, S, total, kappa), n,
    tolerance = 10^-14
  )

  # with return_T = TRUE
  result_T <- dopt(n, H_counts, N, S, total, kappa, TRUE)
  expect_equal(result_T$xopt, n, tolerance = 10^-14)
  expect_equal(result_T$Topt, 0, tolerance = 10^-14)
})

## 3d7s ----

test_that("dopt works for 3 domains and 7 strata, n<sum(N)", {
  H_counts <- c(2, 2, 3)
  N <- c(140, 110, 135, 190, 200, 40, 70)
  S <- c(180, 20, 5, 4, 35, 9, 40)
  total <- c(2, 3, 5)
  kappa <- c(0.5, 0.2, 0.3)
  n <- 700

  result <- dopt(n, H_counts, N, S, total, kappa)
  expected <- c(140, 98.743346, 88.625916, 99.786217, 192.922779, 9.921743, 70)
  expect_equal(result, expected, tolerance = 10^-8)

  # with return_T = TRUE
  result_T <- dopt(n, H_counts, N, S, total, kappa, TRUE)
  expect_equal(result_T$xopt, expected, tolerance = 10^-8)
  expect_equal(result_T$Topt, 2507.981, tolerance = 10^-6)
})

test_that("dopt works for 3 domains and 7 strata, n=sum(N)", {
  H_counts <- c(2, 2, 3)
  N <- c(140, 110, 135, 190, 200, 40, 70)
  S <- c(180, 20, 5, 4, 35, 9, 40)
  total <- c(2, 3, 5)
  kappa <- c(0.5, 0.2, 0.3)
  n <- sum(N)

  result <- dopt(n, H_counts, N, S, total, kappa)
  expect_identical(result, N)

  # with return_T = TRUE
  result_T <- dopt(n, H_counts, N, S, total, kappa, TRUE)
  expect_identical(result, N)
  expect_identical(result_T$Topt, 0)
})

## pop2d4s ----

test_that("dopt works for pop2d4s, n<sum(N)", {
  p <- pop2d4s

  result <- dopt(500, p$H_counts, p$N, p$S, p$total, p$kappa)
  expected <- c(140, 104.5738, 113.0793, 142.3469)
  expect_equal(result, expected, tolerance = 10^-7)

  # with return_T = TRUE
  result_T <- dopt(500, p$H_counts, p$N, p$S, p$total, p$kappa, TRUE)
  expect_equal(result_T$xopt, expected, tolerance = 10^-7)
  expect_equal(result_T$Topt, 71.34697, tolerance = 10^-7)
})

test_that("dopt works for pop2d4s, n=sum(N)", {
  p <- pop2d4s

  result <- dopt(n = sum(p$N), p$H_counts, p$N, p$S, p$total, p$kappa)
  expected <- c(140, 110, 135, 190)
  expect_identical(result, expected)

  # with return_T = TRUE
  result_T <- dopt(n = sum(p$N), p$H_counts, p$N, p$S, p$total, p$kappa, TRUE)
  expect_identical(result_T$xopt, expected)
  expect_identical(result_T$Topt, 0)
})
