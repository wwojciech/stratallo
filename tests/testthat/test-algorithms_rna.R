# Function ----

test_that("rna is valid function", {
  expect_function(
    rna,
    args = c("total_cost", "A", "bounds", "unit_costs", "check_violations", "details")
  )
})

# NO BOUNDS ----

## H = 1 ----

test_that("rna works well when no bounds, H = 1", {
  n <- 400
  result <- rna(n, A[1])
  expect_identical(result, n)

  # Check details.
  result_details <- rna(n, A[1], details = TRUE)
  expected_details <- list(
    opt = n, take_neyman = 1L, take_bound = integer(0), s0 = n / A[1], s = n / A[1], iter = 1L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well when no bounds, H = 1, non-def cost", {
  tcost <- 400
  result <- rna(tcost, A[1], unit_costs = ucosts[1])
  expect_equal(result, tcost / ucosts[1])

  # Check details.
  result_details <- rna(tcost, A[1], unit_costs = ucosts[1], details = TRUE)
  s <- sfun(tcost, A[1], ucosts = ucosts[1])
  expected_details <- list(
    opt = tcost / ucosts[1], take_neyman = 1L, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_equal(result_details, expected_details)
})

## H = 4 ----

test_that("rna works well when no bounds, H = 4)", {
  n <- 400
  result <- rna(n, A)

  s <- n / sum(A)
  expected <- A * s
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(n, A, details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = 1:4, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well when no bounds, H = 4, non-def cost)", {
  tcost <- 400
  result <- rna(tcost, A, unit_costs = ucosts[1])

  s <- sfun(tcost, A, ucosts = ucosts[1])
  expected <- A / sqrt(ucosts[1]) * s
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, unit_costs = ucosts[1], details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = 1:4, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well when no bounds, H = 4, non-def cost vec)", {
  n <- 400
  result <- rna(n, A, unit_costs = ucosts)

  s <- sfun(n, A, ucosts = ucosts)
  expected <- A / sqrt(ucosts) * s
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(n, A, unit_costs = ucosts, details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = 1:4, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_identical(result_details, expected_details)
})

# LOWER ----

## pop507 test ----

test_that("rna works well for m, (pop507)", {
  frac <- c(1, 1.05, 1.1, seq(1.3, 2, .3), seq(2.1, 2.9, 0.5), seq(3, 300, 50), seq(301, 10000, 1500), 15000, 25000)
  n <- frac * sum(N_pop507)
  result <- lapply(n, rna, A_pop507, N_pop507, check_violations = leq)
  expect_snapshot(result)

  # Check details.
  result_details <- lapply(
    n, rna, A_pop507, N_pop507,
    check_violations = leq, details = TRUE
  )
  expect_snapshot(result_details)
})

test_that("rna works well for m, (pop507), non-def cost", {
  frac <- c(1, 1.05, 1.1, seq(1.3, 2, .3), seq(2.1, 2.9, 0.5), seq(3, 300, 50), seq(301, 10000, 1500), 15000, 25000)
  tcost <- frac * sum(ucosts_pop507[1] * N_pop507)
  result <- lapply(
    tcost, rna, A_pop507, N_pop507, ucosts_pop507[1], leq
  )
  expect_snapshot(result)

  # Check details.
  result_details <- lapply(
    tcost, rna, A_pop507, N_pop507, ucosts_pop507[1], leq,
    details = TRUE
  )
  expect_snapshot(result_details)
})

test_that("rna works well for m, (pop507), non-def cost vec", {
  frac <- c(1, 1.05, 1.1, seq(1.3, 2, .3), seq(2.1, 2.9, 0.5), seq(3, 300, 50), seq(301, 10000, 1500), 15000, 25000)
  tcost <- frac * sum(ucosts_pop507 * N_pop507)
  result <- lapply(
    tcost, rna, A_pop507, N_pop507,
    unit_costs = ucosts_pop507, check_violations = leq
  )
  expect_snapshot(result)

  # Check details.
  result_details <- lapply(
    tcost, rna, A_pop507, N_pop507, ucosts_pop507, leq, TRUE
  )
  expect_snapshot(result_details)
})

## H = 1 ----

### alloc: no bound ----

test_that("rna works well for m (alloc: no bound), H = 1", {
  n <- 101
  result <- rna(n, A[1], bounds[1], check_violations = leq)
  expect_equal(result, n) # 100.99999999999999

  # Check details.
  result_details <- rna(n, A[1], bounds[1], check_violations = leq, details = TRUE)
  s <- n / A[1]
  expected_details <- list(
    opt = n, take_neyman = 1L, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_equal(result_details, expected_details)
})

test_that("rna works well for m (alloc: no bound), H = 1, non-def cost", {
  tcost <- 101
  result <- rna(tcost, A[1], bounds[1], ucosts[1], leq)
  expected <- tcost / ucosts[1]
  expect_equal(result, expected)

  # Check details.
  result_details <- rna(tcost, A[1], bounds[1], ucosts[1], leq, TRUE)
  s <- sfun(tcost, A[1], ucosts = ucosts[1])
  expected_details <- list(
    opt = expected, take_neyman = 1L, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_equal(result_details, expected_details)
})

### alloc: 1 bound (total_cost = c * m) ----

test_that("rna works well for m (alloc: 1 bound), H = 1", {
  n <- bounds[1]
  result <- rna(n, A[1], bounds[1], check_violations = leq)
  expect_identical(result, n)

  # Check details.
  result_details <- rna(n, A[1], bounds[1], check_violations = leq, details = TRUE)
  expected_details <- list(
    opt = n, take_neyman = integer(0), take_bound = 1L, s0 = n / A[1], s = NaN, iter = 2L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for m (alloc: 1 bound), H = 1, non-def cost", {
  tcost <- ucosts[1] * bounds[1]
  result <- rna(tcost, A[1], bounds[1], ucosts[1], leq)
  expect_identical(result, bounds[1])

  # Check details.
  result_details <- rna(tcost, A[1], bounds[1], ucosts[1], leq, TRUE)
  s0 <- sfun(tcost, A[1], ucosts = ucosts[1])
  expected_details <- list(
    opt = bounds[1], take_neyman = integer(0), take_bound = 1L, s0 = s0, s = NaN, iter = 2L
  )
  expect_identical(result_details, expected_details)
})

## H = 4 ----

### alloc: no bounds ----

test_that("rna works well for m (alloc: no bounds), H = 4", {
  n <- 600
  result <- rna(n, A, bounds, check_violations = leq)

  s <- n / sum(A)
  expected <- A * s
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(n, A, bounds, check_violations = leq, details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = 1:4, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for m (alloc: no bounds), H = 4, non-def cost", {
  tcost <- 600
  result <- rna(tcost, A, bounds, ucosts[1], leq)

  s <- sfun(tcost, A, ucosts = ucosts[1])
  expected <- A / sqrt(ucosts[1]) * s
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts[1], leq, TRUE)
  expected_details <- list(
    opt = expected, take_neyman = 1:4, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for m (alloc: no bounds), H = 4, non-def cost vec", {
  tcost <- 3000
  result <- rna(tcost, A, bounds, ucosts, leq)

  s <- sfun(tcost, A, ucosts = ucosts)
  expected <- A / sqrt(ucosts) * s
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts, leq, TRUE)
  expected_details <- list(
    opt = expected, take_neyman = 1:4, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_identical(result_details, expected_details)
})

### alloc: 1 bound ----

test_that("rna works well for m (alloc: 1 bound), H = 4", {
  n <- 500
  result <- rna(n, A, bounds, check_violations = leq)

  L <- 4L
  Lc <- 1:3
  s0 <- n / sum(A)
  s <- sfun(n, A, bounds, R = L)
  expected <- c(A[Lc] * s, bounds[L])
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(n, A, bounds, check_violations = leq, details = TRUE)
  s0 <-
    expected_details <- list(
      opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 2L
    )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for m (alloc: 1 bound), H = 4, non-def cost", {
  tcost <- 350
  result <- rna(tcost, A, bounds, ucosts[1], leq)

  L <- 4L
  Lc <- 1:3
  s0 <- sfun(tcost, A, ucosts = ucosts[1])
  s <- sfun(tcost, A, bounds, ucosts[1], L)
  expected <- c(A[Lc] / sqrt(ucosts[1]) * s, bounds[4])
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts[1], leq, TRUE)
  s0 <-
    expected_details <- list(
      opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 2L
    )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for m (alloc: 1 bound), H = 4, non-def cost vec", {
  tcost <- 2400
  result <- rna(tcost, A, bounds, ucosts, leq)

  L <- 2L
  Lc <- c(1L, 3L, 4L)
  s0 <- sfun(tcost, A, ucosts = ucosts)
  s <- sfun(tcost, A, bounds, ucosts, L)
  expected <- c(A[1] / sqrt(ucosts[1]) * s, bounds[2], A[3:4] / sqrt(ucosts[3:4]) * s)
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts, leq, TRUE)
  expected_details <- list(
    opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 2L
  )
  expect_identical(result_details, expected_details)
})

### alloc: 2 bounds ----

test_that("rna works well for m (alloc: 2 bounds), H = 4", {
  n <- 420
  result <- rna(n, A, bounds, check_violations = leq)

  L <- c(1L, 4L)
  Lc <- 2:3
  s0 <- n / sum(A)
  s <- sfun(n, A, bounds, R = L)
  expected <- c(bounds[1], A[Lc] * s, bounds[4])
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(n, A, bounds, check_violations = leq, details = TRUE)
  s0 <-
    expected_details <- list(
      opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 2L
    )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for m (alloc: 2 bounds), H = 4, non-def cost", {
  tcost <- 300
  result <- rna(tcost, A, bounds, ucosts[1], leq)

  L <- c(1L, 4L)
  Lc <- c(2L, 3L)
  s0 <- sfun(tcost, A, ucosts = ucosts[1])
  s <- sfun(tcost, A, bounds, ucosts[1], L)
  expected <- c(bounds[1], A[Lc] / sqrt(ucosts[1]) * s, bounds[4])
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts[1], leq, TRUE)
  s0 <-
    expected_details <- list(
      opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 2L
    )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for m (alloc: 2 bounds), H = 4, non-def cost vec", {
  tcost <- 2000
  result <- rna(tcost, A, bounds, ucosts, leq)

  L <- c(2L, 4L)
  Lc <- c(1L, 3L)
  s0 <- sfun(tcost, A, ucosts = ucosts)
  s <- sfun(tcost, A, bounds, ucosts, L)
  expected <- c(A[1] / sqrt(ucosts[1]) * s, bounds[2], A[3] / sqrt(ucosts[3]) * s, bounds[4])
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, unit_costs = ucosts, leq, TRUE)
  expected_details <- list(
    opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 3L
  )
  expect_identical(result_details, expected_details)
})

### alloc: 3 bounds ----

test_that("rna works well for m (alloc: 3 bounds), H = 4", {
  n <- 370
  result <- rna(n, A, bounds, check_violations = leq)

  L <- c(1L, 2L, 4L)
  Lc <- 3L
  s0 <- n / sum(A)
  s <- sfun(n, A, bounds, R = L)
  expected <- c(bounds[1:2], A[3] * s, bounds[4])
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(n, A, bounds, check_violations = leq, details = TRUE)
  s0 <-
    expected_details <- list(
      opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 3L
    )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for m (alloc: 3 bounds), H = 4, non-def cost", {
  tcost <- 250
  result <- rna(tcost, A, bounds, ucosts[1], leq)

  L <- c(1L, 2L, 4L)
  Lc <- 3L
  s0 <- sfun(tcost, A, ucosts = ucosts[1])
  s <- sfun(tcost, A, bounds, ucosts[1], L)
  expected <- c(bounds[1:2], A[3] / sqrt(ucosts[1]) * s, bounds[4])
  expect_equal(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts[1], leq, TRUE)
  s0 <-
    expected_details <- list(
      opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 3L
    )
  expect_equal(result_details, expected_details)
})

test_that("rna works well for m (alloc: 3 bounds), H = 4, non-def cost vec", {
  tcost <- 1850
  result <- rna(tcost, A, bounds, ucosts, leq)

  L <- c(1L, 2L, 4L)
  Lc <- 3L
  s0 <- sfun(tcost, A, ucosts = ucosts)
  s <- sfun(tcost, A, bounds, ucosts, L)
  expected <- c(bounds[1:2], A[3] / sqrt(ucosts[3]) * s, bounds[4])
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts, leq, TRUE)
  expected_details <- list(
    opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 3L
  )
  expect_identical(result_details, expected_details)
})

### alloc: 4 bounds (total_cost = sum(c * m)) ----

test_that("rna works well for n = sum(m), H = 4", {
  n <- sum(bounds)
  result <- rna(n, A, bounds, check_violations = leq)
  expect_identical(result, bounds)

  # Check details.
  result_details <- rna(n, A, bounds, check_violations = leq, details = TRUE)
  expected_details <- list(
    opt = bounds, take_neyman = integer(0), take_bound = 1:4, s0 = n / sum(A), s = NaN, iter = 4L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for n = sum(c*m), H = 4, non-def cost", {
  tcost <- sum(bounds * ucosts[2])
  result <- rna(tcost, A, bounds, ucosts[2], leq)
  expect_identical(result, bounds)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts[2], leq, TRUE)
  s0 <- sfun(tcost, A, ucosts = ucosts[2])
  expected_details <- list(
    opt = bounds, take_neyman = integer(0), take_bound = 1:4, s0 = s0, s = NaN, iter = 4L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for n = sum(c*m), H = 4, non-def cost vec", {
  tcost <- sum(bounds * ucosts)
  result <- rna(tcost, A, bounds, ucosts, leq)
  expect_identical(result, bounds)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts, leq, TRUE)
  s0 <- sfun(tcost, A, ucosts = ucosts)
  expected_details <- list(
    opt = bounds, take_neyman = integer(0), take_bound = 1:4, s0 = s0, s = NaN, iter = 4L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for n = sum(c*m), H = 4, non-def cost (finite prec. issue)", {
  tcost <- sum(bounds * ucosts[1])
  result <- rna(tcost, A, bounds, ucosts[1], leq)
  expected <- c(bounds[1:2], 70.00000000000001421085, bounds[4])
  expect_identical(result, expected)
  # note: x_3 != 70 due to c_3 = ucosts[1] = 0.7 stored as 0.6999999999999999555911

  # Check details.
  L <- c(1L, 2L, 4L)
  Lc <- 3L
  s0 <- sfun(tcost, A, ucosts = ucosts[1])
  s <- sfun(tcost, A, bounds, ucosts[1], L)
  # 0.01171324037147705721118
  result_details <- rna(tcost, A, bounds, ucosts[1], leq, TRUE)
  expected_details <- list(
    opt = expected,
    take_neyman = Lc,
    take_bound = L,
    s0 = s0,
    s = 0.0117132403714770589459,
    iter = 3L
  )
  expect_equal(result_details, expected_details)
})

# UPPER ----

## pop507 test ----

test_that("rna works well for M, (pop507)", {
  frac <- c(0.00001, 0.0001, 0.001, seq(0.01, 0.99, 0.05), 1) # nolintr
  tcost <- frac * sum(N_pop507)
  result <- lapply(tcost, rna, A_pop507, N_pop507)
  expect_snapshot(result)

  # Check details.
  result_details <- lapply(tcost, rna, A_pop507, N_pop507, details = TRUE)
  expect_snapshot(result_details)
})

test_that("rna works well for M, (pop507), non-def cost", {
  frac <- c(0.00001, 0.0001, 0.001, seq(0.01, 0.99, 0.05), 1)
  tcost <- frac * sum(ucosts_pop507[1] * N_pop507) # nolintr
  result <- lapply(tcost, rna, A_pop507, N_pop507, ucosts_pop507[1])
  expect_snapshot(result)

  # Check details.
  result_details <- lapply(tcost, rna, A_pop507, N_pop507, ucosts_pop507[1], details = TRUE)
  expect_snapshot(result_details)
})

test_that("rna works well for M, (pop507), non-def cost vec", {
  frac <- c(0.00001, 0.0001, 0.001, seq(0.01, 0.99, 0.05), 1)
  tcost <- frac * sum(ucosts_pop507 * N_pop507) # nolintr
  result <- lapply(tcost, rna, A_pop507, N_pop507, ucosts_pop507)
  expect_snapshot(result)

  # Check details.
  result_details <- lapply(tcost, rna, A_pop507, N_pop507, ucosts_pop507, details = TRUE)
  expect_snapshot(result_details)
})

## H = 1 ----

### alloc: no bound ----

test_that("rna works well for M (alloc: no bound), H = 1", {
  n <- 99
  result <- rna(n, A[1], bounds[1])
  expect_equal(result, n) # 100.99999999999999

  # Check details.
  result_details <- rna(n, A[1], bounds[1], details = TRUE)
  s <- n / A[1]
  expected_details <- list(
    opt = n, take_neyman = 1L, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_equal(result_details, expected_details)
})

test_that("rna works well for M (alloc: no bound), H = 1, non-def cost", {
  tcost <- 60
  result <- rna(tcost, A[1], bounds[1], ucosts[1])
  expected <- tcost / ucosts[1]
  expect_equal(result, expected)

  # Check details.
  result_details <- rna(tcost, A[1], bounds[1], ucosts[1], details = TRUE)
  s <- sfun(tcost, A[1], ucosts = ucosts[1])
  expected_details <- list(
    opt = expected, take_neyman = 1L, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_equal(result_details, expected_details)
})

### alloc: 1 bound (total_cost = c * M) ----

test_that("rna works well for M (alloc: 1 bound), H = 1", {
  n <- bounds[1]
  result <- rna(n, A[1], bounds[1])
  expect_identical(result, n)

  # Check details.
  result_details <- rna(n, A[1], bounds[1], details = TRUE)
  expected_details <- list(
    opt = n, take_neyman = integer(0), take_bound = 1L, s0 = n / A[1], s = NaN, iter = 2L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for M (alloc: 1 bound), H = 1, non-def cost", {
  tcost <- ucosts[3] * bounds[3]
  result <- rna(tcost, A[3], bounds[3], ucosts[3])
  expect_identical(result, bounds[3])

  # Check details.
  result_details <- rna(tcost, A[3], bounds[3], ucosts[3], details = TRUE)
  s0 <- sfun(tcost, A[3], ucosts = ucosts[3])
  expected_details <- list(
    opt = bounds[3], take_neyman = integer(0), take_bound = 1L, s0 = s0, s = NaN, iter = 2L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for M (alloc: 1 bound), H = 1, non-def cost (finite prec. issue)", {
  tcost <- ucosts[1] * bounds[1]
  result <- rna(tcost, A[1], bounds[1], ucosts[1])
  expect_equal(result, 99.99999999999997157829) # powinno byc bounds[1] = 100

  # Check details.
  result_details <- rna(tcost, A[1], bounds[1], ucosts[1], details = TRUE)
  s0 <- sfun(tcost, A[1], ucosts = ucosts[1])
  expected_details <- list(
    opt = 99.99999999999997157829, take_neyman = 1L, take_bound = integer(0), s0 = s0, s = s0, iter = 1L
  )
  expect_equal(result_details, expected_details)
})

## H = 4 ----

### M with Inf ----

test_that("rna works well for M with Inf, H = 4", {
  n <- 5000
  result <- rna(n, A, c(bounds[1:3], Inf))

  L <- 1:3
  s <- sfun(n, A, c(bounds[1:3], Inf), R = L)
  expected <- c(bounds[1:3], A[4] * s)
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(n, A, c(bounds[1:3], Inf), details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = 4L, take_bound = L, s0 = n / sum(A), s = s, iter = 2L
  )
  expect_identical(result_details, expected_details, tolerance = 1e-6)
})

test_that("rna works well for M with Inf, H = 4, non-def cost", {
  tcost <- 5000
  result <- rna(tcost, A, c(bounds[1:3], Inf), ucosts[1])

  L <- 1:3
  s0 <- sfun(tcost, A, c(bounds[1:3], Inf), ucosts[1])
  s <- sfun(tcost, A, c(bounds[1:3], Inf), ucosts[1], L)
  expected <- c(bounds[1:3], A[4] / sqrt(ucosts[1]) * s)
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, c(bounds[1:3], Inf), ucosts[1], details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = 4L, take_bound = L, s0 = s0, s = s, iter = 2L
  )
  expect_identical(result_details, expected_details, tolerance = 1e-6)
})

test_that("rna works well for M with Inf, H = 4, non-def cost vec", {
  tcost <- 5000
  result <- rna(tcost, A, c(bounds[1:3], Inf), ucosts)

  L <- 1:3
  s0 <- sfun(tcost, A, c(bounds[1:3], Inf), ucosts)
  s <- sfun(tcost, A, c(bounds[1:3], Inf), ucosts, L)
  expected <- c(bounds[1:3], A[4] / sqrt(ucosts[4]) * s)
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, c(bounds[1:3], Inf), ucosts, details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = 4L, take_bound = L, s0 = s0, s = s, iter = 2L
  )
  expect_identical(result_details, expected_details, tolerance = 1e-6)
})

### alloc: no bounds ----

test_that("rna works well for M (alloc: no bounds), H = 4", {
  n <- 190
  result <- rna(n, A, bounds)

  s <- n / sum(A)
  expected <- A * s
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(n, A, bounds, details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = 1:4, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for M (alloc: no bounds), H = 4, non-def cost", {
  tcost <- 100
  result <- rna(tcost, A, bounds, ucosts[1])

  s <- sfun(tcost, A, ucosts = ucosts[1])
  expected <- A / sqrt(ucosts[1]) * s
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts[1], details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = 1:4, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for m (alloc: no bounds), H = 4, non-def cost vec", {
  tcost <- 120
  result <- rna(tcost, A, bounds, ucosts)

  s <- sfun(tcost, A, ucosts = ucosts)
  expected <- A / sqrt(ucosts) * s
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts, details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = 1:4, take_bound = integer(0), s0 = s, s = s, iter = 1L
  )
  expect_identical(result_details, expected_details)
})

### alloc: 1 bound ----

test_that("rna works well for M (alloc: 1 bound), H = 4", {
  n <- 270
  result <- rna(n, A, bounds)

  L <- 3L
  Lc <- c(1L, 2L, 4L)
  s0 <- n / sum(A)
  s <- sfun(n, A, bounds, R = L)
  expected <- c(A[1:2] * s, bounds[L], A[4] * s)
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(n, A, bounds, details = TRUE)
  s0 <-
    expected_details <- list(
      opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 2L
    )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for M (alloc: 1 bound), H = 4, non-def cost", {
  tcost <- 150
  result <- rna(tcost, A, bounds, ucosts[1])

  L <- 3L
  Lc <- c(1L, 2L, 4L)
  s0 <- sfun(tcost, A, ucosts = ucosts[1])
  s <- sfun(tcost, A, bounds, ucosts[1], L)
  expected <- c(A[1:2] / sqrt(ucosts[1]) * s, bounds[3], A[4] / sqrt(ucosts[1]) * s)
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts[1], details = TRUE)
  s0 <-
    expected_details <- list(
      opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 2L
    )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for M (alloc: 1 bound), H = 4, non-def cost vec", {
  tcost <- 700
  result <- rna(tcost, A, bounds, ucosts)

  L <- 3L
  Lc <- c(1L, 2L, 4L)
  s0 <- sfun(tcost, A, ucosts = ucosts)
  s <- sfun(tcost, A, bounds, ucosts, L)
  expected <- c(A[1:2] / sqrt(ucosts[1:2]) * s, bounds[3], A[4] / sqrt(ucosts[4]) * s)
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts, details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 2L
  )
  expect_identical(result_details, expected_details)
})

### alloc: 2 bounds ----

test_that("rna works well for M (alloc: 2 bounds), H = 4", {
  n <- 300
  result <- rna(n, A, bounds)

  L <- 2:3
  Lc <- c(1L, 4L)
  s0 <- n / sum(A)
  s <- sfun(n, A, bounds, R = L)
  expected <- c(A[1] * s, bounds[L], A[4] * s)
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(n, A, bounds, details = TRUE)
  s0 <-
    expected_details <- list(
      opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 3L
    )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for M (alloc: 2 bounds), H = 4, non-def cost", {
  tcost <- 200
  result <- rna(tcost, A, bounds, ucosts[1])

  L <- c(2L, 3L)
  Lc <- c(1L, 4L)
  s0 <- sfun(tcost, A, ucosts = ucosts[1])
  s <- sfun(tcost, A, bounds, ucosts[1], L)
  expected <- c(A[1] / sqrt(ucosts[1]) * s, bounds[L], A[4] / sqrt(ucosts[1]) * s)
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts[1], details = TRUE)
  s0 <-
    expected_details <- list(
      opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 3L
    )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for M (alloc: 2 bounds), H = 4, non-def cost vec", {
  tcost <- 1400
  result <- rna(tcost, A, bounds, ucosts)

  L <- c(1L, 3L)
  Lc <- c(2L, 4L)
  s0 <- sfun(tcost, A, ucosts = ucosts)
  s <- sfun(tcost, A, bounds, ucosts, L)
  expected <- c(bounds[1], A[2] / sqrt(ucosts[2]) * s, bounds[3], A[4] / sqrt(ucosts[4]) * s)
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, unit_costs = ucosts, details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 2L
  )
  expect_identical(result_details, expected_details)
})

### alloc: 3 bounds ----

test_that("rna works well for M (alloc: 3 bounds), H = 4", {
  n <- 330
  result <- rna(n, A, bounds)

  L <- 1:3
  Lc <- 4L
  s0 <- n / sum(A)
  s <- sfun(n, A, bounds, R = L)
  expected <- c(bounds[1:3], A[4] * s)
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(n, A, bounds, details = TRUE)
  s0 <-
    expected_details <- list(
      opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 3L
    )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for M (alloc: 3 bounds), H = 4, non-def cost", {
  tcost <- 229
  result <- rna(tcost, A, bounds, ucosts[1])

  L <- 1:3
  Lc <- 4L
  s0 <- sfun(tcost, A, ucosts = ucosts[1])
  s <- sfun(tcost, A, bounds, ucosts[1], L)
  expected <- c(bounds[1:3], A[4] / sqrt(ucosts[1]) * s)
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts[1], details = TRUE)
  s0 <-
    expected_details <- list(
      opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 3L
    )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for M (alloc: 3 bounds), H = 4, non-def cost vec", {
  tcost <- 1700
  result <- rna(tcost, A, bounds, ucosts)

  L <- c(1L, 3L, 4L)
  Lc <- 2L
  s0 <- sfun(tcost, A, ucosts = ucosts)
  s <- sfun(tcost, A, bounds, ucosts, L)
  expected <- c(bounds[1], A[2] / sqrt(ucosts[2]) * s, bounds[3:4])
  expect_identical(result, expected)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts, details = TRUE)
  expected_details <- list(
    opt = expected, take_neyman = Lc, take_bound = L, s0 = s0, s = s, iter = 3L
  )
  expect_identical(result_details, expected_details)
})

### alloc: 4 bounds (total_cost = sum(c * M)) ----

test_that("rna works well for n = sum(M), H = 4", {
  n <- sum(bounds)
  result <- rna(n, A, bounds)
  expect_identical(result, bounds)

  # Check details.
  result_details <- rna(n, A, bounds, details = TRUE)
  expected_details <- list(
    opt = bounds, take_neyman = integer(0), take_bound = 1:4, s0 = n / sum(A), s = NaN, iter = 4L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for total_cost = sum(c*M), H = 4, non-def cost", {
  tcost <- sum(bounds * ucosts[2])
  result <- rna(tcost, A, bounds, ucosts[2])
  expect_identical(result, bounds)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts[2], details = TRUE)
  s0 <- sfun(tcost, A, ucosts = ucosts[2])
  expected_details <- list(
    opt = bounds, take_neyman = integer(0), take_bound = 1:4, s0 = s0, s = NaN, iter = 4L
  )
  expect_identical(result_details, expected_details)
})

test_that("rna works well for total_cost = sum(c*M), H = 4, non-def cost vec", {
  tcost <- sum(bounds * ucosts)
  result <- rna(tcost, A, bounds, ucosts)
  expect_equal(result, bounds)

  # Check details.
  result_details <- rna(tcost, A, bounds, ucosts, details = TRUE)
  s0 <- sfun(tcost, A, ucosts = ucosts)
  expected_details <- list(
    opt = bounds,
    take_neyman = 2L,
    take_bound = c(1L, 3L, 4L),
    s0 = s0,
    s = 0.08714212528966687465459,
    iter = 3L
  )
  expect_equal(result_details, expected_details)
})
