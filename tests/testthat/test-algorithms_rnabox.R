M <- c(300, 400, 200, 90)

# Function ----

test_that("rnabox is valid function", {
  expect_function(
    rnabox,
    args = c("n", "A", "bounds1", "bounds2", "check_violations1", "check_violations2")
  )
})

# NEITHER `m` NOR `M` IS SPECIFIED ----

## H = 1 ----

test_that("rnabox works well when no bounds, H = 1)", {
  result <- rnabox(400, A[1])
  expect_equal(result, 400)
})

## H = 4 ----

test_that("rnabox works well when no bounds, H = 4)", {
  result <- rnabox(400, A)
  expected <- A * 400 / sum(A)
  expect_equal(result, expected)
})

# BOTH `m` AND `M` ARE SPECIFIED ----

## pop507 test ----

test_that("rnabox works well for m and M, (pop507)", {
  m <- ceiling(0.3 * N_pop507)
  n_M50 <- ceiling(seq(0.3, 0.99, 0.02) * sum(N_pop507)) # nolintr
  result <- lapply(n_M50, rnabox, A_pop507, N_pop507, m)
  result_twin <- lapply(n_M50, rnabox, A_pop507, m, N_pop507, leq, geq)
  expect_identical(result, result_twin)
  expect_snapshot(result)
})

## H = 1 ----

### alloc: no bound ----

test_that("rnabox works well for m and M (alloc: no bound), H = 1", {
  result <- rnabox(150, A[1], M[1], m[1])
  result_twin <- rnabox(150, A[1], m[1], M[1], leq, geq)
  expect_identical(result, result_twin)
  expect_equal(result, 150)
})

### alloc: 1 bound m (n = m) ----

test_that("rnabox works well for m and M (alloc: 1 bound m), H = 1", {
  result <- rnabox(m[1], A[1], M[1], m[1])
  result_twin <- rnabox(m[1], A[1], m[1], M[1], leq, geq)
  expect_identical(result, result_twin)
  expect_equal(result, m[1])
})

### alloc: 1 bound M (n = M) ----

test_that("rnabox works well for m and M (alloc: 1 bound M), H = 1", {
  result <- rnabox(M[1], A[1], M[1], m[1])
  result_twin <- rnabox(M[1], A[1], m[1], M[1], leq, geq)
  expect_identical(result, result_twin)
  expect_equal(result, M[1])
})

## H = 4 ----

### some example population ----

test_that("rnabox works well for m and M (some population)", {
  S <- c(10.4, 1.8, 0.3, 0.9)
  N <- c(100, 100, 100, 100)
  m <- c(10, 10, 10, 10)
  M <- c(30, 30, 30, 30)
  result <- rnabox(80, N * S, M, m)
  result_twin <- rnabox(80, N * S, m, M, leq, geq)
  expect_identical(result, result_twin)
  expect_equal(result, c(30, 26.6667, 10, 13.3333), tolerance = 10 - 9)
})

### M with Inf ----

test_that("rnabox works well for m and M with Inf, H = 4", {
  result <- rnabox(5000, A, c(M[1:3], Inf), m)
  result_twin <- rnabox(5000, A, m, c(M[1:3], Inf), leq, geq)
  expect_identical(result, result_twin)
  expect_equal(result, c(M[1:3], 4100))
})

### alloc: no bounds ----

test_that("rnabox works well for m and M (alloc: no bounds), H = 4", {
  result <- rnabox(553, A, M, m)
  result_twin <- rnabox(553, A, m, M, leq, geq)
  expected <- c(118.5, 158, 197.5, 79)
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 1 bound m ----

test_that("rnabox works well for m and M (alloc: 1 bound m), H = 4", {
  result <- rnabox(463, A, M, m)
  result_twin <- rnabox(463, A, m, M, leq, geq)
  expected <- c(m[1], 132, 165, 66)
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 1 bound M ----

test_that("rnabox works well for m and M (alloc: 1 bound M), H = 4", {
  result <- rnabox(596, A, M, m)
  result_twin <- rnabox(596, A, m, M, leq, geq)
  expected <- c(132, 176, M[3], 88)
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 2 bounds mm ----

test_that("rnabox works well for m and M (alloc: 2 bounds mm), H = 4", {
  result <- rnabox(366, A, M, m)
  result_twin <- rnabox(366, A, m, M, leq, geq)
  expected <- c(m[1], 96, 120, m[4])
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 2 bounds mM ----

test_that("rnabox works well for m and M (alloc: 2 bounds mM), H = 4", {
  m[3] <- 500
  M[3] <- 800
  result <- rnabox(1283, A, M, m)
  result_twin <- rnabox(1283, A, m, M, leq, geq)
  expected <- c(297, 396, m[3], M[4])
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 2 bounds MM ----

test_that("rnabox works well for m and M (alloc: 2 bounds MM), H = 4", {
  result <- rnabox(976, A, M, m)
  result_twin <- rnabox(976, A, m, M, leq, geq)
  expected <- c(294, 392, M[3:4])
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 3 bounds mmm ----

test_that("rnabox works well for m and M (alloc: 3 bounds mmm), H = 4", {
  m[3] <- 500
  M[3] <- 800
  result <- rnabox(748, A, M, m)
  result_twin <- rnabox(748, A, m, M, leq, geq)
  expected <- c(m[1], 98, m[3:4])
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 3 bounds mmM ----

test_that("rnabox works well for m and M (alloc: 3 bounds mmM), H = 4", {
  A[4] <- 7000
  result <- rnabox(384, A, M, m)
  result_twin <- rnabox(384, A, m, M, leq, geq)
  expected <- c(m[1:2], 104, M[4])
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 3 bounds mMM ----

test_that("rnabox works well for m and M (alloc: 3 bounds mMM), H = 4", {
  m[3] <- 700
  M[2:3] <- c(100, 900)
  result <- rnabox(1174, A, M, m)
  result_twin <- rnabox(1174, A, m, M, leq, geq)
  expected <- c(284, M[2], m[3], M[4])
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 3 bounds MMM ----

test_that("rnabox works well for m and M (alloc: 3 bounds MMM), H = 4", {
  m[3] <- 700
  M[2:3] <- c(100, 900)
  result <- rnabox(1284, A, M, m)
  result_twin <- rnabox(1284, A, m, M, leq, geq)
  expected <- c(M[1:2], 794, M[4])
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 4 bounds mmmm (n = sum(m))----

test_that("rnabox works well for m and M (alloc: 4 bounds mmmm), H = 4", {
  result <- rnabox(310, A, M, m)
  result_twin <- rnabox(310, A, m, M, leq, geq)
  expect_identical(result, result_twin)
  expect_equal(result, m)
})

### alloc: 4 bounds MMMM (n = sum(M)) ----

test_that("rnabox works well for m and M (alloc: 4 bounds MMMM), H = 4", {
  result <- rnabox(990, A, M, m)
  result_twin <- rnabox(990, A, m, M, leq, geq)
  expect_identical(result, result_twin)
  expect_equal(result, M)
})

### alloc: 4 bounds mmmM (vertex) ----

test_that("rnabox works well for m and M (vertex mmmM), H = 4", {
  m[3] <- 700
  M[2:3] <- c(100, 900)
  result <- rnabox(950, A, M, m)
  result_twin <- rnabox(950, A, m, M, leq, geq)
  expected <- c(m[1], M[2], m[3:4])
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

## H = 10 ----

### alloc: 10 bounds 9m1M (vertex) ----

test_that("rnabox works well for m and M, H = 10 (vertex)", {
  N <- c(454, 10, 116, 2500, 2240, 260, 39, 3000, 2500, 400)
  S <- c(0.9, 5000, 32, 0.1, 3, 5, 300, 13, 20, 7)
  A <- N * S
  m <- c(322, 3, 57, 207, 715, 121, 9, 1246, 1095, 294)
  M <- N

  result <- rnabox(4076, A, M, m)
  result_twin <- rnabox(4076, A, m, M, leq, geq)
  expected <- c(m[1], M[2], m[3:10])
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

# ONLY `m` IS SPECIFIED ----

## pop507 test ----

test_that("rnabox works well for m, (pop507)", {
  n_m51 <- seq(1, 2, 0.02) * sum(N_pop507)
  result <- lapply(n_m51, rnabox, A_pop507, bounds2 = N_pop507)
  result_twin <- lapply(
    n_m51, rnabox, A_pop507, N_pop507,
    check_violations1 = leq, check_violations2 = geq
  )
  expect_identical(result, result_twin)
  expect_snapshot(result)
})

## H = 1 ----

### alloc: no bound ----

test_that("rnabox works well for m (alloc: no bound), H = 1", {
  result <- rnabox(101, A[1], bounds2 = m[1])
  result_twin <- rnabox(
    101, A[1], m[1],
    check_violations1 = leq, check_violations2 = geq
  )
  expect_identical(result, result_twin)
  expect_equal(result, 101)
})

### alloc: 1 bound (n = m) ----

test_that("rnabox works well for m (alloc: 1 bound), H = 1", {
  result <- rnabox(m[1], A[1], bounds2 = m[1])
  result_twin <- rnabox(
    m[1], A[1], m[1],
    check_violations1 = leq, check_violations2 = geq
  )
  expect_identical(result, result_twin)
  expect_equal(result, m[1])
})

## H = 4 ----

### alloc: no bounds ----

test_that("rnabox works well for m (alloc: no bounds), H = 4", {
  result <- rnabox(600, A, bounds2 = m)
  result_twin <- rnabox(
    600, A, m,
    check_violations1 = leq, check_violations2 = geq
  )
  expected <- c(128.57143, 171.42857, 214.28571, 85.71429)
  expect_identical(result, result_twin)
  expect_equal(result, expected, tolerance = 1e-7)
})

### alloc: 1 bound ----

test_that("rnabox works well for m (alloc: 1 bound), H = 4", {
  result <- rnabox(463, A, bounds2 = m)
  result_twin <- rnabox(
    463, A, m,
    check_violations1 = leq, check_violations2 = geq
  )
  expected <- c(m[1], 132, 165, 66)
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 2 bounds----

test_that("rnabox works well for m (alloc: 2 bounds), H = 4", {
  result <- rnabox(375, A, bounds2 = m)
  result_twin <- rnabox(
    375, A, m,
    check_violations1 = leq, check_violations2 = geq
  )
  expected <- c(m[1], 100, 125, m[4])
  expect_identical(result, result_twin)
  expect_equal(result, expected, tolerance = 1e-6)
})

### alloc: 3 bounds----

test_that("rnabox works well for m (alloc: 3 bounds), H = 4", {
  result <- rnabox(338, A, bounds2 = m)
  result_twin <- rnabox(
    338, A, m,
    check_violations1 = leq, check_violations2 = geq
  )
  expected <- c(m[1:2], 98, m[4])
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 4 bounds (n = sum(m))----

test_that("rnabox works well for n = sum(m), H = 4", {
  result <- rnabox(310, A, bounds2 = m)
  result_twin <- rnabox(
    310, A, m,
    check_violations1 = leq, check_violations2 = geq
  )
  expect_identical(result, result_twin)
  expect_equal(result, m)
})

# ONLY `M` IS SPECIFIED ----

## pop507 test ----

test_that("rnabox works well for M, (pop507)", {
  n_M50 <- seq(0.01, 0.99, 0.02) * sum(N_pop507) # nolintr
  result <- lapply(n_M50, rnabox, A_pop507, N_pop507)
  result_twin <- lapply(
    n_M50, rnabox, A_pop507,
    bounds2 = N_pop507, check_violations1 = leq, check_violations2 = geq
  )
  expect_identical(result, result_twin)
  expect_snapshot(result)
})

## H = 1 ----

### alloc: no bound ----

test_that("rnabox works well for M (alloc: no bound), H = 1", {
  result <- rnabox(99, A[1], M[1])
  result_twin <- rnabox(
    99, A[1],
    bounds2 = M[1], check_violations1 = leq, check_violations2 = geq
  )
  expect_identical(result, result_twin)
  expect_equal(result, 99)
})

### alloc: 1 bound (n = M) ----

test_that("rnabox works well for M (alloc: 1 bound), H = 1", {
  result <- rnabox(M[1], A[1], M[1])
  result_twin <- rnabox(
    M[1], A[1],
    bounds2 = M[1], check_violations1 = leq, check_violations2 = geq
  )
  expect_identical(result, result_twin)
  expect_equal(result, M[1])
})

## H = 4 ----

### M with Inf ----

test_that("rnabox works well for M with Inf, H = 4", {
  result <- rnabox(5000, A, c(M[1:3], Inf))
  result_twin <- rnabox(
    5000, A,
    bounds2 = c(M[1:3], Inf), check_violations1 = leq, check_violations2 = geq
  )
  expect_identical(result, result_twin)
  expect_equal(result, c(M[1:3], 4100))
})

### alloc: no bounds ----

test_that("rnabox works well for M (alloc: no bounds), H = 4", {
  result <- rnabox(190, A, M)
  result_twin <- rnabox(
    190, A,
    bounds2 = M, check_violations1 = leq, check_violations2 = geq
  )
  expected <- c(40.71429, 54.28571, 67.85714, 27.14286)
  expect_identical(result, result_twin)
  expect_equal(result, expected, tolerance = 1e-7)
})

### alloc: 1 bound ----

test_that("rnabox works well for M (alloc: 1 bound), H = 4", {
  result <- rnabox(596, A, M)
  result_twin <- rnabox(
    596, A,
    bounds2 = M, check_violations1 = leq, check_violations2 = geq
  )
  expected <- c(132, 176, M[3], 88)
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 2 bounds ----

test_that("rnabox works well for M (alloc: 2 bounds), H = 4", {
  result <- rnabox(962, A, M)
  result_twin <- rnabox(
    962, A,
    bounds2 = M, check_violations1 = leq, check_violations2 = geq
  )
  expected <- c(288, 384, M[3:4])
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 3 bounds ----

test_that("rnabox works well for M (alloc: 3 bounds), H = 4", {
  m[3] <- 700
  M[2:3] <- c(100, 900)
  result <- rnabox(1200, A, M)
  result_twin <- rnabox(
    1200, A,
    bounds2 = M, check_violations1 = leq, check_violations2 = geq
  )
  expected <- c(M[1:2], 710, M[4])
  expect_identical(result, result_twin)
  expect_equal(result, expected)
})

### alloc: 4 bounds (n = sum(M)) ----

test_that("rnabox works well for n = sum(M), H = 4", {
  result <- rnabox(990, A, M)
  result_twin <- rnabox(
    990, A,
    bounds2 = M, check_violations1 = leq, check_violations2 = geq
  )
  expect_identical(result, result_twin)
  expect_equal(result, M)
})
