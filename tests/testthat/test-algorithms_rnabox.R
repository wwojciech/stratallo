N_pop507 <- pop507[, "N"] # nolintr
a_pop507 <- N_pop507 * pop507[, "S"]

a <- c(3000, 4000, 5000, 2000)
m <- c(100, 90, 70, 50)
M <- c(300, 400, 200, 90)

# NEITHER `m` NOR `M` IS SPECIFIED ----

## H = 1 ----

test_that("rnabox works well when no bounds, H = 1)", {
  result <- rnabox(400, a[1])
  expect_equal(result, 400)
})

## H = 4 ----

test_that("rnabox works well when no bounds, H = 4)", {
  result <- rnabox(400, a)
  expected <- a * 400 / sum(a)
  expect_equal(result, expected)
})

# BOTH `m` AND `M` ARE SPECIFIED ----

## pop507 test ----

test_that("rnabox works well for m and M, (pop507)", {
  m <- ceiling(0.3 * N_pop507)
  n_M50 <- ceiling(seq(0.3, 0.99, 0.02) * sum(N_pop507)) # nolintr
  result <- lapply(n_M50, rnabox, a_pop507, m, N_pop507)
  expect_snapshot(result)
})

## H = 1 ----

### alloc: no bound ----

test_that("rnabox works well for m and M (alloc: no bound), H = 1", {
  result <- rnabox(150, a[1], m = m[1], M = M[1])
  expect_equal(result, 150)
})

### alloc: 1 bound m (n = m) ----

test_that("rnabox works well for m and M (alloc: 1 bound m), H = 1", {
  result <- rnabox(m[1], a[1], m = m[1], M = M[1])
  expect_equal(result, m[1])
})

### alloc: 1 bound M (n = M) ----

test_that("rnabox works well for m and M (alloc: 1 bound M), H = 1", {
  result <- rnabox(M[1], a[1], m = m[1], M = M[1])
  expect_equal(result, M[1])
})

## H = 4 ----

### some example population ----

test_that("rnabox works well for m and M (some population)", {
  S <- c(10.4, 1.8, 0.3, 0.9)
  N <- c(100, 100, 100, 100)
  result <- rnabox(80, N * S, m = c(10, 10, 10, 10), M = c(30, 30, 30, 30))
  expect_equal(result, c(30, 26.6667, 10, 13.3333), tolerance = 10 - 9)
})

### M with Inf ----

test_that("rnabox works well for m and M with Inf, H = 4", {
  result <- rnabox(5000, a, m = m, M = c(M[1:3], Inf))
  expect_equal(result, c(M[1:3], 4100))
})

### alloc: no bounds ----

test_that("rnabox works well for m and M (alloc: no bounds), H = 4", {
  result <- rnabox(553, a, m, M)
  expected <- c(118.5, 158, 197.5, 79)
  expect_equal(result, expected)
})

### alloc: 1 bound m ----

test_that("rnabox works well for m and M (alloc: 1 bound m), H = 4", {
  result <- rnabox(463, a, m, M)
  expected <- c(m[1], 132, 165, 66)
  expect_equal(result, expected)
})

### alloc: 1 bound M ----

test_that("rnabox works well for m and M (alloc: 1 bound M), H = 4", {
  result <- rnabox(596, a, m, M)
  expected <- c(132, 176, M[3], 88)
  expect_equal(result, expected)
})

### alloc: 2 bounds mm ----

test_that("rnabox works well for m and M (alloc: 2 bounds mm), H = 4", {
  result <- rnabox(366, a, m, M)
  expected <- c(m[1], 96, 120, m[4])
  expect_equal(result, expected)
})

### alloc: 2 bounds mM ----

test_that("rnabox works well for m and M (alloc: 2 bounds mM), H = 4", {
  m[3] <- 500
  M[3] <- 800
  result <- rnabox(1283, a, m, M)
  expected <- c(297, 396, m[3], M[4])
  expect_equal(result, expected)
})

### alloc: 2 bounds MM ----

test_that("rnabox works well for m and M (alloc: 2 bounds MM), H = 4", {
  result <- rnabox(976, a, m, M)
  expected <- c(294, 392, M[3:4])
  expect_equal(result, expected)
})

### alloc: 3 bounds mmm ----

test_that("rnabox works well for m and M (alloc: 3 bounds mmm), H = 4", {
  m[3] <- 500
  M[3] <- 800
  result <- rnabox(748, a, m, M)
  expected <- c(m[1], 98, m[3:4])
  expect_equal(result, expected)
})

### alloc: 3 bounds mmM ----

test_that("rnabox works well for m and M (alloc: 3 bounds mmM), H = 4", {
  a[4] <- 7000
  result <- rnabox(384, a, m, M)
  expected <- c(m[1:2], 104, M[4])
  expect_equal(result, expected)
})

### alloc: 3 bounds mMM ----

test_that("rnabox works well for m and M (alloc: 3 bounds mMM), H = 4", {
  m[3] <- 700
  M[2:3] <- c(100, 900)
  result <- rnabox(1174, a, m, M)
  expected <- c(284, M[2], m[3], M[4])
  expect_equal(result, expected)
})

### alloc: 3 bounds MMM ----

test_that("rnabox works well for m and M (alloc: 3 bounds MMM), H = 4", {
  m[3] <- 700
  M[2:3] <- c(100, 900)
  result <- rnabox(1284, a, m, M)
  expected <- c(M[1:2], 794, M[4])
  expect_equal(result, expected)
})

### alloc: 4 bounds mmmm (n = sum(m))----

test_that("rnabox works well for m and M (alloc: 4 bounds mmmm), H = 4", {
  result <- rnabox(310, a, m, M)
  expect_equal(result, m)
})

### alloc: 4 bounds MMMM (n = sum(M)) ----

test_that("rnabox works well for m and M (alloc: 4 bounds MMMM), H = 4", {
  result <- rnabox(990, a, m, M)
  expect_equal(result, M)
})

### alloc: 4 bounds mmmM (vertex) ----

test_that("rnabox works well for m and M (vertex mmmM), H = 4", {
  m[3] <- 700
  M[2:3] <- c(100, 900)
  result <- rnabox(950, a, m, M)
  expected <- c(m[1], M[2], m[3:4])
  expect_equal(result, expected)
})

## H = 10 ----

### alloc: 10 bounds 9m1M (vertex) ----

test_that("rnabox works well for m and M, H = 10 (vertex)", {
  N <- c(454, 10, 116, 2500, 2240, 260, 39, 3000, 2500, 400)
  S <- c(0.9, 5000, 32, 0.1, 3, 5, 300, 13, 20, 7)
  a <- N * S
  m <- c(322, 3, 57, 207, 715, 121, 9, 1246, 1095, 294)
  M <- N

  result <- rnabox(4076, a, m, M)
  expected <- c(m[1], M[2], m[3:10])
  expect_equal(result, expected)
})

# ONLY `m` IS SPECIFIED ----

## pop507 test ----

test_that("rnabox works well for m, (pop507)", {
  n_m51 <- seq(1, 2, 0.02) * sum(N_pop507)
  result <- lapply(n_m51, rnabox, a_pop507, N_pop507)
  expect_snapshot(result)
})

## H = 1 ----

### alloc: no bound ----

test_that("rnabox works well for m (alloc: no bound), H = 1", {
  result <- rnabox(101, a[1], m[1])
  expect_equal(result, 101)
})

### alloc: 1 bound (n = m) ----

test_that("rnabox works well for m (alloc: 1 bound), H = 1", {
  result <- rnabox(m[1], a[1], m[1])
  expect_equal(result, m[1])
})

## H = 4 ----

### alloc: no bounds ----

test_that("rnabox works well for m (alloc: no bounds), H = 4", {
  result <- rnabox(600, a, m)
  expected <- c(128.57143, 171.42857, 214.28571, 85.71429)
  expect_equal(result, expected, tolerance = 1e-7)
})

### alloc: 1 bound ----

test_that("rnabox works well for m (alloc: 1 bound), H = 4", {
  result <- rnabox(463, a, m)
  expected <- c(m[1], 132, 165, 66)
  expect_equal(result, expected)
})

### alloc: 2 bounds----

test_that("rnabox works well for m (alloc: 2 bounds), H = 4", {
  result <- rnabox(375, a, m)
  expected <- c(m[1], 100, 125, m[4])
  expect_equal(result, expected, tolerance = 1e-6)
})

### alloc: 3 bounds----

test_that("rnabox works well for m (alloc: 3 bounds), H = 4", {
  result <- rnabox(338, a, m)
  expected <- c(m[1:2], 98, m[4])
  expect_equal(result, expected)
})

### alloc: 4 bounds (n = sum(m))----

test_that("rnabox works well for n = sum(m), H = 4", {
  result <- rnabox(310, a, m)
  expect_equal(result, m)
})

# ONLY `M` IS SPECIFIED ----

## pop507 test ----

test_that("rnabox works well for M, (pop507)", {
  n_M50 <- seq(0.01, 0.99, 0.02) * sum(N_pop507) # nolintr
  result <- lapply(n_M50, rnabox, a_pop507, M = N_pop507)
  expect_snapshot(result)
})

## H = 1 ----

### alloc: no bound ----

test_that("rnabox works well for M (alloc: no bound), H = 1", {
  result <- rnabox(99, a[1], M = M[1])
  expect_equal(result, 99)
})

### alloc: 1 bound (n = M) ----

test_that("rnabox works well for M (alloc: 1 bound), H = 1", {
  result <- rnabox(M[1], a[1], M = M[1])
  expect_equal(result, M[1])
})

## H = 4 ----

### M with Inf ----

test_that("rnabox works well for M with Inf, H = 4", {
  result <- rnabox(5000, a, M = c(M[1:3], Inf))
  expect_equal(result, c(M[1:3], 4100))
})

### alloc: no bounds ----

test_that("rnabox works well for M (alloc: no bounds), H = 4", {
  result <- rnabox(190, a, M = M)
  expected <- c(40.71429, 54.28571, 67.85714, 27.14286)
  expect_equal(result, expected, tolerance = 1e-7)
})

### alloc: 1 bound ----

test_that("rnabox works well for M (alloc: 1 bound), H = 4", {
  result <- rnabox(596, a, M = M)
  expected <- c(132, 176, M[3], 88)
  expect_equal(result, expected)
})

### alloc: 2 bounds ----

test_that("rnabox works well for M (alloc: 2 bounds), H = 4", {
  result <- rnabox(962, a, M = M)
  expected <- c(288, 384, M[3:4])
  expect_equal(result, expected)
})

### alloc: 3 bounds ----

test_that("rnabox works well for M (alloc: 3 bounds), H = 4", {
  m[3] <- 700
  M[2:3] <- c(100, 900)
  result <- rnabox(1200, a, M = M)
  expected <- c(M[1:2], 710, M[4])
  expect_equal(result, expected)
})

### alloc: 4 bounds (n = sum(M)) ----

test_that("rnabox works well for n = sum(M), H = 4", {
  result <- rnabox(990, a, M = M)
  expect_equal(result, M)
})
