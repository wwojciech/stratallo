# Function ----

test_that("coma is valid function", {
  expect_function(coma, args = c("total_cost", "A", "M"))
})

# pop507 test ----

test_that("coma works well for M, (pop507)", {
  N <- pop507[, "N"]
  A <- N * pop507[, "S"]
  n <- seq(0.01, 0.99, 0.02) * sum(N)
  result <- lapply(n, coma, A, N)
  expect_snapshot(result)
})

# H = 1 ----

## alloc: no bound ----

test_that("coma works well for M (alloc: no bound), H = 1", {
  result <- coma(99, A[1], M[1])
  expect_equal(result, 99)
})

## alloc: 1 bound (n = M) ----

test_that("coma works well for M (alloc: 1 bound), H = 1", {
  result <- coma(M[1], A[1], M[1])
  expect_equal(result, M[1])
})

# H = 4 ----

## M with Inf ----

test_that("coma works well for M with Inf, H = 4", {
  result <- coma(5000, A, c(M[1:3], Inf))
  expect_equal(result, c(100, 90, 70, 4740))
})

## alloc: no bounds ----

test_that("coma works well for M (alloc: no bounds), H = 4", {
  result <- coma(190, A, M)
  expected <- c(40.71429, 54.28571, 67.85714, 27.14286)
  expect_equal(result, expected, tolerance = 1e-7)
})

## alloc: 1 bound ----

test_that("coma works well for M (alloc: 1 bound), H = 4", {
  result <- coma(270, A, M)
  expected <- c(66.66667, 88.88889, 70, 44.44444)
  expect_equal(result, expected, tolerance = 1e-7)
})

## alloc: 2 bounds ----

test_that("coma works well for M (alloc: 2 bounds), H = 4", {
  result <- coma(300, A, M)
  expected <- c(84, 90, 70, 56)
  expect_equal(result, expected)
})

## alloc: 3 bounds ----

test_that("coma works well for M (alloc: 3 bounds), H = 4", {
  result <- coma(330, A, M)
  expected <- c(100, 90, 70, 70)
  expect_equal(result, expected)
})

## alloc: 4 bounds (n = sum(M)) ----

test_that("coma works well for n = sum(M), H = 4", {
  result <- coma(340, A, M)
  expect_equal(result, M)
})
