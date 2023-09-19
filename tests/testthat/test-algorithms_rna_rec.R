# Function ----

test_that("rna_rec is valid function", {
  expect_function(
    rna_rec,
    args = c("total_cost", "A", "bounds", "unit_costs", "check_violations")
  )
})

# NO BOUNDS ----

## H = 1 ----

test_that("rna_rec works well when no bounds, H = 1)", {
  result <- rna_rec(400, A[1])
  expect_equal(result, 400)
})

## H = 4 ----

test_that("rna_rec works well when no bounds, H = 4)", {
  result <- rna_rec(400, A)
  expected <- A * 400 / sum(A)
  expect_equal(result, expected)
})

# LOWER ----

## pop507 test ----

test_that("rna_rec works well for m, (pop507)", {
  n_m51 <- seq(1, 2, 0.02) * sum(N_pop507)
  result <- lapply(n_m51, rna_rec, A_pop507, N_pop507, check_violations = .Primitive("<="))
  expect_snapshot(result)
})


## H = 1 ----

### alloc: no bound ----

test_that("rna_rec works well for m (alloc: no bound), H = 1", {
  result <- rna_rec(101, A[1], bounds[1], check_violations = .Primitive("<="))
  expect_equal(result, 101)
})

### alloc: 1 bound (n = m) ----

test_that("rna_rec works well for m (alloc: 1 bound), H = 1", {
  result <- rna_rec(bounds[1], A[1], bounds[1], check_violations = .Primitive("<="))
  expect_equal(result, bounds[1])
})

## H = 4 ----

### alloc: no bounds ----

test_that("rna_rec works well for m (alloc: no bounds), H = 4", {
  result <- rna_rec(600, A, bounds, check_violations = .Primitive("<="))
  expected <- c(128.57143, 171.42857, 214.28571, 85.71429)
  expect_equal(result, expected, tolerance = 1e-7)
})

### alloc: 1 bound ----

test_that("rna_rec works well for m (alloc: 1 bound), H = 4", {
  result <- rna_rec(500, A, bounds, check_violations = .Primitive("<="))
  expected <- c(105, 140, 175, 80)
  expect_equal(result, expected)
})

### alloc: 2 bounds ----

test_that("rna_rec works well for m (alloc: 2 bounds), H = 4", {
  result <- rna_rec(420, A, bounds, check_violations = .Primitive("<="))
  expected <- c(100, 106.6667, 133.3333, 80)
  expect_equal(result, expected, tolerance = 1e-6)
})

### alloc: 3 bounds ----

test_that("rna_rec works well for m (alloc: 3 bounds), H = 4", {
  result <- rna_rec(370, A, bounds, check_violations = .Primitive("<="))
  expected <- c(100, 90, 100, 80)
  expect_equal(result, expected)
})

### alloc: 4 bounds (n = sum(m)) ----

test_that("rna_rec works well for n = sum(m), H = 4", {
  result <- rna_rec(340, A, bounds, check_violations = .Primitive("<="))
  expected <- c(100, 90, 70, 80)
  expect_equal(result, expected)
})

# UPPER ----

## pop507 test ----

test_that("rna_rec works well for M, (pop507)", {
  n_M50 <- seq(0.01, 0.99, 0.02) * sum(N_pop507) # nolintr
  result <- lapply(n_M50, rna_rec, A_pop507, N_pop507)
  expect_snapshot(result)
})

## H = 1 ----

### alloc: no bound ----

test_that("rna_rec works well for M (alloc: no bound), H = 1", {
  result <- rna_rec(99, A[1], bounds[1])
  expect_equal(result, 99)
})

### alloc: 1 bound (n = M) ----

test_that("rna_rec works well for M (alloc: 1 bound), H = 1", {
  result <- rna_rec(bounds[1], A[1], bounds[1])
  expect_equal(result, bounds[1])
})

## H = 4 ----

### M with Inf ----

test_that("rna_rec works well for M with Inf, H = 4", {
  result <- rna_rec(5000, A, c(bounds[1:3], Inf))
  expected <- c(100, 90, 70, 4740)
  expect_equal(result, expected)
})

### alloc: no bounds ----

test_that("rna_rec works well for M (alloc: no bounds), H = 4", {
  result <- rna_rec(190, A, bounds)
  expected <- c(40.71429, 54.28571, 67.85714, 27.14286)
  expect_equal(result, expected, tolerance = 1e-7)
})

### alloc: 1 bound ----

test_that("rna_rec works well for M (alloc: 1 bound), H = 4", {
  result <- rna_rec(270, A, bounds)
  expected <- c(66.66667, 88.88889, 70, 44.44444)
  expect_equal(result, expected, tolerance = 1e-7)
})

### alloc: 2 bounds ----

test_that("rna_rec works well for M (alloc: 2 bounds), H = 4", {
  result <- rna_rec(300, A, bounds)
  expected <- c(84, 90, 70, 56)
  expect_equal(result, expected)
})

### alloc: 3 bounds ----

test_that("rna_rec works well for M (alloc: 3 bounds), H = 4", {
  result <- rna_rec(330, A, bounds)
  expected <- c(100, 90, 70, 70)
  expect_equal(result, expected)
})

### alloc: 4 bounds (n = sum(M)) ----

test_that("rna_rec works well for n = sum(M), H = 4", {
  result <- rna_rec(340, A, bounds)
  expect_equal(result, bounds)
})
