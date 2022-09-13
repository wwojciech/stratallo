N_pop507 <- pop507[, "N"] # nolintr
a_pop507 <- N_pop507 * pop507[, "S"]

a <- c(3000, 4000, 5000, 2000)
bounds <- c(100, 90, 70, 80)

# NO BOUNDS ----

## H = 1 ----

test_that("rna_onesided works well when no bounds, H = 1)", {
  result <- rna_onesided(400, a[1])
  expect_equal(result, 400)

  # Check assignments.
  result_asgn <- rna_onesided(400, a[1], assignments = TRUE)
  expected_asgn <- list(opt = 400, take_neyman = 1, take_bound = numeric(0))
  expect_equal(result_asgn, expected_asgn)
})

## H = 4 ----

test_that("rna_onesided works well when no bounds, H = 4)", {
  result <- rna_onesided(400, a)
  expected <- a * 400 / sum(a)
  expect_equal(result, expected)

  # Check assignments.
  result_asgn <- rna_onesided(400, a, assignments = TRUE)
  expected_asgn <- list(opt = expected, take_neyman = 1:4, take_bound = numeric(0))
  expect_equal(result_asgn, expected_asgn)
})

# LOWER ----

## pop507 test ----

test_that("rna_onesided works well for m, (pop507)", {
  n_m51 <- seq(1, 2, 0.02) * sum(N_pop507)
  result <- lapply(n_m51, rna_onesided, a_pop507, N_pop507, FALSE)
  expect_snapshot(result)

  # Check assignments.
  result_asgn <- lapply(n_m51, rna_onesided, a_pop507, N_pop507, FALSE, TRUE)
  expect_snapshot(result_asgn)
})


## H = 1 ----

### alloc: no bound ----

test_that("rna_onesided works well for m (alloc: no bound), H = 1", {
  result <- rna_onesided(101, a[1], bounds[1], FALSE)
  expect_equal(result, 101)

  # Check assignments.
  result_asgn <- rna_onesided(101, a[1], bounds[1], FALSE, TRUE)
  expected_asgn <- list(opt = 101, take_neyman = 1, take_bound = numeric(0))
  expect_equal(result_asgn, expected_asgn)
})

### alloc: 1 bound (n = m) ----

test_that("rna_onesided works well for m (alloc: 1 bound), H = 1", {
  result <- rna_onesided(bounds[1], a[1], bounds[1], FALSE)
  expect_equal(result, bounds[1])

  # Check assignments.
  result_asgn <- rna_onesided(bounds[1], a[1], bounds[1], FALSE, TRUE)
  expected_asgn <- list(opt = bounds[1], take_neyman = numeric(0), take_bound = 1)
  expect_equal(result_asgn, expected_asgn)
})

## H = 4 ----

### alloc: no bounds ----

test_that("rna_onesided works well for m (alloc: no bounds), H = 4", {
  result <- rna_onesided(600, a, bounds, FALSE)
  expected <- c(128.57143, 171.42857, 214.28571, 85.71429)
  expect_equal(result, expected, tolerance = 1e-7)

  # Check assignments.
  result_asgn <- rna_onesided(600, a, bounds, FALSE, TRUE)
  expected_asgn <- list(opt = expected, take_neyman = 1:4, take_bound = numeric(0))
  expect_equal(result_asgn, expected_asgn, tolerance = 1e-7)
})

### alloc: 1 bound ----

test_that("rna_onesided works well for m (alloc: 1 bound), H = 4", {
  result <- rna_onesided(500, a, bounds, FALSE)
  expected <- c(105, 140, 175, 80)
  expect_equal(result, expected)

  # Check assignments.
  result_asgn <- rna_onesided(500, a, bounds, FALSE, TRUE)
  expected_asgn <- list(opt = expected, take_neyman = 1:3, take_bound = 4)
  expect_equal(result_asgn, expected_asgn)
})

### alloc: 2 bounds ----

test_that("rna_onesided works well for m (alloc: 2 bounds), H = 4", {
  result <- rna_onesided(420, a, bounds, FALSE)
  expected <- c(100, 106.6667, 133.3333, 80)
  expect_equal(result, expected, tolerance = 1e-6)

  # Check assignments.
  result_asgn <- rna_onesided(420, a, bounds, FALSE, TRUE)
  expected_asgn <- list(opt = expected, take_neyman = c(2, 3), take_bound = c(1, 4))
  expect_equal(result_asgn, expected_asgn, tolerance = 1e-6)
})

### alloc: 3 bounds ----

test_that("rna_onesided works well for m (alloc: 3 bounds), H = 4", {
  result <- rna_onesided(370, a, bounds, FALSE)
  expected <- c(100, 90, 100, 80)
  expect_equal(result, expected)

  # Check assignments.
  result_asgn <- rna_onesided(370, a, bounds, FALSE, TRUE)
  expected_asgn <- list(opt = expected, take_neyman = 3, take_bound = c(1, 2, 4))
  expect_equal(result_asgn, expected_asgn)
})

### alloc: 4 bounds (n = sum(m)) ----

test_that("rna_onesided works well for n = sum(m), H = 4", {
  result <- rna_onesided(340, a, bounds, FALSE)
  expected <- c(100, 90, 70, 80)
  expect_equal(result, expected)

  # Check assignments.
  result_asgn <- rna_onesided(340, a, bounds, FALSE, TRUE)
  expected_asgn <- list(opt = expected, take_neyman = numeric(0), take_bound = 1:4)
  expect_equal(result_asgn, expected_asgn)
})

# UPPER ----

## pop507 test ----

test_that("rna_onesided works well for M, (pop507)", {
  n_M50 <- seq(0.01, 0.99, 0.02) * sum(N_pop507) # nolintr
  result <- lapply(n_M50, rna_onesided, a_pop507, N_pop507)
  expect_snapshot(result)

  # Check assignments.
  result_asgn <- lapply(n_M50, rna_onesided, a_pop507, N_pop507, assignments = TRUE)
  expect_snapshot(result_asgn)
})

## H = 1 ----

### alloc: no bound ----

test_that("rna_onesided works well for M (alloc: no bound), H = 1", {
  result <- rna_onesided(99, a[1], bounds[1])
  expect_equal(result, 99)

  # Check assignments.
  result_asgn <- rna_onesided(99, a[1], bounds[1], assignments = TRUE)
  expected_asgn <- list(opt = 99, take_neyman = 1, take_bound = numeric(0))
  expect_equal(result_asgn, expected_asgn)
})

### alloc: 1 bound (n = M) ----

test_that("rna_onesided works well for M (alloc: 1 bound), H = 1", {
  result <- rna_onesided(bounds[1], a[1], bounds[1])
  expect_equal(result, bounds[1])

  # Check assignments.
  result_asgn <- rna_onesided(bounds[1], a[1], bounds[1], assignments = TRUE)
  expected_asgn <- list(opt = bounds[1], take_neyman = numeric(0), take_bound = 1)
  expect_equal(result_asgn, expected_asgn)
})

## H = 4 ----

### M with Inf ----

test_that("rna_onesided works well for M with Inf, H = 4", {
  result <- rna_onesided(5000, a, c(bounds[1:3], Inf))
  expected <- c(100, 90, 70, 4740)
  expect_equal(result, expected)

  # Check assignments.
  result_asgn <- rna_onesided(5000, a, c(bounds[1:3], Inf), assignments = TRUE)
  expected_asgn <- list(opt = expected, take_neyman = 4, take_bound = 1:3)
  expect_equal(result_asgn, expected_asgn)
})

### alloc: no bounds ----

test_that("rna_onesided works well for M (alloc: no bounds), H = 4", {
  result <- rna_onesided(190, a, bounds)
  expected <- c(40.71429, 54.28571, 67.85714, 27.14286)
  expect_equal(result, expected, tolerance = 1e-7)

  # Check assignments.
  result_asgn <- rna_onesided(190, a, bounds, assignments = TRUE)
  expected_asgn <- list(opt = expected, take_neyman = 1:4, take_bound = numeric(0))
  expect_equal(result_asgn, expected_asgn, tolerance = 1e-7)
})

### alloc: 1 bound ----

test_that("rna_onesided works well for M (alloc: 1 bound), H = 4", {
  result <- rna_onesided(270, a, bounds)
  expected <- c(66.66667, 88.88889, 70, 44.44444)
  expect_equal(result, expected, tolerance = 1e-7)

  # Check assignments.
  result_asgn <- rna_onesided(270, a, bounds, assignments = TRUE)
  expected_asgn <- list(opt = expected, take_neyman = c(1, 2, 4), take_bound = 3)
  expect_equal(result_asgn, expected_asgn, tolerance = 1e-7)
})

### alloc: 2 bounds ----

test_that("rna_onesided works well for M (alloc: 2 bounds), H = 4", {
  result <- rna_onesided(300, a, bounds)
  expected <- c(84, 90, 70, 56)
  expect_equal(result, expected)

  # Check assignments.
  result_asgn <- rna_onesided(300, a, bounds, assignments = TRUE)
  expected_asgn <- list(opt = expected, take_neyman = c(1, 4), take_bound = 2:3)
  expect_equal(result_asgn, expected_asgn)
})

### alloc: 3 bounds ----

test_that("rna_onesided works well for M (alloc: 3 bounds), H = 4", {
  result <- rna_onesided(330, a, bounds)
  expected <- c(100, 90, 70, 70)
  expect_equal(result, expected)

  # Check assignments.
  result_asgn <- rna_onesided(330, a, bounds, assignments = TRUE)
  expected_asgn <- list(opt = expected, take_neyman = 4, take_bound = 1:3)
  expect_equal(result_asgn, expected_asgn)
})

### alloc: 4 bounds (n = sum(M)) ----

test_that("rna_onesided works well for n = sum(M), H = 4", {
  result <- rna_onesided(340, a, bounds)
  expect_equal(result, bounds)

  # Check assignments.
  result_asgn <- rna_onesided(340, a, bounds, assignments = TRUE)
  expected_asgn <- list(opt = bounds, take_neyman = numeric(0), take_bound = 1:4)
  expect_equal(result_asgn, expected_asgn)
})
