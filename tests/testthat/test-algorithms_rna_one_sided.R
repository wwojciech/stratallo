N_pop507 <- pop507[, "N"] # nolintr
a_pop507 <- N_pop507 * pop507[, "S"]

a <- c(3000, 4000, 5000, 2000)
bounds <- c(100, 90, 70, 80)

# no bounds ----

## H = 1 ----

test_that("rna_one_sided works well when no bounds, H = 1)", {
  result <- rna_one_sided(400, a[1])
  expect_equal(result, 400)
})

## H = 4 ----

test_that("rna_one_sided works well when no bounds, H = 4)", {
  result <- rna_one_sided(400, a)
  expected <- a * 400 / sum(a)
  expect_equal(result, expected)
})

# lower ----

## pop507 test ----

n_m51 <- seq(1, 2, 0.02) * sum(N_pop507)
test_that("rna_one_sided works well for m, (pop507)", {
  result <- sapply(n_m51, rna_one_sided, a_pop507, N_pop507, FALSE)
  expect_snapshot(result)
})


## H = 1 ----

### allocation reaching no bounds ----

test_that("rna_one_sided works well for m (reaching no bound), H = 1", {
  result <- rna_one_sided(101, a[1], bounds[1], FALSE)
  expect_equal(result, 101)
})

### n = m ----

test_that("rna_one_sided works well for m (reaching 1 bound), H = 1", {
  result <- rna_one_sided(bounds[1], a[1], bounds[1], FALSE)
  expect_equal(result, bounds[1])
})

## H = 4 ----

### allocation reaching no bounds ----

test_that("rna_one_sided works well for m (reaching no bounds), H = 4", {
  result <- rna_one_sided(600, a, bounds, FALSE)
  expected <- c(128.57143, 171.42857, 214.28571, 85.71429)
  expect_equal(result, expected, tolerance = 1e-7)
})

### allocation reaching 1 bound ----

test_that("rna_one_sided works well for m (reaching 1 bound), H = 4", {
  result <- rna_one_sided(500, a, bounds, FALSE)
  expected <- c(105, 140, 175, 80)
  expect_equal(result, expected)
})

### allocation reaching 2 bounds----

test_that("rna_one_sided works well for m (reaching 2 bounds), H = 4", {
  result <- rna_one_sided(420, a, bounds, FALSE)
  expected <- c(100, 106.6667, 133.3333, 80)
  expect_equal(result, expected, tolerance = 1e-6)
})

### allocation reaching 3 bounds----

test_that("rna_one_sided works well for m (reaching 3 bounds), H = 4", {
  result <- rna_one_sided(370, a, bounds, FALSE)
  expected <- c(100, 90, 100, 80)
  expect_equal(result, expected)
})

### n = sum(m)----

test_that("rna_one_sided works well for n = sum(m), H = 4", {
  result <- rna_one_sided(340, a, bounds, FALSE)
  expected <- c(100, 90, 70, 80)
  expect_equal(result, expected)
})

# upper ----

## pop507 test ----

n_M50 <- seq(0.01, 0.99, 0.02) * sum(N_pop507) # nolintr
test_that("rna_one_sided works well for M, (pop507)", {
  result <- sapply(n_M50, rna_one_sided, a_pop507, N_pop507)
  expect_snapshot(result)
})

## H = 1 ----

### allocation reaching no bounds ----

test_that("rna_one_sided works well for M (reaching no bound), H = 1", {
  result <- rna_one_sided(99, a[1], bounds[1])
  expect_equal(result, 99)
})

### n = M ----

test_that("rna_one_sided works well for M (reaching 1 bound), H = 1", {
  result <- rna_one_sided(bounds[1], a[1], bounds[1])
  expect_equal(result, bounds[1])
})

## H = 4 ----

### M with Inf ----

test_that("rna_one_sided works well for M with Inf, H = 4", {
  result <- rna_one_sided(5000, a, c(bounds[1:3], Inf))
  expect_equal(result, c(100, 90, 70, 4740))
})

### allocation reaching no bounds ----

test_that("rna_one_sided works well for M (reaching no bounds), H = 4", {
  result <- rna_one_sided(190, a, bounds)
  expected <- c(40.71429, 54.28571, 67.85714, 27.14286)
  expect_equal(result, expected, tolerance = 1e-7)
})

### allocation reaching 1 bound ----

test_that("rna_one_sided works well for M (reaching 1 bound), H = 4", {
  result <- rna_one_sided(270, a, bounds)
  expected <- c(66.66667, 88.88889, 70, 44.44444)
  expect_equal(result, expected, tolerance = 1e-7)
})

### allocation reaching 2 bounds ----

test_that("rna_one_sided works well for M (reaching 2 bounds), H = 4", {
  result <- rna_one_sided(300, a, bounds)
  expected <- c(84, 90, 70, 56)
  expect_equal(result, expected)
})

### allocation reaching 3 bounds ----

test_that("rna_one_sided works well for M (reaching 3 bounds), H = 4", {
  result <- rna_one_sided(330, a, bounds)
  expected <- c(100, 90, 70, 70)
  expect_equal(result, expected)
})

### n = sum(M) ----

test_that("rna_one_sided works well for n = sum(M), H = 4", {
  result <- rna_one_sided(340, a, bounds)
  expect_equal(result, bounds)
})
