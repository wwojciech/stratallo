unit_costs <- c(0.4, 5, 1.5, 10)

# Assertions ----

## function ----

test_that("optcost is valid function", {
  expect_function(optcost, args = c("V", "a", "a0", "M", "unit_costs"))
})

## a ----

test_that("optcost throws error when a is not valid", {
  expect_error(
    optcost(350, c("1", "2"), a0),
    "Assertion on 'a' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    optcost(350, c(-2, 1, 4), a0),
    "Assertion on 'all\\(a > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(350, c(0, 1, 4), a0),
    "Assertion on 'all\\(a > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(350, c(1, 2, Inf), a0),
    "Assertion on 'a' failed: Must be finite."
  )
  expect_error(
    optcost(350, c(1, 2, NA), a0),
    "Assertion on 'a' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    optcost(350, NULL, a0),
    "Assertion on 'a' failed: Must be of type 'numeric', not 'NULL'."
  )
  expect_error(
    optcost(350, numeric(0), a0),
    "Assertion on 'a' failed: Must have length >= 1, but has length 0."
  )
})

## a0 ----

test_that("optcost throws error when a0 is not valid", {
  expect_error(
    optcost(350, a, c("1", "2")),
    "Assertion on 'a0' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    optcost(350, a, 1:2),
    "Assertion on 'a0' failed: Must have length 1."
  )
  expect_error(
    optcost(350, a, NA),
    "Assertion on 'a0' failed: May not be NA."
  )
  expect_error(
    optcost(350, a, NULL),
    "Assertion on 'a0' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    optcost(350, a, Inf),
    "Assertion on 'a0' failed: Must be finite."
  )
})

## M ----

test_that("optcost throws error when M is not valid", {
  expect_error(
    optcost(350, a, a0, c("1", "2")),
    "Assertion on 'M' failed: Must be of type 'numeric' \\(or 'NULL'\\), not 'character'."
  )
  expect_error(
    optcost(350, a, a0, 4),
    "Assertion on 'M' failed: Must have length 4, but has length 1."
  )
  expect_error(
    optcost(350, a, a0, c(-2, 1, 4)),
    "Assertion on 'M' failed: Must have length 4, but has length 3."
  )
  expect_error(
    optcost(350, a, a0, c(-2, 1, 4, 7, 8)),
    "Assertion on 'M' failed: Must have length 4, but has length 5."
  )
  expect_error(
    optcost(350, a, a0, c(-2, 1, 4, 6)),
    "Assertion on 'all\\(M > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(350, a, a0, c(0, 1, 4, 6)),
    "Assertion on 'all\\(M > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(350, a, a0, c(1, 2, NA, 6)),
    "Assertion on 'M' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    optcost(350, a, a0, numeric(0)),
    "Assertion on 'M' failed: Must have length 4, but has length 0."
  )
})

## unit_costs ----

test_that("optcost throws error when unit_costs is not valid", {
  expect_error(
    optcost(350, a, a0, M, c("1", "2", "3", "4")),
    "Assertion on 'unit_costs' failed: Must be of type 'numeric' \\(or 'NULL'\\), not 'character'."
  )
  expect_error(
    optcost(7e5, a, a0, M, c(2, 1, 4)),
    "Assertion on 'length\\(unit_costs\\) == 1L || length\\(unit_costs\\) == length\\(a\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(7e5, a, a0, M, c(2, 1, 4, 5, 9)),
    "Assertion on 'length\\(unit_costs\\) == 1L || length\\(unit_costs\\) == length\\(a\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(7e5, a, a0, M, c(-2, 1, 4, 6)),
    "Assertion on 'all\\(unit_costs > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(7e5, a, a0, M, c(0, 1, 4, 6)),
    "Assertion on 'all\\(unit_costs > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(7e5, a, a0, M, c(1, 2, NA, 6)),
    "Assertion on 'unit_costs' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    optcost(7e5, a, a0, M, numeric(0)),
    "Assertion on 'length\\(unit_costs\\) == 1L || length\\(unit_costs\\) == length\\(a\\)' failed: Must be TRUE."
  )
})

# M not specified ----

## H = 1 ----

### Assertions on V ----

test_that("optcost throws error for non valid V, no M, H = 1", {
  expect_error(
    optcost("1", a[1], a0),
    "Assertion on 'V' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    optcost(-3, a[1], a0),
    "Assertion on 'V > 0' failed: Must be TRUE."
  )
  expect_error(
    optcost(0, a[1], a0),
    "Assertion on 'V > 0' failed: Must be TRUE."
  )
  expect_error(
    optcost(Inf, a[1], a0),
    "Assertion on 'V' failed: Must be finite."
  )
  expect_error(
    optcost(NA, a[1], a0),
    "Assertion on 'V' failed: May not be NA."
  )
  expect_error(
    optcost(NULL, a[1], a0),
    "Assertion on 'V' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    optcost(numeric(0), a[1], a0),
    "Assertion on 'V' failed: Must have length 1."
  )
  expect_error(
    optcost(c(1, 5), a[1], a0),
    "Assertion on 'V' failed: Must have length 1."
  )
})

### valid V ----

test_that("optcost works well when M is not specified, H = 1", {
  result <- optcost(2000, a[1], a0)
  expect_equal(result, 125)
})

test_that("optcost works well when M is not specified, H = 1, non-def cost", {
  result <- optcost(2000, a[1], a0, unit_costs = unit_costs[1])
  expect_equal(result, 125)
})

## H = 4 ----

### Assertions on V ----

test_that("optcost throws error for non valid V, no M, H = 4", {
  expect_error(
    optcost("1", a, a0),
    "Assertion on 'V' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    optcost(-3, a, a0),
    "Assertion on 'V > 0' failed: Must be TRUE."
  )
  expect_error(
    optcost(0, a, a0),
    "Assertion on 'V > 0' failed: Must be TRUE."
  )
  expect_error(
    optcost(Inf, a, a0),
    "Assertion on 'V' failed: Must be finite."
  )
  expect_error(
    optcost(NA, a, a0),
    "Assertion on 'V' failed: May not be NA."
  )
  expect_error(
    optcost(NULL, a, a0),
    "Assertion on 'V' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    optcost(numeric(0), a, a0),
    "Assertion on 'V' failed: Must have length 1."
  )
  expect_error(
    optcost(c(1, 5), a, a0),
    "Assertion on 'V' failed: Must have length 1."
  )
})

### valid V ----

test_that("optcost works well when M is not specified, H = 4", {
  result <- optcost(1500, a, a0)
  expect_equal(result, c(587.4126, 783.2168, 979.0210, 391.6084), tolerance = 1e-7)
})

test_that("optcost works well when M is not specified, H = 4, non-def cost", {
  result <- optcost(1500, a, a0, unit_costs = unit_costs[1])
  expect_equal(result, c(587.4126, 783.2168, 979.0210, 391.6084), tolerance = 1e-7)
})

test_that("optcost works well when M is not specified, H = 4, non-def cost vec", {
  result <- optcost(1500, a, a0, unit_costs = unit_costs)
  expect_equal(result, c(1545.0879, 582.6891, 1329.7999, 206.0117), tolerance = 1e-7)
})

# M specified ----

## H = 1 ----

### Assertion on variance ----

test_that("optcost throws error for non-positive variance, M specified, H = 1", {
  expect_error(
    optcost(100, a[1], sum(a[1]^2 / M[1]) + 1, M[1]),
    "Assertion on 'sum\\(a\\^2/M\\) - a0 > 0' failed: Must be TRUE."
  )
})

test_that("optcost throws error for non-positive variance, M = Inf, H = 1", {
  expect_error(
    optcost(sum(a[1]^2 / Inf) - a0, a[1], a0, Inf),
    "Assertion on 'sum\\(a\\^2/M\\) - a0 > 0' failed: Must be TRUE."
  )
})

### Assertions on V ----

test_that("optcost throws error for non valid V, M specified, H = 1", {
  expect_error(
    optcost("1", a[1], a0, M[1]),
    "Assertion on 'V' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    optcost(-101, a[1], a0, M[1]),
    "Assertion on 'V >= sum\\(a\\^2/M\\) - a0' failed: Must be TRUE."
  )
  expect_error(
    optcost(Inf, a[1], a0, M[1]),
    "Assertion on 'V' failed: Must be finite."
  )
  expect_error(
    optcost(Inf, a[1], -10, Inf),
    "Assertion on 'V' failed: Must be finite."
  )
  expect_error(
    optcost(NA, a[1], a0, M[1]),
    "Assertion on 'V' failed: May not be NA."
  )
  expect_error(
    optcost(NULL, a[1], a0, M[1]),
    "Assertion on 'V' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    optcost(numeric(0), a[1], a0, M[1]),
    "Assertion on 'V' failed: Must have length 1."
  )
  expect_error(
    optcost(c(1, 5), a[1], a0, M[1]),
    "Assertion on 'V' failed: Must have length 1."
  )
})

### V = a^2/M - a0 ----

test_that("optcost works well for M, H = 1 and V = a^2/M - a0", {
  result <- optcost(a[1]^2 / M[1] - a0, a[1], a0, M[1])
  expect_equal(result, M[1])
})

test_that("optcost works well for M, H = 1 and V = a^2/M - a0, non-def cost", {
  result <- optcost(a[1]^2 / M[1] - a0, a[1], a0, M[1], unit_costs[1])
  expect_equal(result, M[1])
})

### V > a^2/M - a0 ----

test_that("optcost works well for M, H = 1 and V > a^2/M - a0", {
  result <- optcost(30010, a[1], a0, M[1])
  expect_equal(result, 89.991)
})

test_that("optcost works well for M = Inf, H = 1 and V > a^2/M - a0", {
  result <- optcost(30010, a[1], -10, Inf)
  expect_equal(result, 300)
})

test_that("optcost works well for M, H = 1 and V > a^2/M - a0, non-def cost", {
  result <- optcost(30010, a[1], a0, M[1], unit_costs[1])
  expect_equal(result, 89.991)
})

## H = 4 ----

### Assertion on variance ----

test_that("optcost throws error for non-positive variance, M specified, H = 4", {
  expect_error(
    optcost(100, a, sum(a^2 / M), M),
    "Assertion on 'sum\\(a\\^2/M\\) - a0 > 0' failed: Must be TRUE."
  )
})

### Assertions on V ----

test_that("optcost throws error for non valid V, M specified, H = 4", {
  expect_error(
    optcost("1", a, a0, M),
    "Assertion on 'V' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    optcost(-101, a, a0, M),
    "Assertion on 'V >= sum\\(a\\^2/M\\) - a0' failed: Must be TRUE."
  )
  expect_error(
    optcost(Inf, a, a0, M),
    "Assertion on 'V' failed: Must be finite."
  )
  expect_error(
    optcost(Inf, a, a0, c(M[1:3], Inf)),
    "Assertion on 'V' failed: Must be finite."
  )
  expect_error(
    optcost(NA, a, a0, M),
    "Assertion on 'V' failed: May not be NA."
  )
  expect_error(
    optcost(NULL, a, a0, M),
    "Assertion on 'V' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    optcost(numeric(0), a, a0, M),
    "Assertion on 'V' failed: Must have length 1."
  )
  expect_error(
    optcost(c(1, 5), a, a0, M),
    "Assertion on 'V' failed: Must have length 1."
  )
})

### V = sum(a^2/M) - a0 ----

test_that("optcost works well for M, H = 4 and V = sum(a^2/M) - a0", {
  result <- optcost(sum(a^2 / M) - a0, a, a0, M)
  expect_equal(result, M)
})

test_that("optcost works well for M with Inf, H = 4 and V = sum(a^2/M) - a0", {
  result <- optcost(sum(a^2 / M) - a0, a, a0, c(M[1:3], Inf))
  expect_equal(result, M)
})

test_that("optcost works well for M, H = 4 and V = sum(a^2/M) - a0, non-def cost", {
  result <- optcost(sum(a^2 / M) - a0, a, a0, M, unit_costs[1])
  expect_equal(result, M)
})

test_that("optcost works well for M, H = 4 and V = sum(a^2/M) - a0, non-def cost vec", {
  result <- optcost(sum(a^2 / M) - a0, a, a0, M, unit_costs)
  expect_equal(result, M)
})

### V > sum(a^2/M) - a0 ----

test_that("optcost works well for M, H = 4 and V > sum(a^2/M) - a0", {
  result <- optcost(700000, a, a0, M)
  expect_equal(result, c(65.39792, 87.19723, M[3], 43.59862), tolerance = 1e-7)
})

test_that("optcost works well for M with Inf, H = 4 and V > sum(a^2/M) - a0", {
  result <- optcost(560000, a, a0, c(M[1:3], Inf))
  expect_equal(result, c(M[1:3], 787.5))
})

test_that("optcost works well for M, H = 4 and V > sum(a^2/M) - a0, non-def cost", {
  result <- optcost(700000, a, a0, M, unit_costs[1])
  expect_equal(result, c(65.39792, 87.19723, M[3], 43.59862), tolerance = 1e-7)
})

test_that("optcost works well for M, H = 4 and V > sum(a^2/M) - a0, non-def cost vec", {
  result <- optcost(700000, a, a0, M, unit_costs)
  expect_equal(result, c(M[1], 84.59998, M[3], 29.91061), tolerance = 1e-7)
})
