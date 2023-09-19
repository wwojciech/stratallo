unit_costs <- c(0.4, 5, 1.5, 10)

# Assertions ----

## function ----

test_that("optcost is valid function", {
  expect_function(optcost, args = c("V", "A", "A0", "M", "unit_costs"))
})

## a ----

test_that("optcost throws error when a is not valid", {
  expect_error(
    optcost(350, c("1", "2"), A0),
    "Assertion on 'A' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    optcost(350, c(-2, 1, 4), A0),
    "Assertion on 'all\\(A > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(350, c(0, 1, 4), A0),
    "Assertion on 'all\\(A > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(350, c(1, 2, Inf), A0),
    "Assertion on 'A' failed: Must be finite."
  )
  expect_error(
    optcost(350, c(1, 2, NA), A0),
    "Assertion on 'A' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    optcost(350, NULL, A0),
    "Assertion on 'A' failed: Must be of type 'numeric', not 'NULL'."
  )
  expect_error(
    optcost(350, numeric(0), A0),
    "Assertion on 'A' failed: Must have length >= 1, but has length 0."
  )
})

## A0 ----

test_that("optcost throws error when A0 is not valid", {
  expect_error(
    optcost(350, A, c("1", "2")),
    "Assertion on 'A0' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    optcost(350, A, 1:2),
    "Assertion on 'A0' failed: Must have length 1."
  )
  expect_error(
    optcost(350, A, NA),
    "Assertion on 'A0' failed: May not be NA."
  )
  expect_error(
    optcost(350, A, NULL),
    "Assertion on 'A0' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    optcost(350, A, Inf),
    "Assertion on 'A0' failed: Must be finite."
  )
})

## M ----

test_that("optcost throws error when M is not valid", {
  expect_error(
    optcost(350, A, A0, c("1", "2")),
    "Assertion on 'M' failed: Must be of type 'numeric' \\(or 'NULL'\\), not 'character'."
  )
  expect_error(
    optcost(350, A, A0, 4),
    "Assertion on 'M' failed: Must have length 4, but has length 1."
  )
  expect_error(
    optcost(350, A, A0, c(-2, 1, 4)),
    "Assertion on 'M' failed: Must have length 4, but has length 3."
  )
  expect_error(
    optcost(350, A, A0, c(-2, 1, 4, 7, 8)),
    "Assertion on 'M' failed: Must have length 4, but has length 5."
  )
  expect_error(
    optcost(350, A, A0, c(-2, 1, 4, 6)),
    "Assertion on 'all\\(M > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(350, A, A0, c(0, 1, 4, 6)),
    "Assertion on 'all\\(M > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(350, A, A0, c(1, 2, NA, 6)),
    "Assertion on 'M' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    optcost(350, A, A0, numeric(0)),
    "Assertion on 'M' failed: Must have length 4, but has length 0."
  )
})

## unit_costs ----

test_that("optcost throws error when unit_costs is not valid", {
  expect_error(
    optcost(350, A, A0, M, c("1", "2", "3", "4")),
    "Assertion on 'unit_costs' failed: Must be of type 'numeric' \\(or 'NULL'\\), not 'character'."
  )
  expect_error(
    optcost(7e5, A, A0, M, c(2, 1, 4)),
    "Assertion on 'length\\(unit_costs\\) == 1L || length\\(unit_costs\\) == length\\(A\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(7e5, A, A0, M, c(2, 1, 4, 5, 9)),
    "Assertion on 'length\\(unit_costs\\) == 1L || length\\(unit_costs\\) == length\\(A\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(7e5, A, A0, M, c(-2, 1, 4, 6)),
    "Assertion on 'all\\(unit_costs > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(7e5, A, A0, M, c(0, 1, 4, 6)),
    "Assertion on 'all\\(unit_costs > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    optcost(7e5, A, A0, M, c(1, 2, NA, 6)),
    "Assertion on 'unit_costs' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    optcost(7e5, A, A0, M, numeric(0)),
    "Assertion on 'length\\(unit_costs\\) == 1L || length\\(unit_costs\\) == length\\(A\\)' failed: Must be TRUE."
  )
})

# M not specified ----

## H = 1 ----

### Assertions on V ----

test_that("optcost throws error for non valid V, no M, H = 1", {
  expect_error(
    optcost("1", A[1], A0),
    "Assertion on 'V' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    optcost(-3, A[1], A0),
    "Assertion on 'V > 0' failed: Must be TRUE."
  )
  expect_error(
    optcost(0, A[1], A0),
    "Assertion on 'V > 0' failed: Must be TRUE."
  )
  expect_error(
    optcost(Inf, A[1], A0),
    "Assertion on 'V' failed: Must be finite."
  )
  expect_error(
    optcost(NA, A[1], A0),
    "Assertion on 'V' failed: May not be NA."
  )
  expect_error(
    optcost(NULL, A[1], A0),
    "Assertion on 'V' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    optcost(numeric(0), A[1], A0),
    "Assertion on 'V' failed: Must have length 1."
  )
  expect_error(
    optcost(c(1, 5), A[1], A0),
    "Assertion on 'V' failed: Must have length 1."
  )
})

### valid V ----

test_that("optcost works well when M is not specified, H = 1", {
  result <- optcost(2000, A[1], A0)
  expect_equal(result, 125)
})

test_that("optcost works well when M is not specified, H = 1, non-def cost", {
  result <- optcost(2000, A[1], A0, unit_costs = unit_costs[1])
  expect_equal(result, 125)
})

## H = 4 ----

### Assertions on V ----

test_that("optcost throws error for non valid V, no M, H = 4", {
  expect_error(
    optcost("1", A, A0),
    "Assertion on 'V' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    optcost(-3, A, A0),
    "Assertion on 'V > 0' failed: Must be TRUE."
  )
  expect_error(
    optcost(0, A, A0),
    "Assertion on 'V > 0' failed: Must be TRUE."
  )
  expect_error(
    optcost(Inf, A, A0),
    "Assertion on 'V' failed: Must be finite."
  )
  expect_error(
    optcost(NA, A, A0),
    "Assertion on 'V' failed: May not be NA."
  )
  expect_error(
    optcost(NULL, A, A0),
    "Assertion on 'V' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    optcost(numeric(0), A, A0),
    "Assertion on 'V' failed: Must have length 1."
  )
  expect_error(
    optcost(c(1, 5), A, A0),
    "Assertion on 'V' failed: Must have length 1."
  )
})

### valid V ----

test_that("optcost works well when M is not specified, H = 4", {
  result <- optcost(1500, A, A0)
  expect_equal(result, c(587.4126, 783.2168, 979.0210, 391.6084), tolerance = 1e-7)
})

test_that("optcost works well when M is not specified, H = 4, non-def cost", {
  result <- optcost(1500, A, A0, unit_costs = unit_costs[1])
  expect_equal(result, c(587.4126, 783.2168, 979.0210, 391.6084), tolerance = 1e-7)
})

test_that("optcost works well when M is not specified, H = 4, non-def cost vec", {
  result <- optcost(1500, A, A0, unit_costs = unit_costs)
  expect_equal(result, c(1545.0879, 582.6891, 1329.7999, 206.0117), tolerance = 1e-7)
})

# M specified ----

## H = 1 ----

### Assertion on variance ----

test_that("optcost throws error for non-positive variance, M specified, H = 1", {
  expect_error(
    optcost(100, A[1], sum(A[1]^2 / M[1]) + 1, M[1]),
    "Assertion on 'sum\\(A2/M\\) - A0 > 0' failed: Must be TRUE."
  )
})

test_that("optcost throws error for non-positive variance, M = Inf, H = 1", {
  expect_error(
    optcost(sum(A[1]^2 / Inf) - A0, A[1], A0, Inf),
    "Assertion on 'sum\\(A2/M\\) - A0 > 0' failed: Must be TRUE."
  )
})

### Assertions on V ----

test_that("optcost throws error for non valid V, M specified, H = 1", {
  expect_error(
    optcost("1", A[1], A0, M[1]),
    "Assertion on 'V' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    optcost(-101, A[1], A0, M[1]),
    "Assertion on 'V >= sum\\(A2/M\\) - A0' failed: Must be TRUE."
  )
  expect_error(
    optcost(Inf, A[1], A0, M[1]),
    "Assertion on 'V' failed: Must be finite."
  )
  expect_error(
    optcost(Inf, A[1], -10, Inf),
    "Assertion on 'V' failed: Must be finite."
  )
  expect_error(
    optcost(NA, A[1], A0, M[1]),
    "Assertion on 'V' failed: May not be NA."
  )
  expect_error(
    optcost(NULL, A[1], A0, M[1]),
    "Assertion on 'V' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    optcost(numeric(0), A[1], A0, M[1]),
    "Assertion on 'V' failed: Must have length 1."
  )
  expect_error(
    optcost(c(1, 5), A[1], A0, M[1]),
    "Assertion on 'V' failed: Must have length 1."
  )
})

### V = A^2/M - A0 ----

test_that("optcost works well for M, H = 1 and V = A^2/M - A0", {
  result <- optcost(A[1]^2 / M[1] - A0, A[1], A0, M[1])
  expect_equal(result, M[1])
})

test_that("optcost works well for M, H = 1 and V = A^2/M - A0, non-def cost", {
  result <- optcost(A[1]^2 / M[1] - A0, A[1], A0, M[1], unit_costs[1])
  expect_equal(result, M[1])
})

### V > A^2/M - A0 ----

test_that("optcost works well for M, H = 1 and V > A^2/M - A0", {
  result <- optcost(30010, A[1], A0, M[1])
  expect_equal(result, 89.991)
})

test_that("optcost works well for M = Inf, H = 1 and V > A^2/M - A0", {
  result <- optcost(30010, A[1], -10, Inf)
  expect_equal(result, 300)
})

test_that("optcost works well for M, H = 1 and V > A^2/M - A0, non-def cost", {
  result <- optcost(30010, A[1], A0, M[1], unit_costs[1])
  expect_equal(result, 89.991)
})

## H = 4 ----

### Assertion on variance ----

test_that("optcost throws error for non-positive variance, M specified, H = 4", {
  expect_error(
    optcost(100, A, sum(A^2 / M), M),
    "Assertion on 'sum\\(A2/M\\) - A0 > 0' failed: Must be TRUE."
  )
})

### Assertions on V ----

test_that("optcost throws error for non valid V, M specified, H = 4", {
  expect_error(
    optcost("1", A, A0, M),
    "Assertion on 'V' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    optcost(-101, A, A0, M),
    "Assertion on 'V >= sum\\(A2/M\\) - A0' failed: Must be TRUE."
  )
  expect_error(
    optcost(Inf, A, A0, M),
    "Assertion on 'V' failed: Must be finite."
  )
  expect_error(
    optcost(Inf, A, A0, c(M[1:3], Inf)),
    "Assertion on 'V' failed: Must be finite."
  )
  expect_error(
    optcost(NA, A, A0, M),
    "Assertion on 'V' failed: May not be NA."
  )
  expect_error(
    optcost(NULL, A, A0, M),
    "Assertion on 'V' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    optcost(numeric(0), A, A0, M),
    "Assertion on 'V' failed: Must have length 1."
  )
  expect_error(
    optcost(c(1, 5), A, A0, M),
    "Assertion on 'V' failed: Must have length 1."
  )
})

### V = sum(A^2/M) - A0 ----

test_that("optcost works well for M, H = 4 and V = sum(A^2/M) - A0", {
  result <- optcost(sum(A^2 / M) - A0, A, A0, M)
  expect_equal(result, M)
})

test_that("optcost works well for M with Inf, H = 4 and V = sum(A^2/M) - A0", {
  result <- optcost(sum(A^2 / M) - A0, A, A0, c(M[1:3], Inf))
  expect_equal(result, M)
})

test_that("optcost works well for M, H = 4 and V = sum(A^2/M) - A0, non-def cost", {
  result <- optcost(sum(A^2 / M) - A0, A, A0, M, unit_costs[1])
  expect_equal(result, M)
})

test_that("optcost works well for M, H = 4 and V = sum(A^2/M) - A0, non-def cost vec", {
  result <- optcost(sum(A^2 / M) - A0, A, A0, M, unit_costs)
  expect_equal(result, M)
})

### V > sum(A^2/M) - A0 ----

test_that("optcost works well for M, H = 4 and V > sum(A^2/M) - A0", {
  result <- optcost(700000, A, A0, M)
  expect_equal(result, c(65.39792, 87.19723, M[3], 43.59862), tolerance = 1e-7)
})

test_that("optcost works well for M with Inf, H = 4 and V > sum(A^2/M) - A0", {
  result <- optcost(560000, A, A0, c(M[1:3], Inf))
  expect_equal(result, c(M[1:3], 787.5))
})

test_that("optcost works well for M, H = 4 and V > sum(A^2/M) - A0, non-def cost", {
  result <- optcost(700000, A, A0, M, unit_costs[1])
  expect_equal(result, c(65.39792, 87.19723, M[3], 43.59862), tolerance = 1e-7)
})

test_that("optcost works well for M, H = 4 and V > sum(A^2/M) - A0, non-def cost vec", {
  result <- optcost(700000, A, A0, M, unit_costs)
  expect_equal(result, c(M[1], 84.59998, M[3], 29.91061), tolerance = 1e-7)
})
