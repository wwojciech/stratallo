a <- c(3000, 4000, 5000, 2000)
bounds <- c(100, 90, 70, 80)

# Assertions ----

## function ----

test_that("dopt is valid function", {
  expect_function(dopt, args = c("n", "a", "m", "M", "M_method"))
})

## a ----

test_that("dopt throws error when a is not valid", {
  expect_error(
    dopt(350, c("1", "2")),
    "Assertion on 'a' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    dopt(350, c(-2, 1, 4)),
    "Assertion on 'a' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    dopt(350, c(0, 1, 4)),
    "Assertion on 'a' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    dopt(350, c(1, 2, Inf)),
    "Assertion on 'a' failed: Must be finite."
  )
  expect_error(
    dopt(350, c(1, 2, NA)),
    "Assertion on 'a' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    dopt(350, NULL),
    "Assertion on 'a' failed: Must be of type 'numeric', not 'NULL'."
  )
  expect_error(
    dopt(350, numeric(0)),
    "Assertion on 'a' failed: Must have length >= 1, but has length 0."
  )
})

## m ----

test_that("dopt throws error when m is not valid", {
  expect_error(
    dopt(350, a, c("1", "2")),
    "Assertion on 'm' failed: Must be of type 'numeric' \\(or 'NULL'\\), not 'character'."
  )
  expect_error(
    dopt(350, a, c(-2, 1, 4)),
    "Assertion on 'm' failed: Must have length 4, but has length 3."
  )
  expect_error(
    dopt(350, a, c(-2, 1, 4, 7, 8)),
    "Assertion on 'm' failed: Must have length 4, but has length 5."
  )
  expect_error(
    dopt(350, a, c(-2, 1, 4, 6)),
    "Assertion on 'm' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    dopt(350, a, c(0, 1, 4, 6)),
    "Assertion on 'm' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    dopt(350, a, c(1, 2, Inf, 5)),
    "Assertion on 'm' failed: Must be finite."
  )
  expect_error(
    dopt(350, a, c(1, 2, NA, 6)),
    "Assertion on 'm' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    dopt(350, a, numeric(0)),
    "Assertion on 'm' failed: Must have length 4, but has length 0."
  )
})

## M ----

test_that("dopt throws error when M is not valid", {
  expect_error(
    dopt(350, a, M = c("1", "2")),
    "Assertion on 'M' failed: Must be of type 'numeric' \\(or 'NULL'\\), not 'character'."
  )
  expect_error(
    dopt(350, a, M = c(-2, 1, 4)),
    "Assertion on 'M' failed: Must have length 4, but has length 3."
  )
  expect_error(
    dopt(350, a, M = c(-2, 1, 4, 7, 8)),
    "Assertion on 'M' failed: Must have length 4, but has length 5."
  )
  expect_error(
    dopt(350, a, M = c(-2, 1, 4, 6)),
    "Assertion on 'M' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    dopt(350, a, M = c(0, 1, 4, 6)),
    "Assertion on 'M' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    dopt(350, a, M = c(1, 2, NA, 6)),
    "Assertion on 'M' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    dopt(350, a, M = numeric(0)),
    "Assertion on 'M' failed: Must have length 4, but has length 0."
  )
})

## M_method ----

test_that("dopt throws error when M_method is not valid", {
  expect_error(
    dopt(350, a, M_method = 1),
    "Assertion on 'M_method' failed: Must be of type 'string', not 'double'."
  )
  expect_error(
    dopt(350, a, M_method = "wrong"),
    "Assertion on 'M_method' failed: Must be a subset of \\{'rna','sga','sgaplus','coma'\\}*."
  )
  expect_error(
    dopt(350, a, M_method = c("wrong", "wrong1", NA)),
    "Assertion on 'M_method' failed: Must have length 1."
  )
  expect_error(
    dopt(350, a, M_method = character(0)),
    "Assertion on 'M_method' failed: Must have length 1."
  )
  expect_error(
    dopt(350, a, M_method = NULL),
    "Assertion on 'M_method' failed: Must be of type 'string', not 'NULL'."
  )
  expect_error(
    dopt(350, a, M_method = NA),
    "Assertion on 'M_method' failed: May not be NA."
  )
})

# m and M both specified (error) ----

## H = 1 ----

test_that("dopt throws error when both m and M are specified, H = 1", {
  expect_error(
    dopt(350, a[1], bounds[1], bounds[1] + 20),
    "Box constraints are not supported. m and M can be both not NULL."
  )
})

## H = 4 ----

test_that("dopt throws error when both m and M are specified, H = 4", {
  expect_error(
    dopt(350, a, bounds, bounds + 100),
    "Box constraints are not supported. m and M can be both not NULL."
  )
})

# Neither m nor M is specified ----

## H = 1 ----

### Assertions on n ----

test_that("dopt throws error for non valid n, when neither m nor M, and H = 1", {
  expect_error(
    dopt("1", a[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    dopt(-3, a[1]),
    "Assertion on 'n' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    dopt(0, a[1]),
    "Assertion on 'n' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    dopt(Inf, a[1]),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    dopt(NA, a[1]),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    dopt(NULL, a[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    dopt(numeric(0), a[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    dopt(c(1, 5), a[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### valid n ----

test_that("dopt works well when neither m nor M is specified, H = 1", {
  result <- dopt(896, a[1])
  expect_equal(result, 896)
})

## H = 4 ----

### Assertions on n ----

test_that("dopt throws error for non valid n, when neither m nor M, and H = 4", {
  expect_error(
    dopt("1", a),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    dopt(-3, a),
    "Assertion on 'n' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    dopt(0, a),
    "Assertion on 'n' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    dopt(Inf, a),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    dopt(NA, a),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    dopt(NULL, a),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    dopt(numeric(0), a),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    dopt(c(1, 5), a),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### valid n ----

test_that("dopt works well when neither m nor M is specified, H = 4", {
  result <- dopt(896, a)
  expect_equal(result, c(192, 256, 320, 128))
})

# m specified ----

## H = 1 ----

### Assertions on n ----

test_that("dopt throws error for non valid n, when m, H = 1", {
  expect_error(
    dopt("1", a[1], bounds[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    dopt(-101, a[1], bounds[1]),
    "Assertion on 'n' failed: Element 1 is not >= 100."
  )
  expect_error(
    dopt(Inf, a[1], bounds[1]),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    dopt(NA, a[1], bounds[1]),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    dopt(NULL, a[1], bounds[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    dopt(numeric(0), a[1], bounds[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    dopt(c(1, 5), a[1], bounds[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n < m (error) ----

test_that("dopt throws error when m, H = 1, n < m", {
  expect_error(
    dopt(99, a[1], bounds[1]),
    "Assertion on 'n' failed: Element 1 is not >= 100."
  )
})

### n = m ----

test_that("dopt works well for m, H = 1 and n = m", {
  result <- dopt(bounds[1], a[1], bounds[1])
  expect_equal(result, bounds[1])
})

### n > m ----

test_that("dopt works well for m, H = 1 and n > m", {
  result <- dopt(101, a[1], bounds[1])
  expect_equal(result, 101)
})

## H = 4 ----

### Assertions on n ----

test_that("dopt throws error for non valid n, when m, H = 4", {
  expect_error(
    dopt("1", a, bounds),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    dopt(-350, a, bounds),
    "Assertion on 'n' failed: Element 1 is not >= 340."
  )
  expect_error(
    dopt(Inf, a, bounds),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    dopt(NA, a, bounds),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    dopt(NULL, a, bounds),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    dopt(numeric(0), a, bounds),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    dopt(c(1, 5), a, bounds),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n < sum(m) (error) ----

test_that("dopt throws error when m, H = 4, n < sum(m)", {
  expect_error(
    dopt(339, a, bounds),
    "Assertion on 'n' failed: Element 1 is not >= 340"
  )
})

### n = sum(m) ----

test_that("dopt works well for m, H = 4 and n = sum(m)", {
  result <- dopt(340, a, bounds)
  expect_equal(result, bounds)
})

### n > sum(m) ----

test_that("dopt works well for m, H = 4 and n > sum(m)", {
  result <- dopt(423, a, bounds)
  expect_equal(result, c(100, 108, 135, 80))
})

# M specified ----

## H = 1 ----

### Assertions on n ----

test_that("dopt throws error for non valid n, when M, H = 1", {
  expect_error(
    dopt("1", a[1], M = bounds[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    dopt(-101, a[1], M = bounds[1]),
    "Assertion on 'n' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    dopt(Inf, a[1], M = bounds[1]),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    dopt(Inf, a[1], M = Inf),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    dopt(NA, a[1], M = bounds[1]),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    dopt(NULL, a[1], M = bounds[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    dopt(numeric(0), a[1], M = bounds[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    dopt(c(1, 5), a[1], M = bounds[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n > M (error) ----

test_that("dopt throws error when M, H = 1, n > M", {
  expect_error(
    dopt(101, a[1], M = bounds[1]),
    "Assertion on 'n' failed: Element 1 is not <= 100."
  )
})

### n = M ----

test_that("dopt works well for M, H = 1 and n = M", {
  result <- dopt(bounds[1], a[1], M = bounds[1])
  expect_equal(result, bounds[1])
})

### n < M ----

test_that("dopt works well for M, H = 1 and n < M", {
  result <- dopt(99, a[1], M = bounds[1])
  expect_equal(result, 99)
})

test_that("dopt works well for M = Inf, H = 1", {
  result <- dopt(5000, a[1], M = Inf)
  expect_equal(result, 5000)
})

## H = 4 ----

### Assertions on n ----

test_that("dopt throws error for non valid n, when M, H = 4", {
  expect_error(
    dopt("1", a, M = bounds),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    dopt(-101, a, M = bounds),
    "Assertion on 'n' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    dopt(Inf, a, M = bounds),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    dopt(Inf, a, M = c(bounds[1:3], Inf)),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    dopt(NA, a, M = bounds),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    dopt(NULL, a, M = bounds),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    dopt(numeric(0), a, M = bounds),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    dopt(c(1, 5), a, M = bounds),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n > sum(M) (error) ----

test_that("dopt throws error when M, H = 4, n > sum(M)", {
  expect_error(
    dopt(341, a, M = bounds),
    "Assertion on 'n' failed: Element 1 is not <= 340"
  )
})

### n = sum(M) ----

test_that("dopt works well for M, H = 4 and n = sum(M)", {
  result <- dopt(340, a, M = bounds)
  expect_equal(result, bounds)
})

### n < sum(M) ----

test_that("dopt works well for M, H = 4 and n < sum(M)", {
  result <- dopt(339, a, M = bounds)
  expect_equal(result, c(100, 90, 70, 79))
})

test_that("dopt works well for M with Inf, H = 4", {
  result <- dopt(5000, a, M = c(bounds[1:3], Inf))
  expect_equal(result, c(100, 90, 70, 4740))
})
