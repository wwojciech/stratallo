a <- c(3000, 4000, 5000, 2000)
b <- 70000
M <- c(100, 90, 70, 80)

# Assertions ----

## function ----

test_that("nopt is valid function", {
  expect_function(nopt, args = c("D", "a", "b", "M"))
})

## a ----

test_that("nopt throws error when a is not valid", {
  expect_error(
    nopt(350, c("1", "2")),
    "Assertion on 'a' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    nopt(350, c(-2, 1, 4)),
    "Assertion on 'a' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    nopt(350, c(0, 1, 4)),
    "Assertion on 'a' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    nopt(350, c(1, 2, Inf)),
    "Assertion on 'a' failed: Must be finite."
  )
  expect_error(
    nopt(350, c(1, 2, NA)),
    "Assertion on 'a' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    nopt(350, NULL),
    "Assertion on 'a' failed: Must be of type 'numeric', not 'NULL'."
  )
  expect_error(
    nopt(350, numeric(0)),
    "Assertion on 'a' failed: Must have length >= 1, but has length 0."
  )
})

## b ----

test_that("nopt throws error when b is not valid", {
  expect_error(
    nopt(350, a, c("1", "2")),
    "Assertion on 'b' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    nopt(350, a, 1:2),
    "Assertion on 'b' failed: Must have length 1."
  )
  expect_error(
    nopt(350, a, NA),
    "Assertion on 'b' failed: May not be NA."
  )
  expect_error(
    nopt(350, a, NULL),
    "Assertion on 'b' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    nopt(350, a, Inf),
    "Assertion on 'b' failed: Must be finite."
  )
})

## M ----

test_that("nopt throws error when M is not valid", {
  expect_error(
    nopt(350, a, b, c("1", "2")),
    "Assertion on 'M' failed: Must be of type 'numeric' \\(or 'NULL'\\), not 'character'."
  )
  expect_error(
    nopt(350, a, b, c(-2, 1, 4)),
    "Assertion on 'M' failed: Must have length 4, but has length 3."
  )
  expect_error(
    nopt(350, a, b, c(-2, 1, 4, 7, 8)),
    "Assertion on 'M' failed: Must have length 4, but has length 5."
  )
  expect_error(
    nopt(350, a, b, c(-2, 1, 4, 6)),
    "Assertion on 'M' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    nopt(350, a, b, c(0, 1, 4, 6)),
    "Assertion on 'M' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    nopt(350, a, b, c(1, 2, NA, 6)),
    "Assertion on 'M' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    nopt(350, a, b, numeric(0)),
    "Assertion on 'M' failed: Must have length 4, but has length 0."
  )
})

# M not specified ----

## H = 1 ----

### Assertions on D ----

test_that("nopt throws error for non valid D, no M, H = 1", {
  expect_error(
    nopt("1", a[1], b),
    "Assertion on 'D' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    nopt(-3, a[1], b),
    "Assertion on 'D' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    nopt(0, a[1], b),
    "Assertion on 'D' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    nopt(Inf, a[1], b),
    "Assertion on 'D' failed: Must be finite."
  )
  expect_error(
    nopt(NA, a[1], b),
    "Assertion on 'D' failed: May not be NA."
  )
  expect_error(
    nopt(NULL, a[1], b),
    "Assertion on 'D' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    nopt(numeric(0), a[1], b),
    "Assertion on 'D' failed: Must have length 1."
  )
  expect_error(
    nopt(c(1, 5), a[1], b),
    "Assertion on 'D' failed: Must have length 1."
  )
})

### valid D ----

test_that("nopt works well when M is not specified, H = 1", {
  result <- nopt(2000, a[1], b)
  expect_equal(result, 125)
})

## H = 4 ----

### Assertions on D ----

test_that("nopt throws error for non valid D, no M, H = 4", {
  expect_error(
    nopt("1", a, b),
    "Assertion on 'D' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    nopt(-3, a, b),
    "Assertion on 'D' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    nopt(0, a, b),
    "Assertion on 'D' failed: Element 1 is not >= 2.22507e-308."
  )
  expect_error(
    nopt(Inf, a, b),
    "Assertion on 'D' failed: Must be finite."
  )
  expect_error(
    nopt(NA, a, b),
    "Assertion on 'D' failed: May not be NA."
  )
  expect_error(
    nopt(NULL, a, b),
    "Assertion on 'D' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    nopt(numeric(0), a, b),
    "Assertion on 'D' failed: Must have length 1."
  )
  expect_error(
    nopt(c(1, 5), a, b),
    "Assertion on 'D' failed: Must have length 1."
  )
})

### valid D ----

test_that("nopt works well when M is not specified, H = 4", {
  result <- nopt(1500, a, b)
  expect_equal(result, c(587.4126, 783.2168, 979.0210, 391.6084), tolerance = 1e-7)
})

# M specified ----

## H = 1 ----

### Assertion on variance ----

test_that("nopt throws error for non-positive variance, M specified, H = 1", {
  expect_error(
    nopt(100, a[1], sum(a[1]^2 / M[1]), M[1]),
    "Assertion on 'D_min > 0' failed: Must be TRUE."
  )
})

test_that("nopt throws error for non-positive variance, M = Inf, H = 1", {
  expect_error(
    nopt(sum(a[1]^2 / Inf) - b, a[1], b, Inf),
    "Assertion on 'D_min > 0' failed: Must be TRUE."
  )
})

### Assertions on D ----

test_that("nopt throws error for non valid D, M specified, H = 1", {
  expect_error(
    nopt("1", a[1], b, M[1]),
    "Assertion on 'D' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    nopt(-101, a[1], b, M[1]),
    "Assertion on 'D' failed: Element 1 is not >= 20000."
  )
  expect_error(
    nopt(Inf, a[1], b, M[1]),
    "Assertion on 'D' failed: Must be finite."
  )
  expect_error(
    nopt(Inf, a[1], -10, Inf),
    "Assertion on 'D' failed: Must be finite."
  )
  expect_error(
    nopt(NA, a[1], b, M[1]),
    "Assertion on 'D' failed: May not be NA."
  )
  expect_error(
    nopt(NULL, a[1], b, M[1]),
    "Assertion on 'D' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    nopt(numeric(0), a[1], b, M[1]),
    "Assertion on 'D' failed: Must have length 1."
  )
  expect_error(
    nopt(c(1, 5), a[1], b, M[1]),
    "Assertion on 'D' failed: Must have length 1."
  )
})

### D = a^2/M - b ----

test_that("nopt works well for M, H = 1 and D = a^2/M - b", {
  result <- nopt(a[1]^2 / M[1] - b, a[1], b, M[1])
  expect_equal(result, M[1])
})

### D > a^2/M - b ----

test_that("nopt works well for M, H = 1 and D > a^2/M - b", {
  result <- nopt(20001, a[1], b, M[1])
  expect_equal(result, 99.99889)
})

test_that("nopt works well for M = Inf, H = 1 and D > a^2/M - b", {
  result <- nopt(20001, a[1], -10, Inf)
  expect_equal(result, 450.202591)
})

## H = 4 ----

### Assertion on variance ----

test_that("nopt throws error for non-positive variance, M specified, H = 4", {
  expect_error(
    nopt(100, a, sum(a^2 / M), M),
    "Assertion on 'D_min > 0' failed: Must be TRUE."
  )
})

### Assertions on D ----

test_that("nopt throws error for non valid D, M specified, H = 4", {
  expect_error(
    nopt("1", a, b, M),
    "Assertion on 'D' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    nopt(-101, a, b, M),
    "Assertion on 'D' failed: Element 1 is not >= 604921."
  )
  expect_error(
    nopt(Inf, a, b, M),
    "Assertion on 'D' failed: Must be finite."
  )
  expect_error(
    nopt(Inf, a, b, c(M[1:3], Inf)),
    "Assertion on 'D' failed: Must be finite."
  )
  expect_error(
    nopt(NA, a, b, M),
    "Assertion on 'D' failed: May not be NA."
  )
  expect_error(
    nopt(NULL, a, b, M),
    "Assertion on 'D' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    nopt(numeric(0), a, b, M),
    "Assertion on 'D' failed: Must have length 1."
  )
  expect_error(
    nopt(c(1, 5), a, b, M),
    "Assertion on 'D' failed: Must have length 1."
  )
})

### D = sum(a^2/M) - b ----

test_that("nopt works well for M, H = 4 and D = sum(a^2/M) - b", {
  result <- nopt(sum(a^2 / M) - b, a, b, M)
  expect_equal(result, M)
})

test_that("nopt works well for M with Inf, H = 4 and D = sum(a^2/M) - b", {
  result <- nopt(sum(a^2 / M) - b, a, b, c(M[1:3], Inf))
  expect_equal(result, c(100, 90, 70, 80))
})

### D > sum(a^2/M) - b ----

test_that("nopt works well for M, H = 4 and D > sum(a^2/M) - b", {
  result <- nopt(604921, a, b, M)
  expect_equal(result, c(100, 90, 70, 79.999416))
})

test_that("nopt works well for M with Inf, H = 4 and D > sum(a^2/M) - b", {
  result <- nopt(554922, a, b, c(M[1:3], Inf))
  expect_equal(result, c(100, 90, 70, 2930232.6))
})
