M <- c(300, 400, 200, 90)

# ASSERTIONS ----

## function ----

test_that("opt is valid function", {
  expect_function(opt, args = c("n", "a", "m", "M", "M_algorithm"))
})

## a ----

test_that("opt throws error when a is not valid", {
  expect_error(
    opt(350, c("1", "2")),
    "Assertion on 'a' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    opt(350, c(-2, 1, 4)),
    "Assertion on 'all\\(a > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    opt(350, c(0, 1, 4)),
    "Assertion on 'all\\(a > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    opt(350, c(1, 2, Inf)),
    "Assertion on 'a' failed: Must be finite."
  )
  expect_error(
    opt(350, c(1, 2, NA)),
    "Assertion on 'a' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    opt(350, NULL),
    "Assertion on 'a' failed: Must be of type 'numeric', not 'NULL'."
  )
  expect_error(
    opt(350, numeric(0)),
    "Assertion on 'a' failed: Must have length >= 1, but has length 0."
  )
})

## m ----

test_that("opt throws error when m is not valid", {
  expect_error(
    opt(350, a, c("1", "2")),
    "Assertion on 'm' failed: Must be of type 'numeric' \\(or 'NULL'\\), not 'character'."
  )
  expect_error(
    opt(350, a, 1),
    "Assertion on 'm' failed: Must have length 4, but has length 1."
  )
  expect_error(
    opt(350, a, c(-2, 1, 4)),
    "Assertion on 'm' failed: Must have length 4, but has length 3."
  )
  expect_error(
    opt(350, a, c(-2, 1, 4, 7, 8)),
    "Assertion on 'm' failed: Must have length 4, but has length 5."
  )
  expect_error(
    opt(350, a, c(-2, 1, 4, 6)),
    "Assertion on 'all\\(m > 0\\)' failed: Must be TRUE"
  )
  expect_error(
    opt(350, a, c(0, 1, 4, 6)),
    "Assertion on 'all\\(m > 0\\)' failed: Must be TRUE"
  )
  expect_error(
    opt(350, a, c(1, 2, Inf, 5)),
    "Assertion on 'm' failed: Must be finite."
  )
  expect_error(
    opt(350, a, c(1, 2, NA, 6)),
    "Assertion on 'm' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    opt(350, a, numeric(0)),
    "Assertion on 'm' failed: Must have length 4, but has length 0."
  )
})

## M ----

test_that("opt throws error when M is not valid", {
  expect_error(
    opt(350, a, M = c("1", "2")),
    "Assertion on 'M' failed: Must be of type 'numeric' \\(or 'NULL'\\), not 'character'."
  )
  expect_error(
    opt(350, a, M = 1),
    "Assertion on 'M' failed: Must have length 4, but has length 1."
  )
  expect_error(
    opt(350, a, M = c(-2, 1, 4)),
    "Assertion on 'M' failed: Must have length 4, but has length 3."
  )
  expect_error(
    opt(350, a, M = c(-2, 1, 4, 7, 8)),
    "Assertion on 'M' failed: Must have length 4, but has length 5."
  )
  expect_error(
    opt(350, a, M = c(-2, 1, 4, 6)),
    "Assertion on 'all\\(M > 0\\)' failed: Must be TRUE"
  )
  expect_error(
    opt(350, a, M = c(0, 1, 4, 6)),
    "Assertion on 'all\\(M > 0\\)' failed: Must be TRUE"
  )
  expect_error(
    opt(350, a, M = c(1, 2, NA, 6)),
    "Assertion on 'M' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    opt(350, a, M = numeric(0)),
    "Assertion on 'M' failed: Must have length 4, but has length 0."
  )
})

## M_algorithm ----

test_that("opt throws error when M_algorithm is not valid", {
  expect_error(
    opt(350, a, M = M, M_algorithm = 1),
    "Assertion on 'M_algorithm' failed: Must be of type 'string', not 'double'."
  )
  expect_error(
    opt(350, a, M = M, M_algorithm = character(0)),
    "Assertion on 'M_algorithm' failed: Must have length 1."
  )
  expect_error(
    opt(350, a, M = M, M_algorithm = c("rna", "sga")),
    "Assertion on 'M_algorithm' failed: Must have length 1."
  )
  expect_error(
    opt(350, a, M = M, M_algorithm = NA),
    "Assertion on 'M_algorithm' failed: May not be NA."
  )
  expect_error(
    opt(350, a, M = M, M_algorithm = NULL),
    "Assertion on 'M_algorithm' failed: Must be of type 'string', not 'NULL'."
  )
  expect_error(
    opt(350, a, M = M, M_algorithm = "rnaw"),
    "Assertion on 'M_algorithm' failed: Must be a subset of \\{'rna','sga','sgaplus','coma'\\}"
  )
})

test_that("opt proprely ignores M_algorithm, even if not valid, no m, no M specified", {
  expect_silent(
    opt(200, a[1], M_algorithm = 1)
  )
  expect_silent(
    opt(200, a[1], M_algorithm = "rnaw")
  )
  expect_silent(
    opt(350, a, M_algorithm = 1)
  )
  expect_silent(
    opt(350, a, M_algorithm = "rnaw")
  )
})

test_that("opt proprely ignores M_algorithm, even if not valid, m specified, no M", {
  expect_silent(
    opt(200, a[1], m[1], M_algorithm = 1)
  )
  expect_silent(
    opt(200, a[1], m[1], M_algorithm = "rnaw")
  )
  expect_silent(
    opt(350, a, m, M_algorithm = 1)
  )
  expect_silent(
    opt(350, a, m, M_algorithm = "rnaw")
  )
})

test_that("opt proprely ignores M_algorithm, even if not valid, no m, M specified", {
  expect_silent(
    opt(200, a[1], M = M[1], M_algorithm = 1)
  )
  expect_silent(
    opt(200, a[1], M = M[1], M_algorithm = "rnaw")
  )
  expect_silent(
    opt(sum(M), a, m, M = M, M_algorithm = 1)
  )
  expect_silent(
    opt(sum(M), a, m, M = M, M_algorithm = "rnaw")
  )
})

test_that("opt proprely ignores M_algorithm, even if not valid, m and M specified", {
  expect_silent(
    opt(200, a[1], m[1], M[1], M_algorithm = 1)
  )
  expect_silent(
    opt(200, a[1], m[1], M[1], M_algorithm = "rnaw")
  )
  expect_silent(
    opt(350, a, m = m, M = M, M_algorithm = 1)
  )
  expect_silent(
    opt(350, a, m = m, M = M, M_algorithm = "rnaw")
  )
})


# NEITHER `m` NOR `M` IS SPECIFIED ----

## H = 1 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when neither m nor M, and H = 1", {
  expect_error(
    opt("1", a[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-3, a[1]),
    "Assertion on 'n > 0' failed: Must be TRUE"
  )
  expect_error(
    opt(Inf, a[1]),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, a[1]),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, a[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), a[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), a[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### valid n ----

test_that("opt works well when neither m nor M is specified, H = 1", {
  result <- opt(896, a[1])
  expect_equal(result, 896)
})

## H = 4 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when neither m nor M, and H = 4", {
  expect_error(
    opt("1", a),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-3, a),
    "Assertion on 'n > 0' failed: Must be TRUE"
  )
  expect_error(
    opt(Inf, a),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, a),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, a),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), a),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), a),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### valid n ----

test_that("opt works well when neither m nor M is specified, H = 4", {
  result <- opt(896, a)
  expect_equal(result, c(192, 256, 320, 128))
})

# BOTH `m` AND `M` ARE SPECIFIED ----

## H = 1 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when m and M, H = 1", {
  expect_error(
    opt("1", a[1], m[1], M[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-101, a[1], m[1], M[1]),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
  expect_error(
    opt(Inf, a[1], m[1], M[1]),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, a[1], m[1], M[1]),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, a[1], m[1], M[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), m[1], M[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), a[1], m[1], M[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n < m (error) ----

test_that("opt throws error when m and M, H = 1, n < m", {
  expect_error(
    opt(99, a[1], m[1], M[1]),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
})

### n > M (error) ----

test_that("opt throws error when m and M, H = 1, n > M", {
  expect_error(
    opt(301, a[1], m[1], M[1]),
    "Assertion on 'n <= sum\\(M\\)' failed: Must be TRUE."
  )
})

### n = m ----

test_that("opt works well for m and M, H = 1 and n = m", {
  result <- opt(m[1], a[1], m[1], M[1])
  expect_equal(result, m[1])
})

### n = M ----

test_that("opt works well for m and M, H = 1 and n = M", {
  result <- opt(M[1], a[1], m[1], M[1])
  expect_equal(result, M[1])
})

### m < n < M ----

test_that("opt works well for m and M, H = 1", {
  result <- opt(150, a[1], m = m[1], M = M[1])
  expect_equal(result, 150)
})

## H = 4 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when m and M, H = 4", {
  expect_error(
    opt("1", a, m, M),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-350, a, m, M),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
  expect_error(
    opt(Inf, a, m, M),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, a, m, M),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, a, m, M),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), a, m, M),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), a, m, M),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n < sum(m) (error) ----

test_that("opt throws error when m and M, H = 4, n < sum(m)", {
  expect_error(
    opt(309, a, m, M),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
})

### n > sum(M) (error) ----

test_that("opt throws error when m and M, H = 4, n > sum(M)", {
  expect_error(
    opt(991, a, m, M),
    "Assertion on 'n <= sum\\(M\\)' failed: Must be TRUE."
  )
})

### n = sum(m) ----

test_that("opt works well for m and M, H = 4 and n = sum(m)", {
  result <- opt(310, a, m, M)
  expect_equal(result, m)
})

### n = sum(M) ----

test_that("opt works well for m and M, H = 4 and n = sum(M)", {
  result <- opt(990, a, m, M)
  expect_equal(result, M)
})

### sum(m) < n < sum(M) ----

test_that("opt works well for m and M, H = 4", {
  result <- opt(976, a, m, M)
  expected <- c(294, 392, M[3:4])
  expect_equal(result, expected)
})

# ONLY `m` IS SPECIFIED ----

## H = 1 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when m, H = 1", {
  expect_error(
    opt("1", a[1], m[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-101, a[1], m[1]),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
  expect_error(
    opt(Inf, a[1], m[1]),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, a[1], m[1]),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, a[1], m[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), a[1], m[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), a[1], m[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n < m (error) ----

test_that("opt throws error when m, H = 1, n < m", {
  expect_error(
    opt(99, a[1], m[1]),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
})

### n = m ----

test_that("opt works well for m, H = 1 and n = m", {
  result <- opt(m[1], a[1], m[1])
  expect_equal(result, m[1])
})

### n > m ----

test_that("opt works well for m, H = 1 and n > m", {
  result <- opt(101, a[1], m[1])
  expect_equal(result, 101)
})

## H = 4 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when m, H = 4", {
  expect_error(
    opt("1", a, m),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-350, a, m),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
  expect_error(
    opt(Inf, a, m),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, a, m),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, a, m),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), a, m),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), a, m),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n < sum(m) (error) ----

test_that("opt throws error when m, H = 4, n < sum(m)", {
  expect_error(
    opt(309, a, m),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
})

### n = sum(m) ----

test_that("opt works well for m, H = 4 and n = sum(m)", {
  result <- opt(310, a, m)
  expect_equal(result, m)
})

### n > sum(m) ----

test_that("opt works well for m, H = 4 and n > sum(m)", {
  result <- opt(490, a, m)
  expect_equal(result, c(105, 140, 175, 70))
})

# ONLY `M` IS SPECIFIED ----

## H = 1 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when M, H = 1", {
  expect_error(
    opt("1", a[1], M = M[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-101, a[1], M = M[1]),
    "Assertion on 'n > 0' failed: Must be TRUE."
  )
  expect_error(
    opt(Inf, a[1], M = M[1]),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(Inf, a[1], M = Inf),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, a[1], M = M[1]),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, a[1], M = M[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), a[1], M = M[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), a[1], M = M[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n > M (error) ----

test_that("opt throws error when M, H = 1, n > M", {
  expect_error(
    opt(301, a[1], M = M[1]),
    "Assertion on 'n <= sum\\(M\\)' failed: Must be TRUE."
  )
})

### n = M ----

test_that("opt works well for M, H = 1 and n = M", {
  result <- opt(M[1], a[1], M = M[1])
  expect_equal(result, M[1])
})

### n < M ----

test_that("opt works well for M, H = 1 and n < M", {
  result <- opt(299, a[1], M = M[1])
  expect_equal(result, 299)
})

test_that("opt works well for M = Inf, H = 1", {
  result <- opt(5000, a[1], M = Inf)
  expect_equal(result, 5000)
})

## H = 4 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when M, H = 4", {
  expect_error(
    opt("1", a, M = M),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-101, a, M = M),
    "Assertion on 'n > 0' failed: Must be TRUE."
  )
  expect_error(
    opt(Inf, a, M = M),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(Inf, a, M = c(M[1:3], Inf)),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, a, M = M),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, a, M = M),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), a, M = M),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), a, M = M),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n > sum(M) (error) ----

test_that("opt throws error when M, H = 4, n > sum(M)", {
  expect_error(
    opt(991, a, M = M),
    "Assertion on 'n <= sum\\(M\\)' failed: Must be TRUE."
  )
})

### n = sum(M) ----

test_that("opt works well for M, H = 4 and n = sum(M)", {
  result <- opt(990, a, M = M)
  expect_equal(result, M)
})

### n < sum(M) ----

test_that("opt works well for M, H = 4 and n < sum(M)", {
  result <- opt(490, a, M = M)
  expect_equal(result, c(105, 140, 175, 70))
})

test_that("opt works well for M with Inf, H = 4", {
  result <- opt(5000, a, M = c(M[1:3], Inf))
  expect_equal(result, c(M[1:3], 4100))
})

test_that("opt works well for M, H = 4 and n < sum(M), [sga]", {
  result <- opt(490, a, M = M, M_algorithm = "sga")
  expect_equal(result, c(105, 140, 175, 70))
})

test_that("opt works well for M with Inf, H = 4, [sga]", {
  result <- opt(5000, a, M = c(M[1:3], Inf), M_algorithm = "sga")
  expect_equal(result, c(M[1:3], 4100))
})

test_that("opt works well for M, H = 4 and n < sum(M), [sgaplus]", {
  result <- opt(490, a, M = M, M_algorithm = "sgaplus")
  expect_equal(result, c(105, 140, 175, 70))
})

test_that("opt works well for M with Inf, H = 4, [sgaplus]", {
  result <- opt(5000, a, M = c(M[1:3], Inf), M_algorithm = "sgaplus")
  expect_equal(result, c(M[1:3], 4100))
})

test_that("opt works well for M, H = 4 and n < sum(M), [coma]", {
  result <- opt(490, a, M = M, M_algorithm = "coma")
  expect_equal(result, c(105, 140, 175, 70))
})

test_that("opt works well for M with Inf, H = 4, [coma]", {
  result <- opt(5000, a, M = c(M[1:3], Inf), M_algorithm = "coma")
  expect_equal(result, c(M[1:3], 4100))
})
