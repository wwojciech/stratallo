M <- c(300, 400, 200, 90)

# ASSERTIONS ----

## function ----

test_that("opt is valid function", {
  expect_function(opt, args = c("n", "A", "m", "M", "M_algorithm"))
})

## a ----

test_that("opt throws error when a is not valid", {
  expect_error(
    opt(350, c("1", "2")),
    "Assertion on 'A' failed: Must be of type 'numeric', not 'character'."
  )
  expect_error(
    opt(350, c(-2, 1, 4)),
    "Assertion on 'all\\(A > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    opt(350, c(0, 1, 4)),
    "Assertion on 'all\\(A > 0\\)' failed: Must be TRUE."
  )
  expect_error(
    opt(350, c(1, 2, Inf)),
    "Assertion on 'A' failed: Must be finite."
  )
  expect_error(
    opt(350, c(1, 2, NA)),
    "Assertion on 'A' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    opt(350, NULL),
    "Assertion on 'A' failed: Must be of type 'numeric', not 'NULL'."
  )
  expect_error(
    opt(350, numeric(0)),
    "Assertion on 'A' failed: Must have length >= 1, but has length 0."
  )
})

## m ----

test_that("opt throws error when m is not valid", {
  expect_error(
    opt(350, A, c("1", "2")),
    "Assertion on 'm' failed: Must be of type 'numeric' \\(or 'NULL'\\), not 'character'."
  )
  expect_error(
    opt(350, A, 1),
    "Assertion on 'm' failed: Must have length 4, but has length 1."
  )
  expect_error(
    opt(350, A, c(-2, 1, 4)),
    "Assertion on 'm' failed: Must have length 4, but has length 3."
  )
  expect_error(
    opt(350, A, c(-2, 1, 4, 7, 8)),
    "Assertion on 'm' failed: Must have length 4, but has length 5."
  )
  expect_error(
    opt(350, A, c(-2, 1, 4, 6)),
    "Assertion on 'all\\(m > 0\\)' failed: Must be TRUE"
  )
  expect_error(
    opt(350, A, c(0, 1, 4, 6)),
    "Assertion on 'all\\(m > 0\\)' failed: Must be TRUE"
  )
  expect_error(
    opt(350, A, c(1, 2, Inf, 5)),
    "Assertion on 'm' failed: Must be finite."
  )
  expect_error(
    opt(350, A, c(1, 2, NA, 6)),
    "Assertion on 'm' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    opt(350, A, numeric(0)),
    "Assertion on 'm' failed: Must have length 4, but has length 0."
  )
})

## M ----

test_that("opt throws error when M is not valid", {
  expect_error(
    opt(350, A, M = c("1", "2")),
    "Assertion on 'M' failed: Must be of type 'numeric' \\(or 'NULL'\\), not 'character'."
  )
  expect_error(
    opt(350, A, M = 1),
    "Assertion on 'M' failed: Must have length 4, but has length 1."
  )
  expect_error(
    opt(350, A, M = c(-2, 1, 4)),
    "Assertion on 'M' failed: Must have length 4, but has length 3."
  )
  expect_error(
    opt(350, A, M = c(-2, 1, 4, 7, 8)),
    "Assertion on 'M' failed: Must have length 4, but has length 5."
  )
  expect_error(
    opt(350, A, M = c(-2, 1, 4, 6)),
    "Assertion on 'all\\(M > 0\\)' failed: Must be TRUE"
  )
  expect_error(
    opt(350, A, M = c(0, 1, 4, 6)),
    "Assertion on 'all\\(M > 0\\)' failed: Must be TRUE"
  )
  expect_error(
    opt(350, A, M = c(1, 2, NA, 6)),
    "Assertion on 'M' failed: Contains missing values \\(element 3\\)."
  )
  expect_error(
    opt(350, A, M = numeric(0)),
    "Assertion on 'M' failed: Must have length 4, but has length 0."
  )
})

## M_algorithm ----

test_that("opt throws error when M_algorithm is not valid", {
  expect_error(
    opt(350, A, M = M, M_algorithm = 1),
    "Assertion on 'M_algorithm' failed: Must be of type 'string', not 'double'."
  )
  expect_error(
    opt(350, A, M = M, M_algorithm = character(0)),
    "Assertion on 'M_algorithm' failed: Must have length 1."
  )
  expect_error(
    opt(350, A, M = M, M_algorithm = c("rna", "sga")),
    "Assertion on 'M_algorithm' failed: Must have length 1."
  )
  expect_error(
    opt(350, A, M = M, M_algorithm = NA),
    "Assertion on 'M_algorithm' failed: May not be NA."
  )
  expect_error(
    opt(350, A, M = M, M_algorithm = NULL),
    "Assertion on 'M_algorithm' failed: Must be of type 'string', not 'NULL'."
  )
  expect_error(
    opt(350, A, M = M, M_algorithm = "rnaw"),
    "Assertion on 'M_algorithm' failed: Must be a subset of \\{'rna','sga','sgaplus','coma'\\}"
  )
})

test_that("opt proprely ignores M_algorithm, even if not valid, no m, no M specified", {
  expect_silent(
    opt(200, A[1], M_algorithm = 1)
  )
  expect_silent(
    opt(200, A[1], M_algorithm = "rnaw")
  )
  expect_silent(
    opt(350, A, M_algorithm = 1)
  )
  expect_silent(
    opt(350, A, M_algorithm = "rnaw")
  )
})

test_that("opt proprely ignores M_algorithm, even if not valid, m specified, no M", {
  expect_silent(
    opt(200, A[1], m[1], M_algorithm = 1)
  )
  expect_silent(
    opt(200, A[1], m[1], M_algorithm = "rnaw")
  )
  expect_silent(
    opt(350, A, m, M_algorithm = 1)
  )
  expect_silent(
    opt(350, A, m, M_algorithm = "rnaw")
  )
})

test_that("opt proprely ignores M_algorithm, even if not valid, no m, M specified", {
  expect_silent(
    opt(200, A[1], M = M[1], M_algorithm = 1)
  )
  expect_silent(
    opt(200, A[1], M = M[1], M_algorithm = "rnaw")
  )
  expect_silent(
    opt(sum(M), A, m, M = M, M_algorithm = 1)
  )
  expect_silent(
    opt(sum(M), A, m, M = M, M_algorithm = "rnaw")
  )
})

test_that("opt proprely ignores M_algorithm, even if not valid, m and M specified", {
  expect_silent(
    opt(200, A[1], m[1], M[1], M_algorithm = 1)
  )
  expect_silent(
    opt(200, A[1], m[1], M[1], M_algorithm = "rnaw")
  )
  expect_silent(
    opt(350, A, m = m, M = M, M_algorithm = 1)
  )
  expect_silent(
    opt(350, A, m = m, M = M, M_algorithm = "rnaw")
  )
})


# NEITHER `m` NOR `M` IS SPECIFIED ----

## H = 1 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when neither m nor M, and H = 1", {
  expect_error(
    opt("1", A[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-3, A[1]),
    "Assertion on 'n > 0' failed: Must be TRUE"
  )
  expect_error(
    opt(Inf, A[1]),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, A[1]),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, A[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), A[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), A[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### valid n ----

test_that("opt works well when neither m nor M is specified, H = 1", {
  result <- opt(896, A[1])
  expect_equal(result, 896)
})

## H = 4 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when neither m nor M, and H = 4", {
  expect_error(
    opt("1", A),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-3, A),
    "Assertion on 'n > 0' failed: Must be TRUE"
  )
  expect_error(
    opt(Inf, A),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, A),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, A),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), A),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), A),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### valid n ----

test_that("opt works well when neither m nor M is specified, H = 4", {
  result <- opt(896, A)
  expect_equal(result, c(192, 256, 320, 128))
})

# BOTH `m` AND `M` ARE SPECIFIED ----

## H = 1 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when m and M, H = 1", {
  expect_error(
    opt("1", A[1], m[1], M[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-101, A[1], m[1], M[1]),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
  expect_error(
    opt(Inf, A[1], m[1], M[1]),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, A[1], m[1], M[1]),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, A[1], m[1], M[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), m[1], M[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), A[1], m[1], M[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n < m (error) ----

test_that("opt throws error when m and M, H = 1, n < m", {
  expect_error(
    opt(99, A[1], m[1], M[1]),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
})

### n > M (error) ----

test_that("opt throws error when m and M, H = 1, n > M", {
  expect_error(
    opt(301, A[1], m[1], M[1]),
    "Assertion on 'n <= sum\\(M\\)' failed: Must be TRUE."
  )
})

### n = m ----

test_that("opt works well for m and M, H = 1 and n = m", {
  result <- opt(m[1], A[1], m[1], M[1])
  expect_equal(result, m[1])
})

### n = M ----

test_that("opt works well for m and M, H = 1 and n = M", {
  result <- opt(M[1], A[1], m[1], M[1])
  expect_equal(result, M[1])
})

### m < n < M ----

test_that("opt works well for m and M, H = 1", {
  result <- opt(150, A[1], m = m[1], M = M[1])
  expect_equal(result, 150)
})

## H = 4 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when m and M, H = 4", {
  expect_error(
    opt("1", A, m, M),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-350, A, m, M),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
  expect_error(
    opt(Inf, A, m, M),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, A, m, M),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, A, m, M),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), A, m, M),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), A, m, M),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n < sum(m) (error) ----

test_that("opt throws error when m and M, H = 4, n < sum(m)", {
  expect_error(
    opt(309, A, m, M),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
})

### n > sum(M) (error) ----

test_that("opt throws error when m and M, H = 4, n > sum(M)", {
  expect_error(
    opt(991, A, m, M),
    "Assertion on 'n <= sum\\(M\\)' failed: Must be TRUE."
  )
})

### n = sum(m) ----

test_that("opt works well for m and M, H = 4 and n = sum(m)", {
  result <- opt(310, A, m, M)
  expect_equal(result, m)
})

### n = sum(M) ----

test_that("opt works well for m and M, H = 4 and n = sum(M)", {
  result <- opt(990, A, m, M)
  expect_equal(result, M)
})

### sum(m) < n < sum(M) ----

test_that("opt works well for m and M, H = 4", {
  result <- opt(976, A, m, M)
  expected <- c(294, 392, M[3:4])
  expect_equal(result, expected)
})

# ONLY `m` IS SPECIFIED ----

## H = 1 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when m, H = 1", {
  expect_error(
    opt("1", A[1], m[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-101, A[1], m[1]),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
  expect_error(
    opt(Inf, A[1], m[1]),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, A[1], m[1]),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, A[1], m[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), A[1], m[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), A[1], m[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n < m (error) ----

test_that("opt throws error when m, H = 1, n < m", {
  expect_error(
    opt(99, A[1], m[1]),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
})

### n = m ----

test_that("opt works well for m, H = 1 and n = m", {
  result <- opt(m[1], A[1], m[1])
  expect_equal(result, m[1])
})

### n > m ----

test_that("opt works well for m, H = 1 and n > m", {
  result <- opt(101, A[1], m[1])
  expect_equal(result, 101)
})

## H = 4 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when m, H = 4", {
  expect_error(
    opt("1", A, m),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-350, A, m),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
  expect_error(
    opt(Inf, A, m),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, A, m),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, A, m),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), A, m),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), A, m),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n < sum(m) (error) ----

test_that("opt throws error when m, H = 4, n < sum(m)", {
  expect_error(
    opt(309, A, m),
    "Assertion on 'n >= sum\\(m\\)' failed: Must be TRUE."
  )
})

### n = sum(m) ----

test_that("opt works well for m, H = 4 and n = sum(m)", {
  result <- opt(310, A, m)
  expect_equal(result, m)
})

### n > sum(m) ----

test_that("opt works well for m, H = 4 and n > sum(m)", {
  result <- opt(490, A, m)
  expect_equal(result, c(105, 140, 175, 70))
})

# ONLY `M` IS SPECIFIED ----

## H = 1 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when M, H = 1", {
  expect_error(
    opt("1", A[1], M = M[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-101, A[1], M = M[1]),
    "Assertion on 'n > 0' failed: Must be TRUE."
  )
  expect_error(
    opt(Inf, A[1], M = M[1]),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(Inf, A[1], M = Inf),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, A[1], M = M[1]),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, A[1], M = M[1]),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), A[1], M = M[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), A[1], M = M[1]),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n > M (error) ----

test_that("opt throws error when M, H = 1, n > M", {
  expect_error(
    opt(301, A[1], M = M[1]),
    "Assertion on 'n <= sum\\(M\\)' failed: Must be TRUE."
  )
})

### n = M ----

test_that("opt works well for M, H = 1 and n = M", {
  result <- opt(M[1], A[1], M = M[1])
  expect_equal(result, M[1])
})

### n < M ----

test_that("opt works well for M, H = 1 and n < M", {
  result <- opt(299, A[1], M = M[1])
  expect_equal(result, 299)
})

test_that("opt works well for M = Inf, H = 1", {
  result <- opt(5000, A[1], M = Inf)
  expect_equal(result, 5000)
})

## H = 4 ----

### Assertions on n ----

test_that("opt throws error for non valid n, when M, H = 4", {
  expect_error(
    opt("1", A, M = M),
    "Assertion on 'n' failed: Must be of type 'number', not 'character'."
  )
  expect_error(
    opt(-101, A, M = M),
    "Assertion on 'n > 0' failed: Must be TRUE."
  )
  expect_error(
    opt(Inf, A, M = M),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(Inf, A, M = c(M[1:3], Inf)),
    "Assertion on 'n' failed: Must be finite."
  )
  expect_error(
    opt(NA, A, M = M),
    "Assertion on 'n' failed: May not be NA."
  )
  expect_error(
    opt(NULL, A, M = M),
    "Assertion on 'n' failed: Must be of type 'number', not 'NULL'."
  )
  expect_error(
    opt(numeric(0), A, M = M),
    "Assertion on 'n' failed: Must have length 1."
  )
  expect_error(
    opt(c(1, 5), A, M = M),
    "Assertion on 'n' failed: Must have length 1."
  )
})

### n > sum(M) (error) ----

test_that("opt throws error when M, H = 4, n > sum(M)", {
  expect_error(
    opt(991, A, M = M),
    "Assertion on 'n <= sum\\(M\\)' failed: Must be TRUE."
  )
})

### n = sum(M) ----

test_that("opt works well for M, H = 4 and n = sum(M)", {
  result <- opt(990, A, M = M)
  expect_equal(result, M)
})

### n < sum(M) ----

test_that("opt works well for M, H = 4 and n < sum(M)", {
  result <- opt(490, A, M = M)
  expect_equal(result, c(105, 140, 175, 70))
})

test_that("opt works well for M with Inf, H = 4", {
  result <- opt(5000, A, M = c(M[1:3], Inf))
  expect_equal(result, c(M[1:3], 4100))
})

test_that("opt works well for M, H = 4 and n < sum(M), [sga]", {
  result <- opt(490, A, M = M, M_algorithm = "sga")
  expect_equal(result, c(105, 140, 175, 70))
})

test_that("opt works well for M with Inf, H = 4, [sga]", {
  result <- opt(5000, A, M = c(M[1:3], Inf), M_algorithm = "sga")
  expect_equal(result, c(M[1:3], 4100))
})

test_that("opt works well for M, H = 4 and n < sum(M), [sgaplus]", {
  result <- opt(490, A, M = M, M_algorithm = "sgaplus")
  expect_equal(result, c(105, 140, 175, 70))
})

test_that("opt works well for M with Inf, H = 4, [sgaplus]", {
  result <- opt(5000, A, M = c(M[1:3], Inf), M_algorithm = "sgaplus")
  expect_equal(result, c(M[1:3], 4100))
})

test_that("opt works well for M, H = 4 and n < sum(M), [coma]", {
  result <- opt(490, A, M = M, M_algorithm = "coma")
  expect_equal(result, c(105, 140, 175, 70))
})

test_that("opt works well for M with Inf, H = 4, [coma]", {
  result <- opt(5000, A, M = c(M[1:3], Inf), M_algorithm = "coma")
  expect_equal(result, c(M[1:3], 4100))
})
