# ran_round ----

test_that("ran_round works as expected)", {
  x <- c(4.5, 4.1, 4.9)

  set.seed(5)
  expect_identical(ran_round(x), c(5L, 4L, 4L))

  set.seed(6)
  expect_identical(ran_round(x), c(4L, 4L, 5L))
})

# round_oric ----

test_that("round_oric works as expected)", {
  x <- c(4.5, 4.1, 4.9)
  expect_identical(round_oric(x), c(4L, 4L, 5L))
})

# var_st ----

test_that("var_st works as expected)", {
  N <- c(300, 400, 500, 200)
  S <- c(2, 5, 3, 1)
  x <- c(27, 88, 66, 9)
  a <- N * S
  a0 <- sum(N * S^2)
  result <- var_st(x, a, a0)
  expect_equal(result, 81423.232)
})

# var_st_tsi ----

test_that("var_st_tsi works as expected)", {
  N <- c(300, 400, 500, 200)
  S <- c(2, 5, 3, 1)
  x <- c(27, 88, 66, 9)
  result <- var_st_tsi(x, N, S)
  expect_equal(result, 81423.232)
})

# asummary ----

test_that("asummary works as expected when no bounds are specified", {
  x <- c(85, 114, 142, 57)

  result <- asummary(x, a)
  expected <- data.frame(
    a = c(a, NA),
    allocation = c(x, sum(x)),
    take_neyman = c(rep("*", 4), 4),
    row.names = c(paste0("Stratum_", 1:4), "SUM")
  )
  expect_identical(result, expected)
})

test_that("asummary works as expected when only m is specified", {
  m <- c(100, 90, 70, 80)
  x <- c(100, 98, 122, 80)

  result <- asummary(x, a, m)
  expected <- data.frame(
    a = c(a, NA),
    m = c(m, sum(m)),
    allocation = c(x, sum(x)),
    take_min = c("*", "", "", "*", "2"),
    take_neyman = c("", "*", "*", "", "2"),
    row.names = c(paste0("Stratum_", 1:4), "SUM")
  )
  expect_identical(result, expected)
})

test_that("asummary works as expected when only M is specified", {
  M <- c(200, 150, 300, 210)
  x <- c(117, 150, 195, 78)

  result <- asummary(x, a, M = M)
  expected <- data.frame(
    a = c(a, NA),
    M = c(M, sum(M)),
    allocation = c(x, sum(x)),
    take_max = c("", "*", "", "", "1"),
    take_neyman = c("*", "", "*", "*", "3"),
    row.names = c(paste0("Stratum_", 1:4), "SUM")
  )
  expect_identical(result, expected)
})

test_that("asummary works as expected when only M is specified", {
  m <- c(100, 90, 70, 80)
  M <- c(200, 150, 300, 210)
  x <- c(117, 150, 194, 80)

  result <- asummary(x, a, m, M)
  expected <- data.frame(
    a = c(a, NA),
    m = c(m, sum(m)),
    M = c(M, sum(M)),
    allocation = c(x, sum(x)),
    take_min = c("", "", "", "*", "1"),
    take_max = c("", "*", "", "", "1"),
    take_neyman = c("*", "", "*", "", "2"),
    row.names = c(paste0("Stratum_", 1:4), "SUM")
  )
  expect_identical(result, expected)
})
