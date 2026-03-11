test_that("alloc_summary works as expected when no bounds are specified", {
  x <- c(85, 114, 142, 57)

  result <- alloc_summary(x, A)
  expected <- data.frame(
    A = c(A, NA),
    allocation = c(x, sum(x)),
    take_neyman = c(rep("*", 4), 4),
    row.names = c(paste0("Stratum_", 1:4), "SUM")
  )
  expect_identical(result, expected)
})

test_that("alloc_summary works as expected when only m is specified", {
  m <- c(100, 90, 70, 80)
  x <- c(100, 98, 122, 80)

  result <- alloc_summary(x, A, m)
  expected <- data.frame(
    A = c(A, NA),
    m = c(m, sum(m)),
    allocation = c(x, sum(x)),
    take_min = c("*", "", "", "*", "2"),
    take_neyman = c("", "*", "*", "", "2"),
    row.names = c(paste0("Stratum_", 1:4), "SUM")
  )
  expect_identical(result, expected)
})

test_that("alloc_summary works as expected when only M is specified", {
  M <- c(200, 150, 300, 210)
  x <- c(117, 150, 195, 78)

  result <- alloc_summary(x, A, M = M)
  expected <- data.frame(
    A = c(A, NA),
    M = c(M, sum(M)),
    allocation = c(x, sum(x)),
    take_max = c("", "*", "", "", "1"),
    take_neyman = c("*", "", "*", "*", "3"),
    row.names = c(paste0("Stratum_", 1:4), "SUM")
  )
  expect_identical(result, expected)
})

test_that("alloc_summary works as expected when only M is specified", {
  m <- c(100, 90, 70, 80)
  M <- c(200, 150, 300, 210)
  x <- c(117, 150, 194, 80)

  result <- alloc_summary(x, A, m, M)
  expected <- data.frame(
    A = c(A, NA),
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
