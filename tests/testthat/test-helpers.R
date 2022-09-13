# h_get_which_violates ----

test_that("h_get_which_violates returns a function when geq is TRUE", {
  which_violated <- h_get_which_violates()
  expect_function(which_violated, args = c("x", "y"))
})

test_that("h_get_which_violates works as expected when geq is TRUE", {
  which_violated <- h_get_which_violates()
  result <- which_violated(1:3, 3:1)
  expect_equal(result, 2:3)
})

test_that("h_get_which_violates returns a function when geq is FALSE", {
  which_violated <- h_get_which_violates(geq = FALSE)
  expect_function(which_violated, args = c("x", "y"))
})

test_that("h_get_which_violates works as expected when geq is FALSE", {
  which_violated <- h_get_which_violates(geq = FALSE)
  result <- which_violated(1:3, 3:1)
  expect_equal(result, 1:2)
})

# allocation_summary ----

test_that("allocation_summary works as expected when no bounds are specified", {
  a <- c(3000, 4000, 5000, 2000)
  x <- c(85, 114, 142, 57)

  result <- allocation_summary(x, a)
  expected <- data.frame(
    a = c(a, NA),
    allocation = c(x, sum(x)),
    take_neyman = c(rep("*", 4), 4),
    row.names = c(paste0("Stratum_", 1:4), "SUM")
  )
  expect_identical(result, expected)
})

test_that("allocation_summary works as expected when only m is specified", {
  a <- c(3000, 4000, 5000, 2000)
  m <- c(100, 90, 70, 80)
  x <- c(100, 98, 122, 80)

  result <- allocation_summary(x, a, m)
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

test_that("allocation_summary works as expected when only M is specified", {
  a <- c(3000, 4000, 5000, 2000)
  M <- c(200, 150, 300, 210)
  x <- c(117, 150, 195, 78)

  result <- allocation_summary(x, a, M = M)
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

test_that("allocation_summary works as expected when only M is specified", {
  a <- c(3000, 4000, 5000, 2000)
  m <- c(100, 90, 70, 80)
  M <- c(200, 150, 300, 210)
  x <- c(117, 150, 194, 80)

  result <- allocation_summary(x, a, m, M)
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
