# round_ran ----

test_that("round_ran works as expected)", {
  x <- c(4.5, 4.1, 4.9)

  set.seed(5)
  expect_identical(round_ran(x), c(5L, 4L, 4L))

  set.seed(6)
  expect_identical(round_ran(x), c(4L, 4L, 5L))
})

# round_oric ----

test_that("round_oric works as expected)", {
  x <- c(4.5, 4.1, 4.9)
  expect_identical(round_oric(x), c(4L, 4L, 5L))
})
