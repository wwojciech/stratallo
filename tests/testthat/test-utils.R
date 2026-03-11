# is_empty ----

test_that("is_empty works as expected)", {
  expect_false(is_empty(1))
  expect_false(is_empty("a"))
  expect_false(is_empty(integer(1)))
  expect_false(is_empty(list(0)))
  expect_true(is_empty(list()))
  expect_true(is_empty(integer()))
  expect_true(is_empty(character()))
  expect_true(is_empty(NULL))
})

# is_nonempty ----

test_that("is_nonempty works as expected)", {
  expect_true(is_nonempty(1))
  expect_true(is_nonempty("a"))
  expect_true(is_nonempty(integer(1)))
  expect_true(is_nonempty(list(0)))
  expect_false(is_nonempty(list()))
  expect_false(is_nonempty(integer()))
  expect_false(is_nonempty(character()))
  expect_false(is_nonempty(NULL))
})

# is_equal ----

test_that("is_equal works as expected)", {
  expect_identical(is_equal(3, 4), c("tol=1e-01" = FALSE))

  expect_identical(
    is_equal(c(3, 4), c(5, 6)),
    c("tol=1e-01" = FALSE, "tol=1e-01" = FALSE)
  )

  expect_identical(
    is_equal(c(3, 4), c(3, 4)),
    c("tol=1e-19" = TRUE, "tol=1e-19" = TRUE)
  )

  expect_identical(
    is_equal(c(3, 3), c(3.09, 3.11)),
    c("tol=1e-01" = TRUE, "tol=1e-01" = FALSE)
  )
})

test_that("is_equal works as expected with tol_max=0)", {
  res <- is_equal(c(2, 2), c(2.09, 2.11), tol_max = 0)
  expect_identical(res, c("tol=1e-01" = TRUE, "tol=1e+00" = TRUE))
})

test_that("is_equal works as expected with tol_max=-3)", {
  res <- is_equal(c(2, 2, 2, 2), c(2.00001, 2.00009, 2.0001, 2.0009), tol_max = -3)
  expect_identical(
    res,
    c("tol=1e-04" = TRUE, "tol=1e-04" = TRUE, "tol=1e-03" = TRUE, "tol=1e-03" = TRUE)
  )
})

test_that("is_equal works as expected with tol_max=-4)", {
  res <- is_equal(c(2, 2, 2, 2), c(2.00001, 2.00009, 2.0001, 2.0009), tol_max = -4)
  expect_identical(
    res,
    c("tol=1e-04" = TRUE, "tol=1e-04" = TRUE, "tol=1e-04" = FALSE, "tol=1e-04" = FALSE)
  )
})

# has_mixed_signs ----

test_that("has_mixed_signs works as expected", {
  expect_false(has_mixed_signs(1:5))
  expect_false(has_mixed_signs(-(1:5)))
  expect_true(has_mixed_signs(c(-1, -2, 3)))
})

test_that("has_mixed_signs works as expected with 0", {
  expect_false(has_mixed_signs(0))
  expect_false(has_mixed_signs(c(0, -1)))
  expect_false(has_mixed_signs(c(0, 1)))
  expect_true(has_mixed_signs(c(0, 1, -1)))
})
