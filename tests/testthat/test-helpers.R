# h_get_which_violated ----

test_that("h_get_which_violated returns a function when geq is TRUE", {
  which_violated <- h_get_which_violated()
  expect_function(which_violated, args = c("x", "y"))
})

test_that("h_get_which_violated works as expected when geq is TRUE", {
  which_violated <- h_get_which_violated()
  result <- which_violated(1:3, 3:1)
  expect_equal(result, 2:3)
})

test_that("h_get_which_violated returns a function when geq is FALSE", {
  which_violated <- h_get_which_violated(geq = FALSE)
  expect_function(which_violated, args = c("x", "y"))
})

test_that("h_get_which_violated works as expected when geq is FALSE", {
  which_violated <- h_get_which_violated(geq = FALSE)
  result <- which_violated(1:3, 3:1)
  expect_equal(result, 1:2)
})
