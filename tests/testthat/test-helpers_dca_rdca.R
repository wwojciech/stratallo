# H_cnt2dind ----

test_that("H_cnt2dind works as expected)", {
  # Zero strata
  expect_identical(H_cnt2dind(0), integer())

  # Single domain with multiple strata
  expect_identical(H_cnt2dind(4), rep(1L, 4))

  # Multiple domains
  expect_identical(
    H_cnt2dind(c(2, 2, 3)),
    c(1L, 1L, 2L, 2L, 3L, 3L, 3L)
  )
})

test_that("H_cnt2dind works as expected for domain with 0 strata)", {
  # somewhat artificial; normally each domain has >=1 strata
  expect_identical(
    H_cnt2dind(c(2, 0, 3)),
    c(1L, 1L, 3L, 3L, 3L)
  )
})

# H_cnt2glbidx ----

test_that("H_cnt2glbidx works as expected)", {
  # Zero strata
  expect_identical(H_cnt2glbidx(0), list(integer()))

  # Single domain with multiple strata
  expect_identical(H_cnt2glbidx(4), list(1:4))

  # Multiple domains
  expect_identical(
    H_cnt2glbidx(c(2, 2, 3)),
    list(1:2, 3:4, 5:7)
  )
})

test_that("H_cnt2glbidx works as expected for domain with 0 strata)", {
  # somewhat artificial; normally each domain has >=1 strata
  expect_identical(
    H_cnt2glbidx(c(2, 0, 3)),
    list(1:2, integer(), 3:5)
  )
})

# H_get_strata_indices ----

test_that("H_get_strata_indices works as expected)", {
  # Zero strata
  expect_identical(H_get_strata_indices(0, 1), integer())

  # Single domain with multiple strata
  expect_identical(H_get_strata_indices(4, 1), 1:4)
  expect_identical(H_get_strata_indices(4, 2), integer())

  # Multiple domains
  expect_identical(H_get_strata_indices(c(2, 2, 3), 1), 1:2)
  expect_identical(H_get_strata_indices(c(2, 2, 3), 2), 3:4)
  expect_identical(H_get_strata_indices(c(2, 2, 3), 3), 5:7)
  expect_identical(H_get_strata_indices(c(2, 2, 3), 4), integer())
})

test_that("H_get_strata_indices works as expected for domain with 0 strata)", {
  # somewhat artificial; normally each domain has >=1 strata
  expect_identical(H_get_strata_indices(c(2, 0, 3), 1), 1:2)
  expect_identical(H_get_strata_indices(c(2, 0, 3), 2), integer())
  expect_identical(H_get_strata_indices(c(2, 0, 3), 3), 3:5)
  expect_identical(H_get_strata_indices(c(2, 0, 3), 4), integer())
})
