# rdca_obj_cnstr ----

test_that("rdca_obj_cnstr works as expected)", {
  d <- pop2d4s
  x <- c(140, 104.57, 113.08, 142.35)

  result <- rdca_obj_cnstr(x, 500, d$H_counts, d$N, d$S, d$rho2)
  expected <- list(
    Topt = 71.35512,
    h = 0,
    h_d = c(`1` = 0.04442545, `2` = -0.01316309)
  )

  expect_equal(result, expected, tolerance = 10^-6)
})

test_that("rdca_obj_cnstr works as expected for custom J)", {
  d <- pop2d4s
  x <- c(140, 104.57, 113.08, 142.35)

  result <- rdca_obj_cnstr(x, 500, d$H_counts, d$N, d$S, d$rho2, 1)
  expected <- list(
    Topt = 71.35512,
    h = 0,
    h_d = c(`1` = 0.04442545, `2` = -0.01316309),
    g_dh = c(`(1,1)` = 0, `(1,2)` = -5.43)
  )

  expect_equal(result, expected, tolerance = 10^-6)
})

# rdca_cnstr_check ----

test_that("rdca_cnstr_check works as expected)", {
  d <- pop2d4s
  x <- c(140, 104.57, 113.08, 142.35)

  result <- rdca_cnstr_check(x, 500, d$H_counts, d$N, d$S, d$rho2)
  expected <- list(
    h = c(`tol=1e-19` = TRUE),
    h_d = c(`tol=1e-01` = TRUE, `tol=1e-01` = TRUE)
  )

  expect_identical(result, expected)
})

test_that("rdca_cnstr_check works as expected for custom J)", {
  d <- pop2d4s
  x <- c(140, 104.57, 113.08, 142.35)

  result <- rdca_cnstr_check(x, 500, d$H_counts, d$N, d$S, d$rho2, 1)
  expected <- list(
    h = c(`tol=1e-19` = TRUE),
    h_d = c(`tol=1e-01` = TRUE, `tol=1e-01` = TRUE),
    g_dh = c(`(1,1)` = TRUE, `(1,2)` = TRUE)
  )

  expect_identical(result, expected)
})

# rdca_optcond_sU ----

test_that("rdca_optcond_sU works as expected (U=1)", {
  s <- c(0.2749514, 0.9614205)
  d <- pop2d4s

  result <- rdca_optcond_sU(d$H_counts, d$S, d$rho, s, 1)
  expect_identical(result, TRUE)

  result <- rdca_optcond_sU(d$H_counts, d$S, d$rho, s, 1, TRUE)
  expect_equal(result, 0.1806705, tolerance = 10^-7)
})

test_that("rdca_optcond_sU works as expected (U=2)", {
  s <- c(0.09402774, 0.95224251)
  d <- pop2d4s

  result <- rdca_optcond_sU(d$H_counts, d$S, d$rho, s, 2)
  expect_identical(result, FALSE)

  result <- rdca_optcond_sU(d$H_counts, d$S, d$rho, s, 2, TRUE)
  expect_equal(result, -0.188815, tolerance = 10^-6)
})

test_that("rdca_optcond_sU works as expected (U=3)", {
  s <- c(0.1094162, 1.1558573)
  d <- pop2d4s

  result <- rdca_optcond_sU(d$H_counts, d$S, d$rho, s, 3)
  expect_identical(result, TRUE)

  result <- rdca_optcond_sU(d$H_counts, d$S, d$rho, s, 3, TRUE)
  expect_equal(result, 0.1166268, tolerance = 10^-6)
})

test_that("rdca_optcond_sU works as expected (U=1:2)", {
  s <- 0.9509614
  d <- pop2d4s

  result <- rdca_optcond_sU(d$H_counts, d$S, d$rho, s, 1:2)
  expect_identical(result, logical(0))

  result <- rdca_optcond_sU(d$H_counts, d$S, d$rho, s, 1:2, TRUE)
  expect_identical(result, double(0))
})

test_that("rdca_optcond_sU works as expected for 5 domains (U=5:9)", {
  H_counts <- c(3, 2, 3, 2, 2)
  S <- 1:12
  rho <- c(11, 12, 13, 14, 15)
  U <- c(9, 5, 7, 6, 8)
  s <- c(10, 20, 40, 50)

  result <- rdca_optcond_sU(H_counts, S, rho, s, U)
  expect_identical(result, c(TRUE, TRUE))

  result <- rdca_optcond_sU(H_counts, S, rho, s, U, TRUE)
  expect_equal(result, c(17.60000, 38.44444), tolerance = 10^-6)
})

test_that("rdca_optcond_sU works as expected for 5 domains (U=1)", {
  H_counts <- c(3, 2, 3, 2, 2)
  S <- 1:12
  rho <- c(11, 12, 13, 14, 15)
  U <- 1
  s <- c(10, 20, 30, 40, 50)

  result <- rdca_optcond_sU(H_counts, S, rho, s, U)
  expect_identical(result, FALSE)

  result <- rdca_optcond_sU(H_counts, S, rho, s, U, TRUE)
  expect_identical(result, -1)
})

test_that("rdca_optcond_sU works as expected for 5 domains (U=1:11)", {
  H_counts <- c(3, 2, 3, 2, 2)
  S <- 1:12
  rho <- c(11, 12, 13, 14, 15)
  U <- 1:11
  s <- 50

  result <- rdca_optcond_sU(H_counts, S, rho, s, U)
  expect_identical(result, TRUE)

  result <- rdca_optcond_sU(H_counts, S, rho, s, U, TRUE)
  expect_equal(result, 48.63636, tolerance = 10^-7)
})
