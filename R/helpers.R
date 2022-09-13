#' @title Get Proper Version of `which_violates` Function
#'
#' @description `r lifecycle::badge("stable")`
#'
#' An internal function that prepares a simple 2-arguments wrapper of
#' [`base::which()`] that checks whether its first argument exceeds the second
#' one. Both arguments are numeric. This excess is hard coded in the returned
#' wrapper function and it is defined either as \eqn{>=} ("greater or equal")
#' or \eqn{<=} ("lower or equal"), depending on the value of the `geq` flag.
#'
#' @param geq (`flag`) \cr if `TRUE`, then "greater or equal" condition is set.
#'   Otherwise, "less then or equal" is set.
#'
#' @return 2-arguments function that checks whether its first argument exceeds
#'   the second one. Both arguments must be numeric.
#'
#' @seealso [rna_onesided()].
#'
#' @examples
#' which_violates <- stratallo:::h_get_which_violates()
#' which_violates(1:3, 3:1)
#'
#' which_violates <- stratallo:::h_get_which_violates(geq = FALSE)
#' which_violates(1:3, 3:1)
h_get_which_violates <- function(geq = TRUE) {
  assert_flag(geq)
  if (geq) {
    function(x, y) {
      which(x >= y)
    }
  } else {
    function(x, y) {
      which(x <= y)
    }
  }
}

#' @title Summarizing the Allocation
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A helper function that creates and returns a simple [`data.frame`] with
#' summary of the allocation as returned by the [dopt()] or [nopt()]. See the
#' illustrate example below.
#'
#' @details The summary [`data.frame`] contains the following columns: \cr
#' (1) the allocation vector, \cr
#' (2) lower bounds constraints (optional), \cr
#' (3) upper bounds constraints (optional), \cr
#' (4) indication whether the allocation for a given stratum is of `take-min`
#' type, i.e. x == m (optional), \cr
#' (5) indication whether the allocation for a given stratum is of `take-max`
#' type, i.e. x == M (optional), \cr
#' (6) indication whether the allocation for a given stratum is of `take-neyman`
#' type, i.e. m < x < M. \cr
#' The last row contains the sum of all of the values from the other rows
#' (wherever feasible).
#'
#' @param x (`numeric`)\cr the allocation vector.
#' @param a (`numeric`)\cr parameters \eqn{a_1, ..., a_H} of variance function
#'   \eqn{D}. See [dopt()] or [nopt()] for more details.
#' @param m (`numeric` or `NA`)\cr lower bounds constraints imposed on sample
#'   sizes in strata.
#' @param M (`numeric` or `NA`)\cr upper bounds constraints imposed on sample
#'   sizes in strata.
#'
#' @return A [`data.frame`] with the allocation summary.
#'
#' @export
#' @examples
#' a <- c(3000, 4000, 5000, 2000)
#' m <- c(100, 90, 70, 80)
#' M <- c(200, 150, 300, 210)
#'
#' x1 <- dopt(n = 400, a, m)
#' s1 <- allocation_summary(x1, a, m)
#'
#' x2 <- dopt(n = 540, a, m, M)
#' s2 <- allocation_summary(x2, a, m, M)
allocation_summary <- function(x,
                               a,
                               m = NULL,
                               M = NULL) {
  H <- length(a)
  assert_numeric(a, min.len = 1L)
  assert_numeric(x, len = H, any.missing = FALSE)
  assert_numeric(m, len = H, null.ok = TRUE)
  assert_numeric(M, len = H, null.ok = TRUE)

  d <- data.frame(
    a = c(a, NA),
    allocation = c(x, sum(x)),
    row.names = c(paste0("Stratum_", 1:length(a)), "SUM")
  )

  is_neyman <- rep(TRUE, H)

  if (!is.null(m)) {
    is_m <- x == m
    d <- cbind(d, m = c(m, sum(m)), take_min = c(ifelse(is_m, "*", ""), sum(is_m)))
    is_neyman <- !is_m
  }

  if (!is.null(M)) {
    is_M <- x == M
    d <- cbind(d, M = c(M, sum(M)), take_max = c(ifelse(is_M, "*", ""), sum(is_M)))
    is_neyman <- is_neyman & !is_M
  }

  d <- cbind(d, take_neyman = c(ifelse(is_neyman, "*", ""), sum(is_neyman)))

  cn_order <- c("a", "m", "M", "allocation", "take_min", "take_max", "take_neyman")
  d[, intersect(cn_order, colnames(d))]
}
