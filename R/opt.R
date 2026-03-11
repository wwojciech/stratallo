#' @title Optimum Sample Allocation in Stratified Sampling
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Computes the optimum allocation for the following optimum allocation problem,
#' formulated in mathematical optimization terms:
#'
#' Minimize
#' \deqn{f(x_1,\ldots,x_H) = \sum_{h=1}^H \frac{A^2_h}{x_h}}
#' over \eqn{\mathbb R_+^H}, subject to
#' \deqn{\sum_{h=1}^H x_h = n,}
#' \deqn{m_h \leq x_h \leq M_h, \qquad h = 1,\ldots,H,}
#' where \eqn{n > 0,\, A_h > 0,\, m_h > 0,\, M_h > 0}, such that
#' \eqn{m_h < M_h,\, h = 1,\ldots,H}, and
#' \eqn{\sum_{h=1}^H m_h \leq n \leq \sum_{h=1}^H M_h}, are given numbers.
#' Inequality constraints are optional and may be omitted.
#'
#' Inequality constraints are optional, and the user can choose whether and how
#' they are applied to the optimization problem. This is controlled using the
#' `m` and `M` arguments as follows:
#' * **No inequality constraints:** both `m` and `M` must be `NULL` (default).
#' * **Lower bounds only (\eqn{m_1,\, \ldots,\, m_H}):** specify `m`, and
#'   set `M = NULL`.
#' * **Upper bounds only (\eqn{M_1,\, \ldots,\, M_H}):** specify `M`, and
#'   set `m = NULL`.
#' * **Box constraints (\eqn{m_h, M_h,\, h = 1,\ldots,H}):** specify both `m`
#'   and `M`.
#'
#' @details
#'   The `opt()` function uses different allocation algorithms depending on which
#'   inequality constraints are applied. Each algorithm is implemented in a
#'   separate R function, which is generally **not intended to be called directly**
#'   by the end user. The algorithms are:
#'
#'   * **Lower bounds only (\eqn{m_1,\, \ldots,\, m_H}):**
#'     * `LRNA` - [rna()]
#'
#'   * **Upper bounds only (\eqn{M_1,\, \ldots,\, M_H}):**
#'     * `RNA` - [rna()]
#'     * `SGA` - [sga()]
#'     * `SGAPLUS` - [sgaplus()]
#'     * `COMA` - [coma()]
#'
#'   * **Box constraints (\eqn{m_h, M_h,\, h = 1,\ldots,H}):**
#'     * `RNABOX` - [rnabox()]
#'
#'   See the documentation of each specific function for more details about the
#'   corresponding algorithm.
#'
#' @note If no inequality constraints are applied, the allocation follows the
#'   Neyman allocation:
#'   \deqn{x_h = A_h \frac{n}{\sum_{i=1}^H A_i}, \quad h = 1,\ldots,H.}
#'
#'   For a **stratified \eqn{\pi} estimator** of the population total using
#'   stratified simple random sampling without replacement design, the objective
#'   function parameters \eqn{A_h} are:
#'   \deqn{A_h = N_h S_h, \quad h = 1,\ldots,H,}
#'   where \eqn{N_h} is the size of stratum \eqn{h} and \eqn{S_h} is the
#'   standard deviation of the study variable in stratum \eqn{h}.
#'
#' @inheritParams rnabox
#' @param m (`numeric` or `NULL`)\cr
#'   optional lower bounds \eqn{m_1,\ldots,m_H} for the stratum sample sizes.
#'   If no lower bounds are desired, set `m = NULL`. If `M` is not `NULL`, it is
#'   required that \eqn{m_h < M_h} for all strata.
#' @param M (`numeric` or `NULL`)\cr
#'   optional upper bounds \eqn{M_1,\ldots,M_H} for the stratum sample sizes.
#'   If no upper bounds are desired, set `M = NULL`. If `m` is not `NULL`, it is
#'   required that \eqn{m_h < M_h} for all strata.
#' @param M_algorithm (`string`)\cr
#'   Name of the algorithm to use for computing the sample allocation when only
#'   upper-bound constraints are imposed.
#'   Must be one of `"rna"` (default), `"sga"`, `"sgaplus"`, or `"coma"`.
#'   This parameter is used only when \eqn{H > 1} and \code{n < sum(M)}.
#'
#' @return A numeric vector of the optimal sample allocations for each stratum.
#'
#' @seealso [optcost()], [rna()], [sga()], [sgaplus()], [coma()], [rnabox()].
#
#' @references
#' \insertRef{Sarndal}{stratallo}
#'
#' @export
#' @example examples/opt.R
#'
opt <- function(n, A, m = NULL, M = NULL, M_algorithm = "rna") {
  H <- length(A)
  assert_number(n, finite = TRUE)
  assert_numeric(A, finite = TRUE, any.missing = FALSE, min.len = 1L)
  assert_numeric(m, finite = TRUE, any.missing = FALSE, len = H, null.ok = TRUE)
  assert_numeric(M, any.missing = FALSE, len = H, null.ok = TRUE)
  assert_true(all(A > 0))
  assert_true(all(m > 0))
  assert_true(all(M > 0))
  assert_true(all(m < M))

  lwr_imposed <- !is.null(m)
  upr_imposed <- !is.null(M)
  if (lwr_imposed) {
    assert_true(n >= sum(m))
  }
  if (upr_imposed) {
    assert_true(n <= sum(M))
    if (!lwr_imposed && H > 1L && n < sum(M)) {
      assert_string(M_algorithm)
      assert_subset(M_algorithm, choices = c("rna", "sga", "sgaplus", "coma"), empty.ok = FALSE)
    }
  }
  assert_true(n > 0)

  if (H == 1L) {
    n
  } else if (n == sum(m)) {
    m
  } else if (n == sum(M)) {
    M
  } else if (lwr_imposed && upr_imposed) {
    rnabox(n = n, A = A, bounds_inner = M, bounds_outer = m)
  } else if (lwr_imposed) {
    rna(total_cost = n, A = A, bounds = m, cmp = .Primitive("<="))
  } else if (upr_imposed) {
    if (M_algorithm == "rna") {
      rna(total_cost = n, A = A, bounds = M)
    } else {
      do.call(M_algorithm, args = list(total_cost = n, A = A, M = M))
    }
  } else {
    (n / sum(A)) * A # Neyman allocation.
  }
}

#' @title Minimum-Cost Allocation in Stratified Sampling
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Computes stratum sample sizes that minimize the total survey cost for a
#' given target variance of a stratified estimator, optionally subject to
#' one-sided upper bounds on the stratum sample sizes.
#' Specifically, the function solves the following optimization problem:
#'
#' Minimize
#' \deqn{c(x_1,\ldots,x_H) = \sum_{h=1}^H c_h x_h}
#' over \eqn{\mathbb R_+^H}, subject to
#' \deqn{\sum_{h=1}^H \frac{A^2_h}{x_h} - A_0 = V,}
#' \deqn{x_h \leq M_h, \qquad h = 1,\ldots,H,}
#' where \eqn{A_0,\, A_h > 0,\, c_h > 0,\, M_h > 0,\, h = 1,\ldots,H},
#' and \eqn{V > \sum_{h=1}^H \frac{A^2_h}{M_h} - A_0}, are given numbers.
#'
#' The upper-bound constraints \eqn{x_h \leq M_h} are optional. If they are not
#' imposed, it is only required that \eqn{V > 0}.
#'
#' @details
#'   The allocation is computed using the **LRNA algorithm**, described in
#'   \insertCite{WojciakLRNA;textual}{stratallo}.
#'
#'   The solution is valid for stratified sampling designs in which the variance
#'   \eqn{V_{st}} of the stratified estimator can be expressed as
#'   \deqn{V_{st} = \sum_{h=1}^H \frac{A^2_h}{x_h} - A_0,}
#'   where \eqn{H} is the number of strata, \eqn{x_1,\ldots,x_H} are the stratum
#'   sample sizes, and \eqn{A_0,\, A_h > 0} do not depend on \eqn{x_h}.
#'
#' @note For the *stratified \eqn{\pi}-estimator* of the population total under
#'   *stratified simple random sampling without replacement* design, the
#'   parameters take the form
#'   \deqn{A_h = N_h S_h, \qquad h = 1,\ldots,H,}
#'   \deqn{A_0 = \sum_{h=1}^H N_h S_h^2,}
#'   where \eqn{N_h} is the size of stratum \eqn{h} and \eqn{S_h} is the
#'   standard deviation of the study variable in stratum \eqn{h}.
#'
#' @inheritParams opt
#' @inheritParams rna
#' @param V (`number`)\cr parameter \eqn{V} in the variance constraint.
#'   If upper bounds are imposed (`M` is not `NULL`), it must satisfy
#'   `V > sum(A^2/M) - A_0`. Otherwise, `V > 0`.
#' @param A0 (`number`)\cr population constant \eqn{A_0}.
#' @param M (`numeric` or `NULL`)\cr optional upper bounds \eqn{M_1,\ldots,M_H}
#'   on the stratum sample sizes.
#'   If no upper bounds are imposed, set `M = NULL`.
#'
#' @return
#'   A numeric vector containing the optimal sample allocation for each stratum.
#'
#' @seealso [rna()], [opt()].
#
#' @references
#' \insertRef{WojciakLRNA}{stratallo}
#'
#' @export
#' @examples
#' A <- c(3000, 4000, 5000, 2000)
#' M <- c(100, 90, 70, 80)
#' x <- optcost(1017579, A = A, A0 = 579, M = M)
#' x
optcost <- function(V, A, A0, M = NULL, unit_costs = 1) {
  H <- length(A)
  assert_number(V, finite = TRUE)
  assert_numeric(A, finite = TRUE, any.missing = FALSE, min.len = 1L)
  assert_number(A0, finite = TRUE)
  assert_numeric(M, any.missing = FALSE, len = H, null.ok = TRUE)
  assert_numeric(unit_costs, any.missing = FALSE, null.ok = TRUE)
  assert_true(length(unit_costs) == 1L || length(unit_costs) == length(A))
  assert_true(all(A > 0))
  assert_true(all(M > 0))
  assert_true(all(unit_costs > 0))

  A2 <- A^2
  upr_imposed <- !is.null(M)
  if (upr_imposed) {
    assert_true(sum(A2 / M) - A0 > 0)
    assert_true(V >= sum(A2 / M) - A0)
  } else {
    assert_true(V > 0)
  }

  if (H == 1L) {
    A2 / (V + A0)
  } else if (!upr_imposed) {
    c_sqrt <- sqrt(unit_costs)
    (A / c_sqrt) * sum(A * c_sqrt) / (V + A0)
  } else if (V == sum(A2 / M) - A0) {
    M
  } else {
    y <- rna(
      total_cost = V + A0,
      A = A,
      bounds = A2 / (unit_costs * M),
      unit_costs = unit_costs,
      cmp = .Primitive("<=")
    )
    A2 / (unit_costs * y)
  }
}
