#' Optimum Sample Allocation in Stratified Sampling
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A classical problem in survey methodology in stratified sampling is optimum
#' sample allocation. This problem is formulated as determination of strata
#' sample sizes that minimize the variance of the
#' \emph{stratified \eqn{\pi} estimator} of the population total (or mean) of a
#' given study variable, under certain constraints on sample sizes in strata.
#'
#' The `opt()` user function solves the following optimum sample allocation
#' problem, formulated below in the language of mathematical optimization.
#'
#' Minimize
#' \deqn{f(x_1,\ldots,x_H) = \sum_{h=1}^H \frac{A^2_h}{x_h}}
#' subject to
#' \deqn{\sum_{h=1}^H x_h = n}
#' \deqn{m_h \leq x_h \leq M_h, \quad h = 1,\ldots,H,}
#' where \eqn{n > 0,\, A_h > 0,\, m_h > 0,\, M_h > 0}, such that
#' \eqn{m_h < M_h,\, h = 1,\ldots,H}, and
#' \eqn{\sum_{h=1}^H m_h \leq n \leq \sum_{h=1}^H M_h}, are given numbers.
#' The minimization is on \eqn{\mathbb R_+^H}.
#'
#' The inequality constraints are optional and user can choose whether and how
#' they are to be added to the optimization problem. This is achieved by the
#' proper use of `m` and `M` arguments of this function, according to the
#' following rules:
#' * no inequality constraints imposed: both `m` and `M` must be both set to
#' `NULL` (default).
#' * one-sided lower bounds \eqn{m_h,\, h = 1,\ldots,H}, imposed:
#' lower bounds are specified with `m`, while `M` is set to `NULL`.
#' * one-sided upper bounds \eqn{M_h,\, h = 1,\ldots,H}, imposed:
#' upper bounds are specified with `M`, while `m` is set to `NULL`.
#' * box-constraints imposed: lower and upper bounds must be specified with `m`
#' and `M`, respectively.
#'
#' @details
#'   The `opt()` function makes use of several allocation algorithms, depending
#'   on which of the inequality constraints should be taken into account in the
#'   optimization problem. Each algorithm is implemented in a separate R
#'   function that in general should not be used directly by the end user.
#'   The following is the list with the algorithms that are used along with the
#'   name of the function that implements a given algorithm. See the description
#'   of a specific function to find out more about the corresponding algorithm.
#'   * one-sided lower-bounds \eqn{m_h,\, h = 1,\ldots,H}:
#'      * `LRNA` - [rna()]
#'  * one-sided upper-bounds \eqn{M_h,\, h = 1,\ldots,H}:
#'      * `RNA` - [rna()]
#'      * `SGA` - [sga()]
#'      * `SGAPLUS` - [sgaplus()]
#'      * `COMA` - [coma()]
#'  * box constraints \eqn{m_h, M_h,\, h = 1,\ldots,H}:
#'      * `RNABOX` - [rnabox()]
#'
#' @note If no inequality constraints are added, the allocation is given by the
#'   Neyman allocation as:
#'   \deqn{x_h = A_h \frac{n}{\sum_{i=1}^H A_i}, \quad h = 1,\ldots,H.}
#'   For \emph{stratified \eqn{\pi} estimator} of the population total with
#'   \emph{stratified simple random sampling without replacement} design in use,
#'   the parameters of the objective function \eqn{f} are:
#'   \deqn{A_h = N_h S_h, \quad h = 1,\ldots,H,}
#'   where \eqn{N_h} is the size of stratum \eqn{h} and \eqn{S_h} denotes
#'   standard deviation of a given study variable in stratum \eqn{h}.
#'
#' @inheritParams rnabox
#' @param m (`numeric` or `NULL`)\cr lower bounds \eqn{m_1,\ldots,m_H},
#'   optionally imposed on sample sizes in strata. If no lower bounds should be
#'   imposed, then `m` must be set to `NULL`. If `M` is not `NULL`, it is then
#'   required that `m < M`.
#' @param M (`numeric` or `NULL`)\cr upper bounds \eqn{M_1,\ldots,M_H},
#'   optionally imposed on sample sizes in strata. If no upper bounds should be
#'   imposed, then `M` must be set to `NULL`. If `m` is not `NULL`, it is then
#'   required that `m < M`.
#' @param M_algorithm (`string`)\cr the name of the underlying algorithm to be
#'   used for computing sample allocation under one-sided upper-bounds
#'   constraints.
#'   It must be one of the following: `rna` (default), `sga`, `sgaplus`, `coma`.
#'   This parameter is used only in case when `m` argument is `NULL` and `M` is
#'   not `NULL` and number of strata \eqn{H > 1} and `n < sum(M)`.
#'
#' @return Numeric vector with optimal sample allocations in strata.
#'
#' @seealso [optcost()], [rna()], [sga()], [sgaplus()], [coma()], [rnabox()].
#
#' @references
#'   Särndal, C.-E., Swensson, B. and Wretman, J. (1992).
#'   *Model Assisted Survey Sampling*, Springer, New York.
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
    rnabox(n = n, A = A, bounds1 = M, bounds2 = m)
  } else if (lwr_imposed) {
    rna(total_cost = n, A = A, bounds = m, check_violations = .Primitive("<="))
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

#' Minimum Cost Allocation in Stratified Sampling
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Function that determines fixed strata sample sizes that minimize total cost
#' of the survey, under assumed level of the variance of the stratified
#' estimator and under optional one-sided upper bounds imposed on strata sample
#' sizes. Namely, the following optimization problem, formulated below in the
#' language of mathematical optimization, is solved by `optcost()` function.
#'
#' Minimize
#' \deqn{c(x_1,\ldots,x_H) = \sum_{h=1}^H c_h x_h}
#' subject to
#' \deqn{\sum_{h=1}^H \frac{A^2_h}{x_h} - A_0 = V}
#' \deqn{x_h \leq M_h, \quad h = 1,\ldots,H,}
#' where \eqn{A_0,\, A_h > 0,\, c_h > 0,\, M_h > 0,\, h = 1,\ldots,H},
#' and \eqn{V > \sum_{h=1}^H \frac{A^2_h}{M_h} - A_0} are given numbers. The
#' minimization is on \eqn{\mathbb R_+^H}.
#' The upper-bounds constraints \eqn{x_h \leq M_h,\, h = 1,\ldots,H}, are
#' optional and can be skipped. In such a case, it is only required that
#' \eqn{V > 0}.
#'
#' @details The algorithm that is used by `optcost()` is the `LRNA` and it is
#'   described in Wójciak (2023). The allocation computed is valid for all
#'   stratified sampling schemes for which the variance of the stratified
#'   estimator is of the form:
#'   \deqn{\sum_{h=1}^H \frac{A^2_h}{x_h} - A_0,}
#'   where \eqn{H} denotes total number of strata, \eqn{x_1,\ldots,x_H} are
#'   strata sample sizes and \eqn{A_0,\, A_h > 0,\, h = 1,\ldots,H}, do not
#'   depend on \eqn{x_h,\, h = 1,\ldots,H}.
#'
#' @note For \emph{stratified \eqn{\pi} estimator} of the population total and
#'   for \emph{stratified simple random sampling without replacement} design,
#'   the population parameters are as follows:
#'   \deqn{A_h = N_h S_h, \quad h = 1,\ldots,H,}
#'   \deqn{A_0 = \sum_{h=1}^H N_h S_h^2,}
#'   where \eqn{N_h} is the size of stratum \eqn{h} and \eqn{S_h} denotes
#'   standard deviation of a given study variable in stratum \eqn{h}.
#'
#' @inheritParams opt
#' @inheritParams rna
#' @param V (`number`)\cr parameter \eqn{V} of the equality constraint. A
#'   strictly positive scalar. If `M` is not `NULL`, it is then required that
#'   `V >= sum(A^2/M) - A0`.
#' @param A0 (`number`)\cr population constant \eqn{A_0}.
#' @param M (`numeric` or `NULL`)\cr upper bounds \eqn{M_1,\ldots,M_H},
#'   optionally imposed on sample sizes in strata. If no upper bounds should be
#'   imposed, then `M` must be set to `NULL`.
#'
#' @return Numeric vector with optimal sample allocations in strata.
#'
#' @seealso [rna()], [opt()].
#
#' @references
#'   Wójciak, W. (2023).
#'   Another Solution of Some Optimum Allocation Problem.
#'   *Statistics in Transition new series*, 24(5) (in press).
#'   <https://arxiv.org/abs/2204.04035>
#'
#' @export
#' @examples
#' A <- c(3000, 4000, 5000, 2000)
#' M <- c(100, 90, 70, 80)
#' xopt <- optcost(1017579, A = A, A0 = 579, M = M)
#' xopt
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
      check_violations = .Primitive("<=")
    )
    A2 / (unit_costs * y)
  }
}
