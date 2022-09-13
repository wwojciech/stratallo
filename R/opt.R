#' @title Optimum Sample Allocation in Stratified Sampling Schemes
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A classical problem in survey methodology in stratified sampling is an
#' optimum sample allocation problem. This problem is formulated as
#' determination of a vector of strata sample sizes that minimizes the variance
#' of the pi-estimator of the population total of a given study variable, under
#' constraint on total sample size.
#'
#' The `dopt()` function solves the problem of optimum sample allocation under
#' lower or upper bounds constraints, optionally imposed on strata sample sizes.
#' The allocation computed is valid for all stratified sampling schemes for
#' which the variance of the stratified pi-estimator is of the form:
#' \deqn{D(x_1,...,x_H) = a^2_1/x_1 + ... + a^2_H/x_H - b,}
#' where \eqn{H} denotes total number of strata, \eqn{x_1, ..., x_H} are the
#' strata sample sizes, and \eqn{b}, \eqn{a_w > 0} do not depend on
#' \eqn{x_w, w = 1, ..., H}. \cr
#'
#' The `dopt()` function makes use of the following allocation algorithms:
#' `rna`, `sga`, `sgaplus`, `coma` for optimal sample allocation under one-sided
#' upper bounds constraints, and `lrna` for optimal sample allocation under
#' one-sided lower bounds constraints. For the allocation under box-constraints,
#' the `rnabox` algorithm is used. The `rna`, `sga`, and `coma` are described in
#' Wesołowski et al. (2021), while the `sgaplus` is in Wójciak (2019). The
#' `lrna` is introduced in Wójciak (2022). The `rnabox` algorithm is a new
#' optimal allocation algorithm that was developed by the authors of this
#' package and will be published soon.
#'
#' @details The `dopt()` function computes:
#'   \deqn{argmin D(x_1,...,x_H),}
#'   under the equality constraint imposed on total sample size:
#'   \deqn{x_1 + ... + x_H = n,}
#'   and inequality constraints (optimally) imposed on strata sample size:
#'   \deqn{m_w <= x_w <= M_w,  w = 1,...,H.}
#'   Here, \eqn{H} denotes total number of strata, \eqn{x_1, ..., x_H} are the
#'   strata sample sizes, and \eqn{n > 0}, \eqn{b},
#'   \eqn{a_w > 0, w = 1, ..., H}, are given numbers. Furthermore, \eqn{m_w > 0}
#'   and \eqn{M_w > 0, w = 1, ..., H} are lower and upper bounds respectively,
#'   optionally imposed on sample sizes in strata. \cr
#'
#'   User of `dopt()` can choose whether the inequality constraints will be
#'   added to the optimization problem or not. This is achieved with the proper
#'   use of `m` and `M` arguments of the function. In case of no inequality
#'   constraints to be added, `m` and `M` must be both specified as `NULL`
#'   (default). If only upper bounds constraints should be added, it should be
#'   specified using `M` argument, while leaving `m` as `NULL`. If only lower
#'   bounds constraints should be added, user must specify it with `m` argument,
#'   while leaving `M` as `NULL`. Finally, in case of box-constraints, both
#'   parameters `m` and `M` must be specified.
#'
#'   For the case of one-sided upper bounds constraints only, there are four
#'   different underlying algorithms available to use. These are abbreviated as:
#'   "rna" ([rna_onesided()]), "sga" ([sga()]), "sgaplus" ([sgaplus()]),
#'   and "coma" ([coma()]). Functions names that perform given algorithms are
#'   given in the brackets. See its help page for more details.
#'   For the case of one-sided lower bounds constraints only, the
#'   "rna" ([rna_onesided()]) is used. Finally, for box-constraints, the
#'   "rnabox" algorithm is used ([rnabox()]).
#'
#' @note For simple random sampling without replacement design in each stratum,
#'   parameters of the variance function \eqn{D} are
#'   \eqn{b = N_1 * S_1^2 + ... + N_H * S_H^2}, and \eqn{a_w = N_w * S_w}, where
#'   \eqn{N_w, S_w, w = 1, ..., H}, are strata sizes and standard deviations of
#'   a study variable in strata respectively. \cr
#'
#'   Note that if no inequality constraints are imposed, the optimal allocation
#'   is given as a closed form expression, known as Neyman allocation:
#'   \deqn{x_w = a_w * n / (a_1 + ... + a_H), w = 1, ..., H.}
#'
#' @param n (`number`)\cr total sample size. A strictly positive scalar.
#' @param a (`numeric`)\cr parameters \eqn{a_1, ..., a_H} of variance function
#'   \eqn{D}. Strictly positive numbers.
#' @param m (`numeric` or `NULL`)\cr lower bounds constraints optionally imposed
#'   on strata sample sizes. If not `NULL`, it is then required that
#'   `n >= sum(m)`. Strictly positive numbers.
#' @param M (`numeric` or `NULL`)\cr upper bounds constraints optionally imposed
#'   on strata sample sizes. If not `NULL`, it is then required that
#'   `n <= sum(M)`.
#'   Strictly positive numbers.
#' @param M_method (`string`)\cr the name of the underlying algorithm to be used
#'   for computing a sample allocation under one-sided upper bounds constraints
#'   One of the following: `rna` (default), `sga`, `sgaplus`, `coma`. This
#'   parameter is used only in case when `m` argument is `NULL` and `M` is not
#'   `NULL`.
#'
#' @return Numeric vector with optimal sample allocation in strata.
#'
#' @seealso [nopt()], [rna_onesided()], [sga()], [sgaplus()], [coma()],
#'   [rnabox()].
#
#' @references
#'   Wesołowski, J., Wieczorkowski, R., Wójciak, W. (2021),
#'   Optimality of the recursive Neyman allocation,
#'   *Journal of Survey Statistics and Methodology*,
#'   \doi{10.1093/jssam/smab018},
#'   \doi{10.48550/arXiv.2105.14486} \cr
#'
#'   Wójciak, W. (2022),
#'   Minimum sample size allocation in stratified sampling under constraints on
#'   variance and strata sample sizes,
#'   \doi{10.48550/arXiv.2204.04035} \cr
#'
#'   Wójciak, W. (2019), Optimal allocation in stratified sampling schemes,
#'   *MSc Thesis*, Warsaw University of Technology, Warsaw, Poland.
#'   <http://home.elka.pw.edu.pl/~wwojciak/msc_optimal_allocation.pdf> \cr
#'
#'   Sarndal, C.-E., Swensson, B., and Wretman, J. (1992),
#'   *Model Assisted Survey Sampling*, New York, NY: Springer.
#'
#' @export
#' @example examples/dopt.R
#'
dopt <- function(n, a, m = NULL, M = NULL, M_method = "rna") {
  H <- length(a)
  rmin <- .Machine$double.xmin
  assert_numeric(a, lower = rmin, finite = TRUE, any.missing = FALSE, min.len = 1L)
  assert_numeric(m, lower = rmin, finite = TRUE, any.missing = FALSE, len = H, null.ok = TRUE)
  assert_numeric(M, lower = rmin, any.missing = FALSE, len = H, null.ok = TRUE)
  assert_subset(M_method, choices = c("rna", "sga", "sgaplus", "coma"), empty.ok = FALSE)

  lower <- !is.null(m)
  upper <- !is.null(M)
  m_sum <- sum(m)
  M_sum <- sum(M)
  assert_number(n, lower = m_sum, upper = ifelse(upper, M_sum, Inf), finite = TRUE)

  if (H == 1L) {
    n
  } else if (n == m_sum) {
    m
  } else if (n == M_sum) {
    M
  } else if (lower && upper) {
    rnabox(n = n, a = a, m = m, M = M)
  } else if (lower) {
    rna_onesided(n = n, a = a, bounds = m, upper = FALSE)
  } else if (upper) {
    if (M_method == "rna") {
      rna_onesided(n = n, a = a, bounds = M)
    } else {
      do.call(M_method, args = list(n = n, a = a, M = M))
    }
  } else {
    (n / sum(a)) * a # Neyman allocation.
  }
}

#' @title Minimum Sample Size Allocation in Stratified Sampling Schemes
#'
#' @description `r lifecycle::badge("stable")`
#'
#' User function that determines fixed strata sample sizes that minimize total
#' sample size, under assumed level of the variance of the stratified
#' pi-estimator of the total and optional one-sided upper bounds imposed on
#' strata sample sizes.
#' The algorithm used by `nopt()` is described in Wójciak (2022). The allocation
#' computed is valid for all stratified sampling schemes for which the variance
#' of the stratified pi-estimator is of the form:
#' \deqn{D(x_1,...,x_H) = a^2_1/x_1 + ... + a^2_H/x_H - b,}
#' where \eqn{H} denotes total number of strata, \eqn{x_1, ..., x_H} are the
#' strata sample sizes, and \eqn{b}, \eqn{a_w > 0} do not depend on
#' \eqn{x_w, w = 1, ..., H}. \cr
#'
#' @details The `nopt()` function computes:
#'   \deqn{argmin n(x_1,...,x_H) = x_1 + ... + x_H,}
#'   under the equality constraint imposed on the variance:
#'   \deqn{a^2_1/x_1 + ... + a^2_H/x_H - b = D.}
#'   Optionally, the following set of one-sided inequality constraints can be
#'   added:
#'   \deqn{x_w <= M_w, w = 1,...,H,}
#'   where \eqn{D > 0} is a given number and \eqn{M_w > 0, w = 1, ..., H}, are
#'   the upper bounds imposed on sample sizes in strata. \cr
#'
#' @note For simple random sampling without replacement design in each stratum,
#'   parameters of the variance function \eqn{D} are
#'   \eqn{b = N_1 * S_1^2 + ... + N_H * S_H^2}, and \eqn{a_w = N_w * S_w}, where
#'   \eqn{N_w, S_w, w = 1, ..., H}, are strata sizes and standard deviations of
#'   a study variable in strata respectively.
#'
#' @param D (`number`)\cr variance equality constraint value. A strictly
#'   positive scalar.
#' @param a (`numeric`)\cr parameters \eqn{a_1, ..., a_H} of variance function
#'   \eqn{D}. Strictly positive numbers.
#' @param b (`number`)\cr parameter \eqn{b} of variance function \eqn{D}.
#' @param M (`numeric` or `NULL`)\cr upper bounds constraints optionally imposed
#'   on strata sample sizes. If different than `NULL`, it is then required that
#'   `D >= sum(a/M) - b > 0`. Strictly positive numbers.
#'
#' @return Numeric vector with optimal sample allocation in strata.
#'
#' @seealso [rna_onesided()], [dopt()].
#
#' @references
#'   Wójciak, W. (2022),
#'   Minimum sample size allocation in stratified sampling under constraints on
#'   variance and strata sample sizes,
#'   \doi{10.48550/arXiv.2204.04035}
#'
#' @export
#' @examples
#' a <- c(3000, 4000, 5000, 2000)
#' M <- c(100, 90, 70, 80)
#' nopt(1017579, a = a, b = 579, M = M)
nopt <- function(D, a, b, M = NULL) {
  H <- length(a)
  rmin <- .Machine$double.xmin
  assert_numeric(a, lower = rmin, finite = TRUE, any.missing = FALSE, min.len = 1L)
  assert_number(b, finite = TRUE)
  assert_numeric(M, lower = rmin, any.missing = FALSE, len = H, null.ok = TRUE)
  a2 <- a^2
  upper <- !is.null(M)
  if (upper) {
    D_min <- sum(a2 / M) - b
    assert_true(D_min > 0)
  } else {
    D_min <- 0 + rmin
  }
  assert_number(D, lower = D_min, finite = TRUE)

  if (H == 1L) {
    return(a2 / (D + b))
  }
  if (upper) {
    if (D == D_min) {
      return(M)
    }
    opt <- rna_onesided(n = D + b, a = a, bounds = a2 / M, upper = FALSE)
    a2 / opt
  } else {
    a * sum(a) / (D + b)
  }
}
