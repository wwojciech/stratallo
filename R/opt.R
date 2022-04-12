#' @title Optimum Sample Allocation in Stratified Sampling Schemes
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   A classical problem in survey methodology in stratified sampling
#'   is an optimum sample allocation problem. This problem is formulated as
#'   determination of a vector of strata sample sizes that minimizes the
#'   variance of the pi-estimator of the population total of a given study
#'   variable, under constraint on total sample size.
#'
#'   The `dopt()` function solves the problem of optimum sample allocation
#'   under either lower or upper bounds constraints, optionally imposed on
#'   strata sample sizes. The allocation computed is valid for all stratified
#'   sampling schemes for which the variance of the stratified pi-estimator is
#'   of the form
#'   \deqn{D(x_1,...,x_H) = a^2_1/x_1 + ... + a^2_H/x_H - b,}
#'   where \eqn{H} denotes total number of strata, \eqn{x_1, ..., x_H} are the
#'   strata sample sizes, and \eqn{b}, \eqn{a_w > 0} do not depend on
#'   \eqn{x_w, w = 1, ..., H}. \cr
#'
#'   The `dopt()` function makes use of the following allocation algorithms:
#'   `rNa`, `sga`, `sgaplus`, `coma` for optimal sample allocation under upper
#'   bounds constraints only, and `LrNa` for optimal sample allocation under
#'   lower bounds constraints only. The `rNa`, `sga`, and `coma` are described
#'   in Wesołowski et al. (2021), the `sgaplus` in Wójciak (2019), and the
#'   `LrNa` is introduced in Wójciak (2022).
#'   If no inequality constraints are added, then no any special
#'   algorithm is used as the allocation is given as a closed form expression,
#'   known as Neyman allocation
#'   \deqn{x_w = a_w * n / (a_1 + ... + a_H), w = 1, ..., H.}\cr
#'
#' @details The `dopt()` function computes
#'   \deqn{argmin D(x_1,...,x_H),}
#'   under the equality constraint imposed on total sample size
#'   \deqn{x_1 + ... + x_H = n.}
#'   Optionally, one of the following set of one-sided inequality constraints
#'   can be added
#'   \deqn{x_w <= M_w, w = 1,...,H, (Case I)}
#'   or
#'   \deqn{x_w >= m_w, w = 1,...,H, (Case II)}
#'   where \eqn{n > 0} denotes overall sample size, \eqn{m_w > 0}, and
#'   \eqn{M_w > 0, w = 1, ..., H}, are the lower and upper bounds respectively,
#'   imposed on sample strata sizes.\cr
#'   User of `dopt()` can choose whether the inequality constraints will be
#'   added to the optimization problem or not. This is achieved with the proper
#'   use of `m` and `M` arguments of the function. In case of no inequality
#'   constraints to be added, `m` and `M` must be both specified as `NULL`
#'   (default). If only upper bounds constraints should be added (Case I), user
#'   specifies these bounds with `M` argument, while leaving `m` as `NULL`.
#'   Finally, if only lower bounds constraints should be added (Case II), user
#'   specifies these bounds with `m` argument, while leaving `M` as `NULL`.
#'   At the moment, `dopt()` function does not allow to add box-constraints
#'   simultaneously.
#'
#'   For the Case I, there are four different underlying algorithms available
#'   to use. These are abbreviated as: "rNa" ([rna_one_sided()]),
#'   "sga" ([sga()]), "sgaplus" ([sgaplus()]), "coma" ([coma()]). Functions
#'   names that perform given algorithms are given in the brackets. See its help
#'   page for more details. For the Case II, the "rNa" ([rna_one_sided()]) is
#'   used.
#'
#' @note For simple random sampling without replacement design in each stratum,
#'   parameters of the variance function \eqn{D} are
#'   \eqn{b = N_1 * S_1^2 + ... + N_H * S_H^2}, and \eqn{a_w = N_w * S_w}, where
#'   \eqn{N_w, S_w, w = 1, ..., H}, are strata sizes and standard deviations of
#'   a study variable in strata respectively.
#'
#' @param n (`number`)\cr total sample size. A strictly positive scalar.
#' @param a (`numeric`)\cr parameters \eqn{a_1, ..., a_H} of variance function
#'   \eqn{D}. Strictly positive numbers.
#' @param m (`numeric` or `NULL`)\cr lower bounds constraints optionally imposed
#'   on strata sample sizes. If different than `NULL`, it is then required that
#'   `n >= sum(m)`. Must be assigned with `NULL` (default) if `M` is not `NULL`.
#'   Strictly positive numbers.
#' @param M (`numeric` or `NULL`)\cr upper bounds constraints optionally imposed
#'   on strata sample sizes. If different than `NULL`, it is then required that
#'   `n <= sum(M)`. Must be assigned with `NULL` (default) if `m` is not `NULL`.
#'   Strictly positive numbers.
#' @param M_method (`string`)\cr the name of the underlying algorithm to be used
#'   for computing a sample allocation under one-sided upper bounds constraints
#'   (Case I). One of the following: \emph{"rna"} (default), \emph{"sga"},
#'   \emph{"sgaplus"}, \emph{"coma"}.
#'
#' @return Numeric vector with optimal sample allocation in strata.
#'
#' @seealso [nopt()], [rna_one_sided()], [sga()], [sgaplus()], [coma()].
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
  assert_string(M_method)
  assert_subset(M_method, choices = c("rna", "sga", "sgaplus", "coma"))

  lower <- !is.null(m)
  upper <- !is.null(M)

  if (lower && upper) {
    stop("Box constraints are not supported. m and M can be both not NULL.")
  }

  if (lower) { # Lower bounds only.
    m_sum <- sum(m)
    assert_number(n, lower = m_sum, finite = TRUE)
    if (H == 1L) {
      return(n)
    }
    if (n == m_sum) {
      return(m)
    }
    fun <- "rna_one_sided"
    args <- list(n = n, a = a, bounds = m, upper = FALSE)
  } else if (upper) { # Upper bounds only.
    M_sum <- sum(M)
    assert_number(n, lower = rmin, upper = M_sum, finite = TRUE)
    if (H == 1L) {
      return(n)
    }
    if (n == M_sum) {
      return(M)
    }
    if (M_method == "rna") {
      fun <- "rna_one_sided"
      args <- list(n = n, a = a, bounds = M)
    } else {
      fun <- M_method
      args <- list(n = n, a = a, M = M)
    }
  } else {
    assert_number(n, lower = rmin, finite = TRUE)
    return((n / sum(a)) * a) # No bounds, Neyman allocation.
  }

  do.call(fun, args)
}

#' @title Minimum Sample Size Allocation in Stratified Sampling Schemes
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   User function that determines fixed strata sample sizes that minimize
#'   total sample size, under assumed level of the variance of the stratified
#'   pi-estimator of the total and optional one-sided upper bounds imposed on
#'   strata sample sizes.
#'   The algorithm used by `nopt()` is described in Wójciak (2022).
#'   The allocation computed is valid for all stratified sampling schemes
#'   for which the variance of the stratified pi-estimator is of the form
#'   \deqn{D(x_1,...,x_H) = a_1/x_1 + ... + a_H/x_H - b,}
#'   where \eqn{H} denotes total number of strata, \eqn{x_1, ..., x_H} are the
#'   strata sample sizes, and \eqn{b}, \eqn{a_w > 0} do not depend on
#'   \eqn{x_w, w = 1, ..., H}. \cr
#'
#' @details The `nopt()` function computes
#'   \deqn{argmin n(x_1,...,x_H) = x_1 + ... + x_H,}
#'   under the equality constraint imposed on the variance
#'   \deqn{a^2_1/x_1 + ... + a^2_1/x_H - b = D.}
#'   Optionally, the following set of one-sided inequality constraints
#'   can be added
#'   \deqn{x_w <= M_w, w = 1,...,H,}
#'   where \eqn{D > 0} is a given number and \eqn{M_w > 0, w = 1, ..., H}, are
#'   the upper bounds, imposed on sample strata sizes.\cr
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
#' @seealso [rna_one_sided()], [dopt()].
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
    opt <- rna_one_sided(n = D + b, a = a, bounds = a2 / M, upper = FALSE)
    a2 / opt
  } else {
    a * sum(a) / (D + b)
  }
}
