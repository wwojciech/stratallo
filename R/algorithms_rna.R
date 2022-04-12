#' @include helpers.R
NULL

#' @title Recursive Neyman Algorithm for Optimal Sample Allocation in Stratified
#'   Sampling Schemes
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   Internal function that implements the recursive Neyman optimal allocation
#'   algorithms, `rNa` and `LrNa`, described in Wesołowski et al. (2021) and
#'   Wójciak (2022) respectively. The `rna_one_sided()` should not be used
#'   directly. Instead, user function [dopt()] or [nopt()] should be used. \cr
#'
#'   The `rna_one_sided()` function computes
#'   \deqn{argmin D(x_1,...,x_H) = a^2_1/x_1 + ... + a^2_H/x_H - b,}
#'   under the equality constraint imposed on total sample size
#'   \deqn{x_1 + ... + x_H = n.}
#'   Here, \eqn{H} denotes total number of strata, \eqn{x_1, ..., x_H} are the
#'   strata sample sizes, and \eqn{n > 0}, \eqn{b},
#'   \eqn{a_w > 0, w = 1, ..., H}, are given numbers. \cr
#'   Optionally, one of the following set of one-sided inequality constraints
#'   can be added
#'   \deqn{x_w <= M_w,  w = 1,...,H, (Case I)}
#'   or
#'   \deqn{x_w >= m_w, w = 1,...,H, (Case II)}
#'   where \eqn{m_w > 0} and \eqn{M_w > 0, w = 1, ..., H} are lower and upper
#'   bounds respectively, imposed on sample strata sizes. \cr
#'
#'   User of `rna_one_sided()` can choose whether the inequality constraints
#'   will be added to the optimization problem or not. This is achieved with the
#'   proper use of `bounds` and `upper` arguments of the function. In case of no
#'   inequality constraints to be added, `bounds` must be specified as `NULL`
#'   (default).
#'   If any bounds should be imposed on sample strata sizes, user must specify
#'   these with `bounds` argument.
#'   For the Case I of the upper bounds, `upper` flag must be set to `TRUE`
#'   (default) and then the `rna_one_sided()` performs the `rNa`.
#'   For the Case II of lower bounds, `upper` flag must be set to `FALSE` and
#'   then the `rna_one_sided()` performs the `LrNa`.
#'   The `upper` flag is ignored when `bounds` is `NULL`.
#'   If no inequality constraints are added, the allocation is given as a closed
#'   form expression, known as Neyman allocation
#'   \deqn{x_w = a_w * n / (a_1 + ... + a_H), w = 1, ..., H.}
#'
#' @note For simple random sampling without replacement design in each stratum,
#'   parameters of the variance function \eqn{D} are
#'   \eqn{b = N_1 * S_1^2 + ... + N_H * S_H^2}, and \eqn{a_w = N_w * S_w}, where
#'   \eqn{N_w, S_w, w = 1, ..., H}, are strata sizes and standard deviations of
#'   a study variable in strata respectively. \cr
#'
#'   The `rNa` and `LrNa` are kind of more general versions of popular recursive
#'   Neyman allocation procedure that is described in Remark 12.7.1 in Sarndal
#'   et al. (1992). It is a procedure of optimal sample allocation in stratified
#'   sampling scheme with simple random sampling without replacement design in
#'   each stratum while not exceeding strata sizes.
#'
#' @param n (`number`)\cr total sample size. A strictly positive scalar.
#' @param a (`numeric`)\cr parameters \eqn{a_1, ..., a_H} of variance function
#'   \eqn{D}. Strictly positive numbers.
#' @param bounds (`numeric` or `NULL`) \cr optional lower or upper bounds
#'   constraints imposed on strata sample sizes. If `bounds` is not `NULL`, it
#'   is required that `n <= sum(bounds)` in case of `upper = TRUE`, and
#'   `n >= sum(bounds)`, in case of `upper = FALSE`.
#'   Strictly positive numbers.
#' @param upper (`flag`) \cr should values of `bounds` be treated as one-sided
#'   upper bounds constraints (default)? Otherwise, they are treated as lower
#'   bounds.
#'
#' @return Numeric vector with optimal sample allocations in strata.
#'
#' @seealso [dopt()], [nopt()], [sga()], [sgaplus()], [coma()].
#'
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
#'   Sarndal, C.-E., Swensson, B., and Wretman, J. (1992),
#'   *Model Assisted Survey Sampling*, New York, NY: Springer.
#'
#' @export
#' @examples
#' a <- c(3000, 4000, 5000, 2000)
#' bounds <- c(100, 90, 70, 80)
#' rna_one_sided(n = 190, a = a, bounds = bounds)
rna_one_sided <- function(n, a, bounds = NULL, upper = TRUE) {
  which_violated <- h_get_which_violated(geq = upper)

  A <- sum(a)
  x <- (n / A) * a # Neyman allocation.
  Ri <- which_violated(x, bounds) # Indices of W for which x violates bounds.

  if (length(Ri) == 0L) {
    return(x)
  } else {
    H <- length(a) # Number of strata.
    W <- seq_len(H) # Strata native labels. To be shrunk in the repeat loop.
    repeat {
      R <- W[Ri] # Set of strata native labels for which x violates bounds.
      W <- W[-Ri] # W = W \ R.
      n <- n - sum(bounds[R])
      A <- A - sum(a[R])
      x <- (n / A) * a[W] # Neyman allocation for W.
      Ri <- which_violated(x, bounds[W]) # Indices of W for which x violates bounds.
      if (length(Ri) == 0L) {
        bounds[W] <- x
        break
      }
    }
  }
  bounds
}
