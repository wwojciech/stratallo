#' @title Variance of Stratified Pi-estimator of the Total
#'
#' @name var_tst
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   Compute the variance of the stratified pi-estimator of the population
#'   total, that is of the following generic form
#'   \deqn{D(x_1,...,x_H) = a^2_1/x_1 + ... + a^2_H/x_H - b,}
#'   where \eqn{H} denotes total number of strata, \eqn{x_1, ..., x_H} are the
#'   strata sample sizes, and \eqn{b}, \eqn{a_w > 0} do not depend on
#'   \eqn{x_w, w = 1, ..., H}. \cr
#'
#' @param x (`numeric`)\cr sample allocations in strata. Strictly positive
#'   numbers.
#' @param a (`numeric`)\cr parameters \eqn{a_1, ..., a_H} of variance function
#'   \eqn{D}. Strictly positive numbers.
#' @param b (`numeric`)\cr parameter \eqn{b} of variance function \eqn{D}.
#'
#' @return Value of the variance \eqn{D} for a given allocation vector `x`.
#'
#' @references
#'   Sarndal, C.-E., Swensson, B., and Wretman, J. (1992),
#'   *Model Assisted Survey Sampling*,
#'   Chapter 3.7 *Stratified Sampling*,
#'   New York, NY: Springer.
#'
#' @export
#'
var_tst <- function(x, a, b) {
  sum(a^2 / x) - b
}

#' @describeIn var_tst computes variance of stratified pi-estimator of the total
#'   for simple random sampling without replacement design in each stratum.
#'   Under this design, parameters of the variance function \eqn{D} take the
#'   following form
#'   \deqn{a_w = N_w * S_w, w = 1, ..., H,}
#'   and
#'   \deqn{b = N_1 * S_1^2 + ... + N_H * S_H^2,}
#'   where \eqn{N_w, S_w, w = 1, ..., H}, are strata sizes and standard
#'   deviations of a study variable in strata respectively.
#'
#' @param x (`numeric`)\cr sample allocations in strata. Strictly positive
#'   numbers.
#' @param N (`numeric`)\cr strata sizes. Strictly positive numbers.
#' @param S (`numeric`)\cr strata standard deviations of a given study variable.
#'   Strictly positive numbers.
#'
#' @export
#' @examples
#' N <- c(3000, 4000, 5000, 2000)
#' S <- rep(1, 4)
#' M <- c(100, 90, 70, 80)
#' opt <- dopt(n = 190, a = N * S, M = M)
#' var_tst_si(x = opt, N, S)
var_tst_si <- function(x, N, S) {
  a <- N * S
  b <- sum(N * S^2)
  var_tst(x, a, b)
}
