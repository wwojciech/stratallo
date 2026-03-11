#' @title Variance of the Stratified \eqn{\pi} Estimator of the Population Total
#'
#' @name var_st
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Computes the value of the variance function of the stratified \eqn{\pi}
#' estimator of the population total, which has the following generic form:
#' \deqn{V_{st} = \sum_{h=1}^H \frac{A_h^2}{x_h} - A_0,}
#' where \eqn{H} denotes the total number of strata, \eqn{x_1,\ldots,x_H} are
#' the stratum sample sizes, and \eqn{A_0} and \eqn{A_h > 0}, for
#' \eqn{h = 1,\ldots,H}, are population constants that do not depend on the
#' \eqn{x_h}.
#'
#' @param x (`numeric`)\cr sample allocations \eqn{x_1,\ldots,x_H}.
#' @param A (`numeric`)\cr population constants \eqn{A_1,\ldots,A_H}.
#' @param A0 (`numeric(1)`)\cr population constant \eqn{A_0}.
#'
#' @return The value of the variance \eqn{V_{st}} for a given allocation vector
#'   \eqn{x_1,\ldots,x_H}.
#'
#' @references
#' \insertRef{Sarndal}{stratallo}
NULL

#' @describeIn var_st
#' The value of the variance \eqn{V_{st}}.
#'
#' @export
#' @examples
#' N <- c(300, 400, 500, 200)
#' S <- c(2, 5, 3, 1)
#' x <- c(27, 88, 66, 9)
#' A <- N * S
#' A0 <- sum(N * S^2)
#'
#' var_st(x, A, A0)
#'
var_st <- function(x, A, A0) {
  sum(A^2 / x) - A0
}

#' @describeIn var_st
#' The value of the variance \eqn{V_{st}} for the case of
#' \emph{simple random sampling without replacement} design within each stratum.
#'
#' This particular case yields:
#' \deqn{A_h = N_h S_h, \qquad h = 1,\ldots,H,}
#' \deqn{A_0 = \sum_{h=1}^H N_h S_h^2,}
#' where \eqn{N_h} denotes the size of stratum \eqn{h} and \eqn{S_h} is the
#' corresponding stratum standard deviation of the study variable, for
#' \eqn{h = 1,\ldots,H}.
#'
#' @param N (`integerish`)\cr strata sizes \eqn{N_1,\ldots,N_H}.
#' @param S (`numeric`)\cr strata standard deviations of a given study variable
#'   \eqn{S_1,\ldots,S_H}.
#'
#' @export
#' @examples
#' N <- c(3000, 4000, 5000, 2000)
#' S <- rep(1, 4)
#' M <- c(100, 90, 70, 80)
#' x <- opt(n = 320, A = N * S, M = M)
#'
#' var_stsi(x = x, N, S)
var_stsi <- function(x, N, S) {
  A <- N * S
  A0 <- sum(N * S^2)
  var_st(x, A, A0)
}
