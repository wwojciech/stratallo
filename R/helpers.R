#' Random Rounding of Numbers
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A number \eqn{x} is rounded to integer \eqn{y} according to the following
#' rule:
#' \deqn{y = \left\lfloor{x}\right\rfloor + I(u < (x - \left\lfloor{x}\right\rfloor)),}
#' where function \eqn{I:\{TRUE, FALSE\} \to \{0, 1\}}, is defined as:
#' \deqn{
#'  I(x) = \begin{cases}
#'  0,  & x \text{ is } FALSE \\
#'  1, & x \text{ is } TRUE,
#'  \end{cases}
#' }
#' and \eqn{u} is number that is generated from `Uniform(0, 1)` distribution.
#'
#' @importFrom stats runif
#'
#' @param x (`numeric`)\cr a numeric vector.
#' @return An integer vector.
#'
#' @export
#' @examples
#' x <- c(4.5, 4.1, 4.9)
#' set.seed(5)
#' ran_round(x) # 5 4 4
#' set.seed(6)
#' ran_round(x) # 4 4 5
ran_round <- function(x) {
  assert_numeric(x)
  as.integer(floor(x) + (runif(length(x)) < (x - floor(x))))
}

#' Optimal Rounding under Integer Constraints
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (`numeric`)\cr a numeric vector.
#' @return An integer vector.
#'
#' @references
#'   Cont, R., Heidari, M. (2014).
#'   Optimal rounding under integer constraints.
#'   \doi{10.48550/arXiv.1501.00014}
#'
#' @export
#' @examples
#' x <- c(4.5, 4.1, 4.9)
#' round_oric(x) # 4 4 5
round_oric <- function(x) {
  n <- round(sum(x))
  m <- floor(x)
  y <- x - m
  Ix <- sum(y)

  if (Ix == 0) {
    as.integer(x)
  } else {
    iy <- order(-y)
    u <- unique(y[iy])
    z <- integer(length(x))
    for (i in 1:length(u)) z[iy] <- z[iy] + (y[iy] == u[i]) * i
    iy2 <- order(-y, z, -m)
    # m[iy][iy2][1:Ix] <- ceiling(x[iy][iy2][1:Ix])
    m[iy2][1:Ix] <- (m[iy2][1:Ix]) + 1
    as.integer(m)
  }
}

#' Variance of the Stratified Estimator
#'
#' @name var_st
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Compute the value of the variance function \eqn{V} of the stratified
#' estimator, which is of the following generic form:
#' \deqn{V(x_1,\ldots,x_H) = \sum_{h=1}^H \frac{A^2_h}{x_h} - A_0,}
#' where \eqn{H} denotes total number of strata, \eqn{x_1,\ldots,x_H} are strata
#' sample sizes and \eqn{A_0,\, A_h > 0,\, h = 1,\ldots,H}, are population
#' constants.
#'
#' @param x (`numeric`)\cr sample allocations \eqn{x_1,\ldots,x_H} in strata.
#' @param a (`numeric`)\cr population constants \eqn{A_1,\ldots,A_H}.
#' @param a0 (`number`)\cr population constant \eqn{A_0}.
#'
#' @return Value of the variance \eqn{V} for a given allocation vector
#'   \eqn{x_1,\ldots,x_H}.
#'
#' @references
#'   Särndal, C.-E., Swensson, B. and Wretman, J. (1992).
#'   *Model Assisted Survey Sampling*,
#'   Chapter 3.7 *Stratified Sampling*,
#'   Springer, New York.
#'
#' @export
#'
var_st <- function(x, a, a0) {
  sum(a^2 / x) - a0
}

#' @describeIn var_st computes value of variance \eqn{V} for the case of
#'   \emph{stratified \eqn{\pi} estimator} of the population total and
#'   \emph{stratified simple random sampling without replacement} design. This
#'   particular case yields:
#'   \deqn{A_h = N_h S_h, \quad h = 1,\ldots,H,}
#'   \deqn{A_0 = \sum_{h=1}^H N_h S_h^2,}
#'   where \eqn{N_h} is the size of stratum \eqn{h}, and \eqn{S_h} is stratum
#'   standard deviation of a study variable, \eqn{h = 1,\ldots,H}.
#'
#' @param N (`numeric`)\cr strata sizes \eqn{N_1,\ldots,N_H}.
#' @param S (`numeric`)\cr strata standard deviations of a given study variable
#'   \eqn{S_1,\ldots,S_H}.
#'
#' @export
#' @examples
#' N <- c(3000, 4000, 5000, 2000)
#' S <- rep(1, 4)
#' M <- c(100, 90, 70, 80)
#' opt <- opt(n = 190, a = N * S, M = M)
#' var_st_tsi(x = opt, N, S) # 1017579
var_st_tsi <- function(x, N, S) {
  a <- N * S
  a0 <- sum(N * S^2)
  var_st(x, a, a0)
}

#' Summarizing the Allocation
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A helper function that returns a simple [`data.frame`] with summary of the
#' allocation as returned by the [opt()] or [optcost()]. See the illustrate
#' example below.
#'
#' @inheritParams var_st
#' @param m (`numeric` or `NULL`)\cr lower bounds \eqn{m_1,\ldots,m_H},
#'   optionally imposed on sample sizes in strata.
#' @param M (`numeric` or `NULL`)\cr upper bounds \eqn{M_1,\ldots,M_H},
#'   optionally imposed on sample sizes in strata.
#'
#' @return A [`data.frame`] with as many rows as number of strata \eqn{H} + 1,
#'   and up to 7 variables. A single row corresponds to a given stratum
#'   \eqn{h \in \{1,\ldots,H\}}, whilst the last row contains sums of all of the
#'   numerical values from the above rows (wherever feasible).
#'   Summary table has the following columns (* indicates that the column may
#'   not be present):
#' \describe{
#'   \item{a}{population constant \eqn{A_h}}
#'   \item{m*}{lower bound imposed on sample size in stratum}
#'   \item{M*}{upper bound imposed on sample size in stratum}
#'   \item{allocation}{sample size for a given stratum}
#'   \item{take_min*}{indication whether the allocation is of `take-min` type,
#'     i.e. \eqn{x_h = m_h}}
#'   \item{take_max*}{indication whether the allocation is of `take-max` type,
#'     i.e. \eqn{x_h = M_h}}
#'   \item{take_neyman}{indication whether the allocation is of `take-Neyman`
#'    type, i.e. \eqn{m_h < x_h < M_h}}
#' }
#'
#' @seealso [opt()], [optcost()].
#'
#' @export
#' @examples
#' a <- c(3000, 4000, 5000, 2000)
#' m <- c(100, 90, 70, 80)
#' M <- c(200, 150, 300, 210)
#'
#' x1 <- opt(n = 400, a, m)
#' s1 <- asummary(x1, a, m)
#'
#' x2 <- opt(n = 540, a, m, M)
#' s2 <- asummary(x2, a, m, M)
asummary <- function(x, a, m = NULL, M = NULL) {
  assert_numeric(a, min.len = 1L)
  H <- length(a)
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
