#' Summarizing the Allocation
#'
#' @description `r lifecycle::badge("stable")`
#'
#' A utility that returns a simple [`data.frame`] summarizing the allocation
#' returned by [opt()] or [optcost()].
#'
#' @param x (`numeric`)\cr sample allocations \eqn{x_1,\ldots,x_H}.
#' @param A (`numeric`)\cr population constants \eqn{A_1,\ldots,A_H}.
#' @param m (`numeric` or `NULL`)\cr optional lower bounds \eqn{m_1,\ldots,m_H}.
#' @param M (`numeric` or `NULL`)\cr optional upper bounds \eqn{M_1,\ldots,M_H}.
#'
#' @return A [`data.frame`] with \eqn{H + 1} rows and up to seven variables,
#'   where \eqn{H} is the number of strata.
#'   The first \eqn{H} rows correspond to strata \eqn{h = 1,\ldots,H}, while
#'   the last row contains column totals (where applicable).
#'   The columns include:
#' \describe{
#'   \item{A}{Population constant \eqn{A_h}.}
#'   \item{m*}{Lower bound \eqn{m_h} (if provided).}
#'   \item{M*}{Upper bound \eqn{M_h} (if provided).}
#'   \item{allocation}{The optimized sample size \eqn{x_h}.}
#'   \item{take_min*}{Boolean indicator: \eqn{x_h = m_h}.}
#'   \item{take_max*}{Boolean indicator: \eqn{x_h = M_h}.}
#'   \item{take_Neyman}{Boolean indicator: \eqn{m_h < x_h < M_h} (or
#'     simply the internal Neyman allocation if no bounds were violated).}
#' }
#'
#' @seealso [opt()], [optcost()].
#'
#' @export
#' @examples
#' A <- c(3000, 4000, 5000, 2000)
#' m <- c(100, 90, 70, 80)
#' M <- c(200, 150, 300, 210)
#'
#' xopt_1 <- opt(n = 400, A, m)
#' alloc_summary(xopt_1, A, m)
#'
#' xopt_2 <- opt(n = 540, A, m, M)
#' alloc_summary(xopt_2, A, m, M)
alloc_summary <- function(x, A, m = NULL, M = NULL) {
  assert_numeric(A, min.len = 1L)
  H <- length(A)
  assert_numeric(x, len = H, any.missing = FALSE)
  assert_numeric(m, len = H, null.ok = TRUE)
  assert_numeric(M, len = H, null.ok = TRUE)

  d <- data.frame(
    A = c(A, NA),
    allocation = c(x, sum(x)),
    row.names = c(paste0("Stratum_", 1:length(A)), "SUM")
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

  cn_order <- c("A", "m", "M", "allocation", "take_min", "take_max", "take_neyman")
  d[, intersect(cn_order, colnames(d))]
}
