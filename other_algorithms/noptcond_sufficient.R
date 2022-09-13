#' Optimal sample allocation in stratified random sampling scheme.
#' Maximize: \deqn{ \sum_{h \in J} \frac{N_h^2 S_h^2}{n_h} - \sum_{h \in J} N_h S_h^2 }
#' Subject to: \deqn{ \sum_{h \in J} = n } and \deqn{ l_h \leq n_h \leq u_h \forall h \in J }
#'
#' @param d numeric vector, d = N*S, where N - strata sizes, S - strata standard deviations.
#' @param l numeric vector, lower bounds on sample sizes in strata.
#' @param u numeric vector, upper bounds on sample sizes in strata.
#' @param n numeric scalar, total sample size.
#'
#' @return numeric vector with optimal allocation or error if the problem is infeasible
#'
#' @examples
#'
#' noptcond_sufficient(
#'   d = c(2000, 3000),
#'   l = c(30, 40),
#'   u = c(50, 200),
#'   n = 160
#' )
#'
#' noptcond_sufficient(
#'   d = data_mM[, "N"] * data_mM[, "N"],
#'   l = data_mM[, "m"],
#'   u = data_mM[, "M"],
#'   n = 2000
#' )
noptcond_sufficient <- function(d, l, u, n) {
  `&` <- function(x, y) {
    if (!all(x)) {
      return(FALSE)
    }
    if (!all(y)) {
      return(FALSE)
    }
    return(TRUE)
  }

  if (n > sum(u) | n < sum(l)) {
    stop("n is not feasible")
  }

  dL <- d / l
  dU <- d / u

  J_dL <- order(dL)
  J_dU <- order(dU, decreasing = TRUE)
  H <- length(d)
  J <- seq_along(d)

  for (R in 0:(H - 1)) { # if problem is feasible, solution must be found in at most H-1 iterations

    for (i in 0:R) {
      JL <- J_dL[0:(R - i)]
      JU <- J_dU[0:i]

      if (!any(JL %in% JU) & (n > sum(l[JL], u[JU]))) {
        J0 <- if (R != 0) J[-c(JL, JU)] else J
        noptJ0 <- (n - sum(l[JL], u[JU])) * (d[J0] / sum(d[J0]))

        if ((l[J0] <= noptJ0 & noptJ0 <= u[J0]) &
          (d[J0] / noptJ0 >= suppressWarnings(max(dL[JL])) &
            d[J0] / noptJ0 <= suppressWarnings(min(dU[JU])))) {

          # form solution
          return(c(l[JL], noptJ0, u[JU])[order(c(JL, J0, JU))])
        }
      }
    }
    # print((n - sum(l[JL], u[JU])) / sum(d[J0])) # s(L, U)
  }
}

# Note: max() and min() functions are wrapped with suppressWarnings()
# to suppress a warning, thrown when the argument is of length 0,
# which is a behavior of base::max() and base::min().
