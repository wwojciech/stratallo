#' @title Multi-Domain Optimum Sample Allocation with Controlled-Precision under
#'   Upper-Bound Constraints
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Computes the optimum allocation for the following multi-domain optimum
#' allocation problem, formulated in mathematical optimization terms:
#'
#' Minimize
#' \deqn{f(T,\, \boldsymbol x) = T}
#' over \eqn{\mathbb R \times \mathbb R_+^{\lvert \mathcal H \rvert}},
#' subject to
#' \deqn{\sum_{(d,h) \in \mathcal H} x_{d,h} = n,}
#' \deqn{\sum_{h \in \mathcal H_d} (\frac{1}{x_{d,h}} - \frac{1}{N_{d,h}}) \frac{N_{d,h}^2 S_{d,h}^2}{\rho_d^2} = T, \qquad d \in \mathcal D,}
#' \deqn{x_{d,h} \leq N_{d,h}, \qquad (d,h) \in \mathcal H,}
#' where:
#' \describe{
#'   \item{\eqn{(T,\, \boldsymbol x) = (T,\, (x_{d,h},\, (d,h) \in \mathcal H))}}{
#'     the optimization variable,
#'   }
#'   \item{\eqn{\mathcal H \subset \mathbb N^2}}{
#'     the set of domain-stratum indices,
#'   }
#'   \item{\eqn{\mathcal D := \{d \in \mathbb N \colon\; \exists h,\, (d,h) \in \mathcal H\}}}{
#'     the set of domain indices,
#'   }
#'   \item{\eqn{\mathcal H_d := \{h \in \mathbb N \colon\; (d,h) \in \mathcal H\}}}{
#'     the set of strata indices in domain \eqn{d},
#'   }
#'   \item{\eqn{N_{d,h} > 0}}{size of stratum \eqn{(d,h)},}
#'   \item{\eqn{S_{d,h} > 0}}{standard deviation of the study variable in stratum \eqn{(d,h)},}
#'   \item{\eqn{\rho_d := t_d\, \sqrt{\kappa_d}}}{
#'     where \eqn{t_d} denotes the total in domain \eqn{d}, i.e., the sum of the
#'     values of the study variable for population elements in domain \eqn{d},
#'     and \eqn{\kappa_d} is a priority weight for domain \eqn{d},
#'   }
#'   \item{\eqn{n \in (0,\, \sum_{(d,h) \in \mathcal H} N_{d,h}]}}{total sample size.}
#' }
#'
#' @details
#'   The `dopt()` function uses the RDCA algorithm implemented in [rdca()].
#'
#' @inheritParams rdca
#' @param total (`numeric`)\cr vector of domain totals,
#'   \eqn{t_d,\, d \in \mathcal D}, i.e., the sum of the study variable over all
#'   population elements in each domain.
#' @param kappa (`numeric`)\cr vector of priority weights for the domains,
#'   \eqn{\kappa_d,\, d \in \mathcal D}.
#' @param return_T (`logical(1)`)\cr
#'   If `TRUE`, the function returns a list containing the optimal allocation
#'   and the optimal value of the objective function \eqn{T}. If `FALSE`
#'   (default), only the optimal allocation vector is returned.
#'
#' @return
#' If `return_T = FALSE` (default), a numeric vector containing the optimal
#' sample allocations \eqn{x_{d,h}} for each stratum \eqn{(d,h) \in \mathcal H}.
#'
#' If `return_T = TRUE`, a list with components:
#' \describe{
#'   \item{xopt}{numeric vector of optimal sample allocations.}
#'   \item{Topt}{optimal value of the objective function \eqn{T}.}
#' }
#'
#' @seealso [rdca()], [dca()], [dca_nmax()], [opt()], [optcost()].
#
#' @references
#' \insertRef{WojciakPhD}{stratallo}
#'
#' @export
#' @examples
#'
#' # Three domains with 2, 2, and 3 strata, respectively,
#' # that is, H = {(1,1), (1,2), (2,1), (2,2), (3,1), (3,2), (3,3)}.
#' H_counts <- c(2, 2, 3)
#' # (N_{1,1}, N_{1,2}, N_{2,1}, N_{2,2}, N_{3,1}, N_{3,2}, N_{3,3})
#' N <- c(140, 110, 135, 190, 200, 40, 70)
#' # (S_{1,1}, S_{1,2}, S_{2,1}, S_{2,2}, S_{3,1}, S_{3,2}, S_{3,3})
#' S <- c(180, 20, 5, 4, 35, 9, 40)
#' total <- c(2, 3, 5)
#' kappa <- c(0.5, 0.2, 0.3)
#' n <- 828
#'
#' # Optimum allocation.
#' dopt(n, H_counts, N, S, total, kappa)
#'
#' # Example population with 9 domains and 278 strata
#' p <- pop9d278s
#' sum(p$N)
#' n <- 5000
#' x <- dopt(n, p$H_counts, p$N, p$S, p$total, p$kappa, return_T = TRUE)
#' x
#' all(x$xopt <= p$N)
#' sum(x$xopt)
#'
dopt <- function(n, H_counts, N, S, total, kappa, return_T = FALSE) {
  assert_integerish(H_counts, any.missing = FALSE, min.len = 1L)
  assert_true(all(H_counts > 0))

  H_len <- sum(H_counts)
  D_len <- length(H_counts)

  assert_number(n, finite = TRUE)
  assert_integerish(N, any.missing = FALSE, len = H_len)
  assert_numeric(S, finite = TRUE, any.missing = FALSE, len = H_len)
  assert_numeric(total, finite = TRUE, any.missing = FALSE, len = D_len)
  assert_numeric(kappa, finite = TRUE, any.missing = FALSE, len = D_len)
  assert_flag(return_T)
  assert_true(n > 0)
  assert_true(all(N > 0))
  assert_true(all(S > 0))
  assert_true(all(total > 0))
  assert_true(all(kappa > 0))

  rho <- total * sqrt(kappa)
  rho2 <- total^2 * kappa

  x <- rdca(n, H_counts, N, S, rho, rho2)

  if (return_T) {
    obj <- rdca_obj_cnstr(x, n, H_counts, N, S, rho2)
    list(xopt = x, Topt = obj$Topt)
  } else {
    x
  }
}
