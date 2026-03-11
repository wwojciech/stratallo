# STABLE ----

#' @include utils.R
#' @include helpers_dca_rdca.R
NULL

#' @name rdca
#'
#' @title Recursive Domain-Controlled Allocation (RDCA) Algorithm
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Implements the Recursive Domain-Controlled Allocation (RDCA) algorithm
#' described in \insertCite{WojciakPhD;textual}{stratallo}.
#' The algorithm solves the following optimum allocation problem,
#' formulated in mathematical optimization terms:
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
#' @details The upper-bound constraints \eqn{x_{d,h} \le N_{d,h}} are guaranteed
#'   to be preserved only if domain \eqn{d} is in `J`.
#'   The parameter `J` is used in the recursion.
#'   The specified optimization problem is solved when `J = NULL`, i.e., when
#'   `J` contains all domains.
#'
#' @note This function is optimized for internal use and should typically not be
#'   called directly by users.
#'   It is designed to handle a large number of invocations, specifically
#'   recursive invocations of `rdca()`, and, as a result, parameter assertions
#'   are minimal.
#'
#' @references
#' \insertRef{WojciakPhD}{stratallo}
#'
#' @inheritParams dca
#' @param J (`integerish` or `NULL`)\cr vector of domain indices, subset of
#'   \eqn{\mathcal D}.
#'   Specifies the domains for which allocated sample sizes must not exceed
#'   the corresponding strata sizes `N`.
#'   For domains not included in `J`, allocations may exceed strata sizes.
#'   Must not be empty.
#'   If `J` is `NULL`, it is treated as containing all domains.
#'   `U` must not contain any strata from domains included in `J`.
#'
#' @seealso [dca()]
#'
#' @export
#' @examples
#' # Three domains with 2, 2, and 3 strata, respectively,
#' # that is, H = {(1,1), (1,2), (2,1), (2,2), (3,1), (3,2), (3,3)}.
#' H_counts <- c(2, 2, 3)
#' # (N_{1,1}, N_{1,2}, N_{2,1}, N_{2,2}, N_{3,1}, N_{3,2}, N_{3,3})
#' N <- c(140, 110, 135, 190, 200, 40, 70)
#' # (S_{1,1}, S_{1,2}, S_{2,1}, S_{2,2}, S_{3,1}, S_{3,2}, S_{3,3})
#' S <- c(180, 20, 5, 4, 35, 9, 40)
#' total <- c(2, 3, 5)
#' kappa <- c(0.5, 0.2, 0.3)
#' rho <- total * sqrt(kappa) # (rho_1, rho_2, rho_3)
#' rho2 <- total^2 * kappa
#' sum(N)
#' n <- 828
#'
#' # Optimum allocation.
#' rdca(n, H_counts, N, S, rho, rho2)
#'
#' # Upper bounds enforced only for domain 1.
#' rdca(n, H_counts, N, S, rho, rho2, J = 1)
#'
rdca <- function(n, H_counts, N, S, rho, rho2 = rho^2, U = NULL, J = NULL) {
  if (is.null(J)) {
    J <- seq_along(H_counts)
  }
  sumU_N <- sum(N[U])
  if (!(sumU_N < n || (sumU_N == n && sum(N) == n))) {
    stop("U must be such that sum(N[U]) < n || (sum(N[U]) == n && sum(N) == n)")
  }
  if (any(H_cnt2dind(H_counts)[U] %in% J)) {
    stop("U must contain no strata from domains in J")
  }

  # strata global indices for domain j
  H_j <- H_get_strata_indices(H_counts, J[1])
  repeat {
    x <- if (length(J) == 1L) {
      dca(
        n = n, H_counts = H_counts, N = N, S = S, rho = rho, rho2 = rho2, U = U
      )
    } else {
      rdca(
        n = n, H_counts = H_counts, N = N, S = S, rho = rho, rho2 = rho2,
        U = U, J = J[-1]
      )
    }
    Y_Hj <- which(x[H_j] >= N[H_j])
    if (is_nonempty(Y_Hj)) {
      U <- c(U, H_j[Y_Hj])
      H_j <- H_j[-Y_Hj]
    } else {
      break
    }
  }
  return(x)
}

# EXPERIMENTAL (under testing) ----

#' @title Iterative RDCA Implementation
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Iterative implementation of the Recursive Domain-Controlled Allocation (RDCA)
#' algorithm. Not tested.
#'
#' @inheritParams rdca
#' @param ref_domain (`integerish(1)`)\cr reference domain (denoted by j in the
#'   thesis).
#'
#' @examples
#' H_counts <- c(2, 2, 3)
#' N <- c(140, 110, 135, 190, 200, 40, 70)
#' S <- sqrt(c(180, 20, 5, 4, 35, 9, 40))
#' total <- c(2, 3, 5)
#' kappa <- c(0.5, 0.2, 0.3)
#' rho <- total * sqrt(kappa)
#' (n <- dca_nmax(H_counts, N, S) - 1)
#'
#' # experimental function (not exported) – examples skipped
#' \dontrun{
#' rdca_iter(n, H_counts, N, S, rho)
#' # 140.0000 103.6139 132.1970 166.4127 195.9701  19.8750  70.0000
#' }
#'
rdca_iter <- function(n, H_counts, N, S, rho, rho2 = NULL, ref_domain = 1L) {
  if (is.null(rho2)) {
    rho2 <- rho^2
  }

  stopifnot(length(ref_domain) == 1L)
  stopifnot(ref_domain <= length(H_counts))

  H <- H_cnt2glbidx(H_counts) # list with strata global indices, list elements - domains
  H_ref <- H[[ref_domain]] # reference domain (denoted by j in the paper)
  H_nref <- H[-ref_domain] # remaining domains other than j
  H_nref_active <- vector("list", length(H_nref)) # active strata in remaining domains
  D_nref <- seq_along(H_nref) # indices of remaining domains

  repeat {
    # 1. Allocate in a chosen reference domain j
    H_active <- unlist(H_nref_active)
    repeat {
      x <- dca(n, H_counts, N, S, rho, rho2, U = H_active)
      violated <- which(x[H_ref] > N[H_ref])
      if (is_empty(violated)) {
        break
      } else {
        H_active <- c(H_active, H_ref[violated])
        # if (all(H_ref %in% H_active)) { # jesli n < nmax to tak sie nie zdarzy (chyba)
        #   warning("all ineuqality constraints active in the reference domain")
        # }
      }
    }

    # 2. Check for violations in remaining (non recursive) domains.
    for (d in D_nref) {
      H_d <- H_nref[[d]]
      violated <- which(x[H_d] > N[H_d]) # W_d powinno zostac zmniejszone o active temp
      if (is_nonempty(violated)) {
        if (d >= 2 && is_nonempty(H_nref_active[1:(d - 1)])) {
          # wyczysc temp active z poprzednich domen
          H_nref_active[1:(d - 1)] <- list(NULL)
        }
        H_nref_active[[d]] <- c(H_nref_active[[d]], H_d[violated])
        break
      }
    }

    if (is_empty(violated)) {
      break
    }
  }
  x
}
