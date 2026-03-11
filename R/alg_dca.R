# STABLE ----

#' @include utils.R
#' @include helpers_dca_rdca.R
NULL

#' @name dca
#'
#' @title Domain-Controlled Allocation (DCA) Algorithm
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Functions implementing the Domain-Controlled Allocation (DCA)
#' algorithm described in \insertCite{Wesolowski;textual}{stratallo} and
#' \insertCite{WojciakPhD;textual}{stratallo}.
#' The algorithm solves the following
#' optimum allocation problem, formulated in mathematical optimization terms:
#'
#' Minimize
#' \deqn{f(T,\, \boldsymbol x) = T}
#' over \eqn{\mathbb R \times \mathbb R_+^{\lvert \mathcal H \rvert}},
#' subject to
#' \deqn{\sum_{(d,h) \in \mathcal H} x_{d,h} = n,}
#' \deqn{\sum_{h \in \mathcal H_d} (\frac{1}{x_{d,h}} - \frac{1}{N_{d,h}}) \frac{N_{d,h}^2 S_{d,h}^2}{\rho_d^2} = T, \qquad d \in \mathcal D,}
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
#' @details For \eqn{n \in (0,\, n_{max})}, the optimal value satisfies \eqn{T^* > 0},
#'   where
#'   \deqn{
#'     n_{max} := \sum_{d \in \mathcal D}
#'     \frac{\bigl( \sum_{h \in \mathcal H_d} N_{d,h} S_{d,h} \bigr)^2}{\sum_{h \in \mathcal H_d} N_{d,h} S_{d,h}^2}.
#'   }
#'   See Proposition 2.1 in \insertCite{Wesolowski;textual}{stratallo} or
#'   \insertCite{WojciakPhD;textual}{stratallo} for details.
#'   The value \eqn{n_{max}} is less than or equal to `sum(N)` and can be
#'   computed with [dca_nmax()].
#'
#' @note These functions are optimized for internal use and should typically not
#'   be called directly by users. They are designed to handle a large number of
#'   invocations, specifically recursive calls from `rdca()`, and, as a result,
#'   parameter assertions are minimal.
#'
#' @references
#' \insertRef{WojciakPhD}{stratallo}
#'
#' \insertRef{Wesolowski}{stratallo}
#'
#' \insertRef{WJWR2017}{stratallo}
#'
#' @param n (`integerish(1)`)\cr total sample size \eqn{n}. Must satisfy `0 < n <= sum(N)`.
#' @param H_counts (`integerish`)\cr strata counts in each domain.
#' @param N (`integerish`)\cr strata sizes \eqn{(N_{d,h},\, (d,h) \in \mathcal H)}.
#' @param S (`numeric`)\cr standard deviations \eqn{(S_{d,h},\, (d,h) \in \mathcal H)}
#'   of surveyed variable in strata.
#' @param rho (`numeric`)\cr parameters \eqn{(\rho_d,\, d \in \mathcal D)} of the
#'   optimization problem.
#' @param rho2 (`numeric`)\cr the square of `rho` (`rho^2`), provided to reduce
#'   potential loss of precision due to finite-precision arithmetic.
#' @param details (`logical(1)`)\cr whether to produce detailed debug output.
#'
#' @return
#' If `details = FALSE`, the optimal \eqn{\boldsymbol x^*} is returned.
#' Otherwise, a list is returned containing the optimal \eqn{\boldsymbol x^*}
#' (element named `x`) along with other internal details of this algorithm.
#' In particular, the `lambda` element of the list corresponds to the optimal
#' \eqn{T^*}.
#'
#' @seealso [rdca()]
#'
#' @export
#' @examples
#' # Two domains with 1 and 3 strata, respectively,
#' # that is, H = {(1,1), (2,1), (2,2), (2,3)}.
#' H_counts <- c(1, 3)
#' N <- c(140, 110, 135, 190) # (N_{1,1}, N_{2,1}, N_{2,2}, N_{2,3})
#' S <- sqrt(c(180, 20, 5, 4)) # (S_{1,1}, S_{2,1}, S_{2,2}, S_{2,3})
#' total <- c(2, 3)
#' kappa <- c(0.4, 0.6)
#' rho <- total * sqrt(kappa) # (rho_1, rho_2)
#' rho2 <- total^2 * kappa
#' sum(N) # 575
#' n_max <- dca_nmax(H_counts, N, S) # 519.0416
#'
NULL

#' @describeIn dca
#'
#' Domain-Controlled Allocation algorithm by
#' \insertCite{Wesolowski;textual}{stratallo}
#'
#' @export
#' @examples
#' n <- floor(n_max) - 1
#'
#' dca0(n, H_counts, N, S, rho, rho2)
#' x0 <- dca0(n, H_counts, N, S, rho, rho2, details = TRUE)
#' x0$x
#' x0$lambda
#' x0$k
#' x0$v
#' x0$s
#'
#' n <- ceiling(n_max) + 1
#' x0 <- dca0(n, H_counts, N, S, rho, rho2, details = TRUE)
#' x0$x
#' x0$lambda
#'
dca0 <- function(n, H_counts, N, S, rho, rho2, details = FALSE) {
  if (n <= 0 || n > sum(N)) {
    stop("n must be such that n > 0 && n <= sum(N)")
  }

  # H_counts as param instead of H_dind to avoid index misorder.
  H_dind <- H_cnt2dind(H_counts)

  # D matrix
  a.vec <- tapply(N * S, H_dind, sum) / rho
  b.vec <- tapply(N * S^2, H_dind, sum) / rho2 # - b (b = 0 if M = N)
  D.matrix <- (a.vec %*% t(a.vec)) / n - diag(b.vec, nrow = length(b.vec))

  # Eigen
  eigen_decomp <- eigen(D.matrix, symmetric = TRUE)
  lambda <- eigen_decomp$values[1] # largest eigenvalue (can be < 0)
  v <- eigen_decomp$vectors[, 1] # (unit) eigenvector corresponding to lambda,
  # can have all its elements negative - this is not a problem.
  if (has_mixed_signs(v)) {
    stop("eigenvector contains entries of a different sign")
  }

  # allocation
  k <- n / sum(a.vec * v) # scalar - scaling factor
  x <- k * rep(v / rho, H_counts) * N * S

  # old code
  # s.vec <- (n / sum(a.vec * v)) * v # as.numeric(t(a.vec) %*% v)
  # A <- (N * S) / rep(rho, H_counts) # brackets due to finite-prec arithmetic!
  # x <- rep(s.vec, H_counts) * A

  # prepare return object
  if (details) {
    list(
      D.matrix = D.matrix, eigen = eigen_decomp, lambda = lambda,
      k = k, v = v, s = k * v, x = x
    )
  } else {
    x
  }
}

#' @describeIn dca
#'
#' Domain-Controlled Allocation algorithm by
#' \insertCite{Wesolowski;textual}{stratallo}, optionally using a set of
#' take-max strata as described in \insertCite{WojciakPhD;textual}{stratallo}.
#'
#' @param U (`integerish` or `NULL`)\cr a vector of indices identifying the
#'   \emph{take-max} strata, i.e., the strata \eqn{(d,h)} for which the
#'   allocation is fixed to \eqn{x_{d,h} = N_{d,h}}.
#'   The indices refer to the positions of strata in the set \eqn{\mathcal H},
#'   in the same order as in the input vectors (`N`, `S`, etc.).
#'
#'   For example, if \eqn{\mathcal H = \{(1,1),\, (2,1)\}} and
#'   stratum \eqn{(2,1)} is a take-max stratum, then `U = 2`.
#'
#'   If `U` contains all strata from a domain, the dimension of the
#'   D matrix is reduced accordingly.
#'
#'   `U` must satisfy one of the following conditions:
#'   - `n > sum(N[U])`,
#'   - `n = sum(N[U])` and `n = sum(N)`.
#'
#' @export
#' @examples
#' n <- floor(n_max) - 1
#'
#' x1 <- dca(n, H_counts, N, S, rho, rho2, details = TRUE)
#' x1$x
#' x1$x_Uc
#' x1$lambda
#' x1$s
#'
#' dca(n, H_counts, N, S, rho, rho2, U = 1)
#' x2 <- dca(n, H_counts, N, S, rho, rho2, U = 1, details = TRUE)
#' x2$x
#' x2$x_Uc
#' x2$lambda
#' x2$s
#'
dca <- function(n, H_counts, N, S, rho, rho2, U = NULL, details = FALSE) {
  sumU_N <- sum(N[U])
  if (!(n > sumU_N || (n == sumU_N && n == sum(N)))) {
    stop("U must be such that n > sum(N[U]) || (n == sum(N[U]) && n == sum(N))")
  }

  # reduce the population
  if (is_nonempty(U)) {
    S <- S[-U]
    if (is_empty(S)) {
      return(N)
    }
    x <- N
    N <- N[-U]
    n <- n - sumU_N
    H_dind <- H_cnt2dind(H_counts)
    H_dind_Uc <- H_dind[-U]
    H_counts <- tabulate(H_dind_Uc, nbins = length(H_counts))
    D_blocked <- which(H_counts == 0)
    if (is_nonempty(D_blocked)) { # remove blocked domains, if any
      rho <- rho[-D_blocked]
      rho2 <- rho2[-D_blocked]
      H_counts <- H_counts[-D_blocked]
    }
  }

  # allocation for reduced population
  ret <- dca0(
    n = n, H_counts = H_counts, N = N, S = S, rho = rho, rho2 = rho2, details = details
  )

  # prepare return object depending on the details flag
  x_Uc <- if (details) {
    ret$x
  } else {
    ret
  }

  if (is_nonempty(U)) {
    x[-U] <- x_Uc
  } else {
    x <- x_Uc
  }

  if (details) {
    names(ret)[names(ret) == "x"] <- "x_Uc"
    c(ret, list(x = x))
  } else {
    x
  }
}

#' @describeIn dca
#'
#' Computes the maximum total sample size \eqn{n_{max}} such that the
#' optimization problem solved by the Domain-Controlled Allocation (DCA)
#' algorithm admits a strictly positive optimal value \eqn{T^*}.
#'
#' @export
#'
dca_nmax <- function(H_counts, N, S) {
  H_dind <- H_cnt2dind(H_counts)
  sum(tapply(N * S, H_dind, sum)^2 / tapply(N * S^2, H_dind, sum))
}

# EXPERIMENTAL (under testing) ----

#' @title DCA Algorithm for Upper-Bound Constrained Allocations (M <= N)
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Prototype (under testing).
#'
#' @inheritParams dca
#' @param M (`integerish`)\cr upper bounds on sample sizes in strata.
#'   Defaults to `N`.
#'
#' @examples
#' H_counts <- c(5, 2)
#' H_names <- rep(seq_along(H_counts), times = H_counts)
#' S <- c(154, 178, 134, 213, 124, 102, 12)
#' N <- c(100, 100, 100, 100, 100, 100, 100)
#' M <- c(80, 90, 70, 40, 10, 90, 100)
#' names(M) <- names(N) <- H_names
#' total <- c(13, 2)
#' kappa <- c(0.8, 0.2)
#' n <- 150
#'
#' # experimental function (not exported) – examples skipped
#' \dontrun{
#' dca_M(n, H_counts, N, S, total, kappa, M = M, U = 5)
#' #         1         1         1         1         1         2         2
#' # 12.754880 14.742653 11.098402 17.641490 10.000000 74.945462  8.817113
#' }
#'
dca_M <- function(n, H_counts, N, S, rho, rho2, M = N, U = NULL) {
  H_dcount <- length(H_counts)

  # stratum-domain indicators,
  # e.g. for 2 domains with 4 and 2 strata: 1 1 1 1 2 2
  H_di <- H_cnt2dind(H_counts)

  # ASSUMPTION: works only if U does not contain whole domain!
  if (is.null(U)) {
    n_adjusted <- n
    a.vec <- as.matrix(tapply(N * S, H_di, sum) / rho)
    b.vec <- tapply(N * S^2, H_di, sum) / rho2
  } else {
    n_adjusted <- n - sum(M[U])
    a.vec <- as.matrix(tapply(N[-U] * S[-U], H_di[-U], sum) / rho)
    b.vec_b <- vector(mode = "numeric", length = H_dcount)
    b.vec_b[H_di[U]] <- tapply(N[U]^2 * S[U]^2 / M[U], H_di[U], sum)
    b.vec <- (tapply(N * S^2, H_di, sum) - b.vec_b) / rho2
  }
  D.matrix <- (a.vec %*% t(a.vec)) / n_adjusted - diag(b.vec, nrow = length(b.vec))

  eigen_decomp <- eigen(D.matrix, symmetric = TRUE)
  # lambda <- eigen_decomp$values[1] # largest eigenvalue
  v <- eigen_decomp$vectors[, 1] # corresponding eigenvector
  if (has_mixed_signs(v)) {
    stop("eigenvector contains entries of a different sign")
  }

  s.vec <- n_adjusted * v / as.numeric(t(a.vec) %*% v)
  A <- (N * S) / rep(rho, table(H_di)) # brackets due to finite-prec arithmetic!
  x <- rep(s.vec, table(H_di)) * A
  x[U] <- M[U]
  x
}
