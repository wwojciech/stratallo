#' @name check_rdca
#'
#' @title Internal Diagnostic Functions for Checking Optimality of `rdca()`
#'   Allocations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Diagnostic tools to verify the objective function and constraint
#' satisfaction for allocations computed by the [rdca()] algorithm.
#'
#' @param x (numeric)\cr vector of sample allocations.
#' @param rho2 (`numeric`)\cr the square of `rho` (`rho^2`).
#' @inheritParams dca0
#'
#' @examples
#' H_counts <- c(2, 2) # 2 domains with 2 strata each
#' N <- c(140, 110, 135, 190)
#' S <- sqrt(c(180, 20, 5, 4))
#' total <- c(2, 3)
#' kappa <- c(0.4, 0.6)
#' rho <- total * sqrt(kappa)
#' rho2 <- total^2 * kappa
#' n <- 500
#'
#' (x <- dca(n, H_counts, N, S, rho, rho2))
#'
#' # internal functions (not exported) – examples skipped
#'
NULL

#' @describeIn check_rdca
#'
#' Compute the value of the objective function and constraint functions for a
#' given allocation.
#'
#' @importFrom stats setNames
#'
#' @param J (`integerish` or `NULL`)\cr vector of domain indices, a subset of
#'   \eqn{\mathcal D}. Specifies domains \eqn{d \in \mathcal D} for which
#'   values of constraint functions \eqn{g_{dh}} are computed.
#'   If `integer(0)`, these values are not computed. If `NULL`, all domains are
#'   included.
#'
#' @examples
#' \dontrun{
#' rdca_obj_cnstr(x, n, H_counts, N, S, rho2)
#' rdca_obj_cnstr(x, n, H_counts, N, S, rho2, 2)
#' rdca_obj_cnstr(x, n, H_counts, N, S, rho2, NULL)
#' }
#'
rdca_obj_cnstr <- function(x, n, H_counts, N, S, rho2, J = integer(0)) {
  # Topt (objective)
  Td_sum <- tapply(N^2 / x * S^2 - N * S^2, H_cnt2dind(H_counts), sum)
  Td_sum <- setNames(as.vector(Td_sum), names(Td_sum))
  Topt <- sum(Td_sum) / sum(rho2)

  # equality constraint h(x)
  h <- sum(x) - n

  # equality constraints h_d(x)
  h_d <- Td_sum / rho2 - Topt

  rval <- list(Topt = Topt, h = h, h_d = h_d)

  # inequality constraints g_dh(x) for all d \in J
  if (is.null(J)) {
    J <- seq_along(H_counts)
  }
  if (is_nonempty(J)) {
    g_dh_names <- paste0("(", H_cnt2dind(H_counts), ",", sequence(H_counts), ")")

    strata_in_J <- unlist(H_cnt2glbidx(H_counts)[J])
    g_dh <- x[strata_in_J] - N[strata_in_J]
    names(g_dh) <- g_dh_names[strata_in_J]

    rval <- c(rval, list(g_dh = g_dh))
  }

  rval
}

#' @describeIn check_rdca
#'
#' Check whether the equality and inequality constraints are satisfied
#' for a given allocation, within a specified tolerance.
#' The tolerance applies to equality constraints only.
#'
#' @inheritParams rdca_obj_cnstr
#' @inheritParams is_equal
#'
#' @examples
#' \dontrun{
#' rdca_cnstr_check(x, n, H_counts, N, S, rho2)
#' rdca_cnstr_check(x, n, H_counts, N, S, rho2, 1)
#' rdca_cnstr_check(x, n, H_counts, N, S, rho2, 2)
#' rdca_cnstr_check(x, n, H_counts, N, S, rho2, NULL)
#' }
#'
rdca_cnstr_check <- function(x, n, H_counts, N, S, rho2, J = integer(0), tol_max = 0.1) {
  values <- rdca_obj_cnstr(x, n, H_counts, N, S, rho2, J)

  cnstr_names <- setdiff(names(values), "Topt")
  sapply(cnstr_names, function(i) {
    if (i != "g_dh") { # equality constraints
      is_equal(values[[i]], 0, tol_max = tol_max)
    } else {
      values[[i]] <= tol_max
    }
  })
}

#' @describeIn check_rdca
#'
#' Check the optimality condition related to \eqn{s}.
#' Specifically, verifies whether
#' \eqn{s(\mathcal{U},\, \boldsymbol{v},\, d \mid p) \ge \rho_d / S_{d,h}}
#' for all \eqn{(d,h) \in \mathcal{U}} such that \eqn{d} is not fully blocked
#' by \eqn{\mathcal{U}}.
#'
#' @param s (`numeric`)\cr vector of values for function
#'   \eqn{s(\mathcal U, \boldsymbol{v}, d \mid p)} calculated only for domains
#'   included in `U` that are not fully blocked by the take-max assignment.
#' @param return_diff (`logical(1)`)\cr
#'   If `FALSE`, the function returns a logical vector indicating whether the
#'   optimality condition
#'   \deqn{s(\mathcal{U},\, \boldsymbol{v},\, d \mid p) \ge \rho_d / S_{d,h}}
#'   is satisfied for each unblocked stratum \eqn{(d,h) \in \mathcal U.}
#'   If `TRUE`, the function returns the **differences**
#'   \deqn{s(\mathcal{U},\, \boldsymbol{v},\, d \mid p) - \rho_d / S_{d,h}}
#'   instead of a logical vector, which can be used to assess by how much the
#'   condition is satisfied or violated.
#'
#' @examples
#' \dontrun{
#' (n <- dca_nmax(H_counts, N, S) - 1)
#'
#' U <- 1
#' (x <- dca(n, H_counts, N, S, rho, rho2, U = U, details = TRUE))
#' rdca_optcond_sU(H_counts, S, rho, x$s, U) # TRUE
#'
#' U <- 2
#' (x <- dca(n, H_counts, N, S, rho, rho2, U = U, details = TRUE))
#' rdca_optcond_sU(H_counts, S, rho, x$s, U) # FALSE
#'
#' U <- 3
#' (x <- dca(n, H_counts, N, S, rho, rho2, U = U, details = TRUE))
#' rdca_optcond_sU(H_counts, S, rho, x$s, U) # TRUE
#'
#' U <- 1:2 # domain 2 blocked
#' (x <- dca(n, H_counts, N, S, rho, rho2, U = U, details = TRUE))
#' rdca_optcond_sU(H_counts, S, rho, x$s, U) # no unblocked strata in `U`
#' }
#'
rdca_optcond_sU <- function(H_counts, S, rho, s, U, return_diff = FALSE) {
  U <- sort(U)
  H_dind <- H_cnt2dind(H_counts)

  # 1. For all strata in U, strata counts per domain.
  U_counts <- tabulate(H_dind[U], nbins = length(H_counts))

  # 2. Remove blocked domains.
  blocked_domains <- which(H_counts - U_counts == 0)
  if (is_nonempty(blocked_domains)) {
    U_nb <- U[!H_dind[U] %in% blocked_domains]
    U_nb_counts <- U_counts[-blocked_domains]
  } else {
    U_nb <- U
    U_nb_counts <- U_counts
  }

  # 3. Replicate `rho`, `s`, and `S`, according to the number of strata in each unblocked domain.
  rho_nb_rep <- rho[H_dind[U_nb]]
  s_rep <- rep(s, times = U_nb_counts) # Note: `s` contains values only for non-blocked domains.

  if (return_diff) {
    s_rep - rho_nb_rep / S[U_nb]
  } else {
    s_rep >= rho_nb_rep / S[U_nb]
  }
}
