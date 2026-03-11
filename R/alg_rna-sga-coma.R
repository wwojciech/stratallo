# STABLE ----

#' @include utils.R
NULL

#' @name alg_1sided
#'
#' @title Algorithms for Optimum Sample Allocation Under One-Sided Bound Constraints
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Functions implementing selected optimum allocation algorithms for solving
#' the optimum sample allocation problem, formulated as follows:
#'
#' Minimize
#' \deqn{f(x_1,\ldots,x_H) = \sum_{h=1}^H \frac{A^2_h}{x_h}}
#' over \eqn{\mathbb R_+^H}, subject to
#' \deqn{\sum_{h=1}^H c_h x_h = c,}
#' and either
#' \deqn{x_h \leq M_h, \qquad h = 1,\ldots,H,}
#' or
#' \deqn{x_h \geq m_h, \qquad h = 1,\ldots,H,}
#' where
#' \eqn{c > 0,\, c_h > 0,\, A_h > 0,\, m_h > 0,\, M_h > 0,\, h = 1,\ldots,H},
#' are given numbers.
#'
#' The following is a list of all available algorithms along with the functions
#' that implement them:
#'
#' * RNA - `rna()`,
#' * LRNA - `rna()`,
#' * SGA - `sga()`,
#' * SGAPLUS - `sgaplus()`,
#' * COMA - `coma()`.
#'
#' See the documentation of a specific function for details.
#'
#' The inequality constraints are optional. The user can choose whether and how
#' they are imposed in the optimization problem, depending on the chosen algorithm:
#' * Lower bounds \eqn{m_1, \ldots, m_H} can be specified only for the LRNA
#'   algorithm (by setting `cmp = .Primitive("<=")` for `rna()`).
#' * Upper bounds \eqn{M_1, \ldots, M_H} are supported by all other algorithms.
#' * Simultaneous constraints (both lower and upper bounds) are not supported
#'   by these functions.
#'
#' The costs \eqn{c_1, \ldots, c_H} of surveying one element in a stratum
#' can be specified by the user only for the RNA and LRNA algorithms.
#' For the remaining algorithms, these costs are fixed at 1, i.e.,
#' \eqn{c_h = 1,\, h = 1,\ldots,H}.
#'
#' @details If no inequality constraints are imposed, the allocation is given by
#'   the Neyman allocation:
#'   \deqn{x_h = \frac{A_h}{\sqrt{c_h}} \frac{c}{\sum_{i=1}^H A_i \sqrt{c_i}},
#'   \qquad h = 1,\ldots,H.}
#'
#'   For the \emph{stratified \eqn{\pi}-estimator} of the population total under
#'   \emph{stratified simple random sampling without replacement} design, the
#'   parameters of the objective function \eqn{f} are
#'   \deqn{A_h = N_h S_h, \qquad h = 1,\ldots,H,}
#'   where \eqn{N_h} denotes the size of stratum \eqn{h} and \eqn{S_h} is the
#'   standard deviation of the study variable in stratum \eqn{h}.
#'
#' @note These functions are optimized for internal use and should typically not
#'   be called directly by users. Use [opt()] or [optcost()] instead.
#'
#' @param total_cost (`numeric(1)`)\cr total survey cost \eqn{c}.
#'   Must be strictly positive.
#'   Additionally:
#'   * If one-sided lower bounds \eqn{m_1, \ldots, m_H} are imposed, it is
#'     required that \eqn{c \geq \sum_{h=1}^H c_h m_h}, i.e.
#'     `total_cost >= sum(unit_costs * bounds)`.
#'   * If one-sided upper bounds \eqn{M_1, \ldots, M_H} are imposed, it is
#'     required that \eqn{c \leq \sum_{h=1}^H c_h M_h}, i.e.
#'     `total_cost <= sum(unit_costs * bounds)`.
#' @param A (`numeric`)\cr population constants \eqn{A_1,\ldots,A_H}.
#'   All values must be strictly positive.
#' @param M (`numeric` or `NULL`)\cr upper bounds \eqn{M_1,\ldots,M_H} optionally
#'   imposed on sample sizes in strata. Set to `NULL` if no upper bounds are imposed.
#'   Otherwise, it is required that `total_cost <= sum(unit_costs * M)`.
#'
#' @return A numeric vector of optimum sample allocations in strata. In the case
#'   of `rna()` only, the return value may also be a [`list`] containing the
#'   optimum allocations and strata assignments.
#'
#' @seealso [opt()], [optcost()], [rnabox()].
#'
#' @references
#' \insertRef{WojciakLRNA}{stratallo}
#'
#' \insertRef{WWW}{stratallo}
#'
#' \insertRef{WojciakMsc}{stratallo}
#'
#' \insertRef{SG}{stratallo}
#'
#' \insertRef{Sarndal}{stratallo}
#'
#' @examples
#' A <- c(3000, 4000, 5000, 2000)
#' m <- c(50, 40, 10, 30) # lower bounds
#' M <- c(100, 90, 70, 80) # upper bounds
NULL

#' @describeIn alg_1sided
#'
#' Implements the Recursive Neyman Algorithm (RNA) and its counterpart,
#' the Lower Recursive Neyman Algorithm (LRNA), designed for the optimum
#' allocation problem with one-sided lower-bound constraints.
#' The RNA is described in \insertCite{WWW;textual}{stratallo}, whereas the LRNA
#' is introduced in \insertCite{WojciakLRNA;textual}{stratallo}.
#'
#' @param bounds (`numeric` or `NULL`)\cr optional lower bounds \eqn{m_1,\ldots,m_H},
#'   or upper bounds \eqn{M_1,\ldots,M_H}, or `NULL` to indicate that no inequality
#'   constraints are imposed.
#'   If not `NULL`, `bounds` is interpreted as:
#'   * lower bounds, if `cmp = .Primitive("<=")`, or
#'   * upper bounds, if `cmp = .Primitive(">=")`.
#'
#'   See also `total_cost`.
#' @param unit_costs (`numeric`)\cr costs \eqn{c_1,\ldots,c_H} of surveying one
#'   element in each stratum. Strictly positive values. May also be of length 1,
#'   in which case the value is recycled to match the length of `bounds`.
#' @param cmp (`function`)\cr a binary comparison operator used to check for
#'   violations of `bounds`.
#'   Must be either `.Primitive("<=")` (treating `bounds` as lower bounds and
#'   invoking the LRNA algorithm) or `.Primitive(">=")` (treating `bounds` as
#'   upper bounds and invoking the RNA algorithm).
#'
#'   The value of this argument has no effect if `bounds` is `NULL`.
#' @param details (`logical(1)`)\cr should detailed information on stratum
#'   assignments (either take-Neyman or take-bound), values of the set function
#'   \eqn{s}, and the number of iterations be included in the output?
#'
#' @export
#' @examples
#'
#' rna(total_cost = 190, A = A, bounds = M)
#'
#' rna(total_cost = 190, A = A, bounds = m, cmp = .Primitive("<="))
#'
#' rna(total_cost = 300, A = A, bounds = M)
#'
rna <- function(total_cost,
                A,
                bounds = NULL,
                unit_costs = 1,
                cmp = .Primitive(">="),
                details = FALSE) {
  i <- 0L # Iteration index, for debugging purposes only.
  c_sqrt <- sqrt(unit_costs)
  A_c <- A * c_sqrt
  A_over_c <- A / c_sqrt
  s0 <- s <- total_cost / sum(A_c)
  x <- s * A_over_c # Neyman allocation.
  V <- cmp(x, bounds)

  if (any(V)) {
    W <- seq_along(A_c) # Set of strata original indices. To be shrunk in repeat loop.
    bounds_c <- bounds * unit_costs
    for (i in W) {
      total_cost <- total_cost - sum(bounds_c[W[V]])
      W <- W[!V] # W <- W \ W[V]
      # Neyman allocation for W. Denominator could be accumulated (i.e.
      # A_c_sum <- A_c_sum - sum(A_c[W[V]]), where first A_c_sum = A_c[1]+...+A_c[H]),
      # but resigned from this version due to finite precision arithmetic issues.
      s <- total_cost / sum(A_c[W])
      x <- s * A_over_c[W]
      V <- cmp(x, bounds[W]) # Indices of W for which x violates bounds.
      if (!any(V)) {
        bounds[W] <- x
        break
      }
    }
    if (details) {
      take_bound <- if (length(W) == 0L) {
        seq_along(A) # Vertex allocation.
      } else {
        seq_along(A)[-W]
      }
      list(opt = bounds, take_neyman = W, take_bound = take_bound, s0 = s0, s = s, iter = i + 1L)
    } else {
      bounds
    }
  } else if (details) {
    list(opt = x, take_neyman = seq_along(A_c), take_bound = integer(0), s0 = s0, s = s, iter = i + 1L)
  } else {
    x
  }
}

#' @describeIn alg_1sided
#'
#' The Stenger-Gabler (SGA) algorithm, as proposed by
#' \insertCite{SG;textual}{stratallo} and described in
#' \insertCite{WWW;textual}{stratallo}.
#' This algorithm solves the optimum allocation problem with one-sided
#' upper-bound constraints. It assumes unit costs are constant and equal to 1,
#' i.e., \eqn{c_h = 1,\, h = 1,\ldots,H}.
#'
#' @export
#' @examples
#' sga(total_cost = 190, A = A, M = M)
#'
sga <- function(total_cost, A, M) {
  H <- length(A) # Number of strata.
  AoM <- A / M
  # W - strata indices sorted with regard to non-increasing order of a/M.
  # The ordering preserves relative order of the elements with equivalent values.
  W <- order(AoM, decreasing = TRUE)

  Asum <- sum(A)
  for (i in 1:H) {
    s_inv <- Asum / total_cost # Inverse of s function.
    h <- W[i]
    if (AoM[h] < s_inv) {
      break
    } else {
      total_cost <- total_cost - M[h]
      Asum <- Asum - A[h]
    }
  }

  if (i == 1L) { # To improve the performance, otherwise, else block only.
    A / s_inv
  } else {
    V <- W[i:H]
    M[V] <- A[V] / s_inv
    M
  }
}

#' @describeIn alg_1sided
#'
#' A modified Stenger-Gabler-type algorithm, described in
#' \insertCite{WojciakMsc;textual}{stratallo}, implemented as the
#' Sequential Allocation (version 1) algorithm.
#' This algorithm solves the optimum allocation problem with one-sided
#' upper-bound constraints. It assumes unit costs are constant and equal to 1,
#' i.e., \eqn{c_h = 1,\, h = 1,\ldots,H}.
#'
#' @export
#' @examples
#' sgaplus(total_cost = 190, A = A, M = M)
#'
sgaplus <- function(total_cost, A, M) {
  H <- length(A) # Number of strata.
  AoM <- A / M
  # W - strata indices sorted with regard to non-increasing order of a/M.
  # The ordering preserves relative order of the elements with equivalent values.
  W <- order(AoM, decreasing = TRUE)

  Asum <- sum(A)
  ksi_inv <- Asum / total_cost
  i <- 1L
  j <- 1L
  repeat {
    if (AoM[W[i]] < ksi_inv) { # Allocation for stratum W[i] does not exceed M.
      if (j < i) { # It must not be exceeded also when W = W[-(1:(i-1))]
        Rj <- W[j:(i - 1L)]
        total_cost <- total_cost - sum(M[Rj])
        Asum <- Asum - sum(A[Rj])
        ksi_inv <- Asum / total_cost
        j <- i
      } else {
        break
      }
    } else if (i == H) { # Last stratum in W cannot be exceeded, allocation = M.
      return(M)
    } else {
      i <- i + 1L
    }
  }

  if (i == 1L) { # To improve the performance, otherwise, else block only.
    A / ksi_inv
  } else {
    V <- W[i:H]
    M[V] <- A[V] / ksi_inv
    M
  }
}

#' @describeIn alg_1sided
#'
#' The Change of Monotonicity Algorithm (COMA), described in
#' \insertCite{WWW;textual}{stratallo}, solves the optimum allocation problem
#' with one-sided upper-bound constraints. It assumes unit costs are constant
#' and equal to 1, i.e., \eqn{c_h = 1,\, h = 1,\ldots,H}.
#'
#' @export
#' @examples
#' coma(total_cost = 190, A = A, M = M)
#'
coma <- function(total_cost, A, M) {
  H <- length(A) # Number of strata.
  if (H == 1L) {
    return(min(total_cost, M))
  }
  # W - strata indices sorted with regard to non-increasing order of a/M.
  # The ordering preserves relative order of the elements with equivalent values.
  W <- order(A / M, decreasing = TRUE)

  Asum <- sum(A)
  s <- total_cost / Asum
  for (i in 1:(H - 1)) {
    h <- W[i]
    total_cost <- total_cost - M[h]
    Asum <- Asum - A[h]
    s_next <- total_cost / Asum

    if (s > s_next) { # Change of monotonicity found.
      break
    } else {
      s <- s_next
    }
  }

  i <- ifelse(i == H - 1 && s <= s_next, H, i)

  if (i == 1) { # To improve the performance, otherwise, else block only.
    A * s
  } else {
    V <- W[i:H]
    M[V] <- A[V] * s
    M
  }
}

# EXPERIMENTAL ----

#' @name rna_experimental
#'
#' @title RNA – Experimental Versions
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Experimental variants of the Recursive Neyman Algorithm (RNA).
#'
#' @inheritParams rna
#'
#' @note This code has not been extensively tested and may change in future
#'   releases.
#'
#' @examples
#' A <- c(3000, 4000, 5000, 2000)
#' M <- c(100, 90, 70, 80) # upper bounds
#'
NULL

#' @describeIn rna_experimental
#'
#' Recursive implementation of the RNA.
#'
#' @examples
#' # experimental function (not exported) – examples skipped
#' \dontrun{
#' rna_rec(total_cost = 190, A = A, bounds = M)
#' rna_rec(total_cost = 312, A = A, bounds = M)
#' rna_rec(total_cost = 339, A = A, bounds = M)
#' rna_rec(total_cost = 340, A = A, bounds = M)
#' }
#'
rna_rec <- function(total_cost,
                    A,
                    bounds = NULL,
                    unit_costs = rep(1, length(A)),
                    cmp = .Primitive(">=")) {
  x <- (total_cost / sum(A * sqrt(unit_costs))) * (A / sqrt(unit_costs)) # Neyman allocation.
  t_bound <- cmp(x, bounds) # take-bound

  if (any(t_bound)) {
    total_cost <- total_cost - sum(unit_costs[t_bound] * bounds[t_bound])
    t_neyman <- !t_bound # take-Neyman
    bounds[t_neyman] <- rna_rec(
      total_cost, A[t_neyman], bounds[t_neyman], unit_costs[t_neyman], cmp
    )
    bounds
  } else {
    x
  }
}

#' @describeIn rna_experimental
#'
#' A variant of the Recursive Neyman Algorithm (RNA) that uses prior information
#' about strata for which allocation constraints may be violated. For all other
#' strata, allocations are assumed to satisfy the bounds.
#'
#' @param check (`integer`)\cr indices of strata for which allocation constraints
#'   may be violated. For all other strata, constraint violations are not checked.
#'
rna_prior <- function(total_cost,
                      A,
                      bounds = NULL,
                      check = NULL,
                      cmp = .Primitive(">="),
                      details = FALSE) {
  s <- total_cost / sum(A)
  V <- cmp(s * A[check], bounds[check])

  if (any(V)) {
    check_ind <- seq_along(check)
    repeat {
      total_cost <- total_cost - sum(bounds[check[check_ind[V]]])
      check_ind <- check_ind[!V]
      tN <- -check[-check_ind]
      tB <- check[check_ind]
      s <- total_cost / sum(A[tN])
      V <- cmp(s * A[tB], bounds[tB])
      if (!any(V)) {
        bounds[tN] <- s * A[tN]
        break
      } else if (sum(V) == length(check_ind)) {
        bounds[-check] <- (total_cost - sum(bounds[check[check_ind[V]]])) / sum(A[-check]) * A[-check]
        break
      }
    }
    bounds
  } else {
    s * A
  }
}

# Does not work when: check == take-bound or V is NULL.
rna_prior1 <- function(total_cost,
                       A,
                       bounds = NULL,
                       check = NULL,
                       cmp = .Primitive(">="),
                       details = FALSE) {
  s <- total_cost / sum(A)
  V <- NULL
  repeat {
    Vi <- cmp(s * A[check], bounds[check])
    if (any(Vi)) {
      total_cost <- total_cost - sum(bounds[check[Vi]])
      V <- c(V, check[Vi])
      check <- check[!Vi]
      s <- total_cost / sum(A[-V])
    } else {
      bounds[-V] <- s * A[-V]
      break
    }
  }
  bounds
}

# Works well, the best out of all the options so far.
rna_prior2 <- function(total_cost,
                       A,
                       bounds = NULL,
                       check = NULL,
                       cmp = .Primitive(">="),
                       details = FALSE) {
  s <- total_cost / sum(A)
  tN_map <- !logical(length = length(A)) # Map with strata to take-Neyman
  repeat {
    V <- cmp(s * A[check], bounds[check])
    if (any(V)) {
      tB <- check[V]
      tN_map[tB] <- FALSE
      check <- check[!V]
      total_cost <- total_cost - sum(bounds[tB])
      A_tN <- A[tN_map]
      s <- total_cost / sum(A_tN)
    } else {
      bounds[tN_map] <- s * A_tN
      break
    }
  }
  bounds
}

# Works well.
rna_prior3 <- function(total_cost,
                       A,
                       bounds = NULL,
                       check = NULL,
                       cmp = .Primitive(">="),
                       details = FALSE) {
  s <- total_cost / sum(A)
  tN <- seq_along(A) # Strata indices for take-Neyman.
  repeat {
    V <- cmp(s * A[check], bounds[check])
    if (any(V)) {
      tB <- check[V]
      tN[tB] <- 0L
      check <- check[!V]
      total_cost <- total_cost - sum(bounds[tB])
      A_tN <- A[tN]
      s <- total_cost / sum(A_tN)
    } else {
      bounds[tN] <- s * A_tN
      break
    }
  }
  bounds
}

# with take_bound
rna_prior2a <- function(total_cost,
                        A,
                        bounds = NULL,
                        check = seq_along(A),
                        take_bound = NULL,
                        cmp = .Primitive(">="),
                        details = FALSE) {
  tN_map <- !logical(length = length(A)) # Map with strata to take-Neyman
  tN_map[take_bound] <- FALSE
  check <- setdiff(check, take_bound)
  repeat {
    A_tN <- A[tN_map]
    s <- total_cost / sum(A_tN)
    V <- cmp(s * A[check], bounds[check])
    if (any(V)) {
      tB <- check[V]
      tN_map[tB] <- FALSE
      check <- check[!V]
      total_cost <- total_cost - sum(bounds[tB])
    } else {
      bounds[tN_map] <- s * A_tN
      break
    }
  }
  bounds
}
