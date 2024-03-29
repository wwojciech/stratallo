#' @include helpers.R
NULL

#' Algorithms for Optimum Sample Allocation Under One-Sided Bounds
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Functions that implement selected optimal allocation algorithms that compute
#' a solution to the optimal allocation problem defined in the language of
#' mathematical optimization as follows.
#'
#' Minimize
#' \deqn{f(x_1,\ldots,x_H) = \sum_{h=1}^H \frac{A^2_h}{x_h}}
#' subject to
#' \deqn{\sum_{h=1}^H c_h x_h = c}
#' and either
#' \deqn{x_h \leq M_h, \quad h = 1,\ldots,H}
#' or
#' \deqn{x_h \geq m_h, \quad h = 1,\ldots,H,}
#' where
#' \eqn{c > 0,\, c_h > 0,\, A_h > 0,\, m_h > 0,\, M_h > 0,\, h = 1,\ldots,H},
#' are given numbers. The minimization is on \eqn{\mathbb R_+^H}.
#'
#' The inequality constraints are optional and user can choose whether and how
#' they are to be added to the optimization problem.
#' If one-sided lower bounds \eqn{m_h,\, h = 1,\ldots,H}, must be imposed, it
#' is then required that \eqn{c \geq \sum_{h=1}^H c_h m_h}.
#' If one-sided upper bounds \eqn{M_h,\, h = 1,\ldots,H}, must be imposed, it
#' is then required that \eqn{0 < c \leq \sum_{h=1}^H c_h M_h}.
#' Lower bounds can be specified instead of the upper bounds only in case of the
#' \emph{LRNA} algorithm. All other algorithms allow only for specification of
#' the upper bounds. For the sake of clarity, we emphasize that in the
#' optimization problem consider here, the lower and upper bounds cannot be
#' imposed jointly.
#'
#' Costs \eqn{c_h,\, h = 1,\ldots,H}, of surveying one element in stratum, can
#' be specified by the user only in case of the \emph{RNA} and \emph{LRNA}
#' algorithms. For remaining algorithms, these costs are fixed at 1, i.e.
#' \eqn{c_h = 1,\, h = 1,\ldots,H}.
#'
#' The following is the list of all the algorithms available to use along with
#' the name of the function that implements a given algorithm. See the
#' description of a specific function to find out more about the corresponding
#' algorithm.
#' * \emph{RNA} - `rna()`
#' * \emph{LRNA}- `rna()`
#' * \emph{SGA}- `sga()`
#' * \emph{SGAPLUS} - `sgaplus()`
#' * \emph{COMA} - `coma()`
#'
#' Functions in this family should not be called directly by the user. Use
#' [opt()] or [optcost()] instead.
#'
#' @note If no inequality constraints are added, the allocation is given by the
#'   Neyman allocation as:
#'   \deqn{x_h = \frac{A_h}{\sqrt{c_h}} \frac{n}{\sum_{i=1}^H A_i \sqrt{c_i}},
#'   \quad h = 1,\ldots,H.}
#'   For \emph{stratified \eqn{\pi} estimator} of the population total with
#'   \emph{stratified simple random sampling without replacement} design in use,
#'   the parameters of the objective function \eqn{f} are:
#'   \deqn{A_h = N_h S_h, \quad h = 1,\ldots,H,}
#'   where \eqn{N_h} is the size of stratum \eqn{h} and \eqn{S_h} denotes
#'   standard deviation of a given study variable in stratum \eqn{h}.
#'
#' @param total_cost (`number`)\cr total cost \eqn{c} of the survey. A strictly
#'   positive scalar.
#' @param A (`numeric`)\cr population constants \eqn{A_1,\ldots,A_H}. Strictly
#'   positive numbers.
#' @param M (`numeric` or `NULL`)\cr upper bounds \eqn{M_1,\ldots,M_H},
#'   optionally imposed on sample sizes in strata. If no upper bounds should be
#'   imposed, then `M` must be set to `NULL`. Otherwise, it is required that
#'   `total_cost <= sum(unit_costs * M)`. Strictly positive numbers.
#'
#' @return Numeric vector with optimal sample allocations in strata. In case
#'   of the `rna()` only, it can also be a [`list`] with optimal sample
#'   allocations and strata assignments (either to take-Neyman or take-bound).
#'
#' @seealso [opt()], [optcost()], [rnabox()].
#'
#' @references
#'   Wójciak, W. (2023).
#'   Another Solution of Some Optimum Allocation Problem.
#'   *Statistics in Transition new series*, 24(5) (in press).
#'   <https://arxiv.org/abs/2204.04035> \cr
#'
#'   Wesołowski, J., Wieczorkowski, R., Wójciak, W. (2021).
#'   Optimality of the Recursive Neyman Allocation.
#'   *Journal of Survey Statistics and Methodology*, 10(5), pp. 1263–1275.
#'   \doi{10.1093/jssam/smab018},
#'   \doi{10.48550/arXiv.2105.14486} \cr
#'
#'   Wójciak, W. (2019). Optimal Allocation in Stratified Sampling Schemes.
#'   *MSc Thesis*, Warsaw University of Technology, Warsaw, Poland.
#'   <http://home.elka.pw.edu.pl/~wwojciak/msc_optimal_allocation.pdf> \cr
#'
#'   Stenger, H., Gabler, S. (2005).
#'   Combining random sampling and census strategies -
#'   Justification of inclusion probabilities equal to 1.
#'   *Metrika*, 61(2), pp. 137–156.
#'   \doi{10.1007/s001840400328} \cr
#'
#'   Särndal, C.-E., Swensson, B. and Wretman, J. (1992).
#'   *Model Assisted Survey Sampling*, Springer, New York.
#'
#' @name opt_1sided
#' @examples
#' A <- c(3000, 4000, 5000, 2000)
#' m <- c(50, 40, 10, 30) # lower bounds
#' M <- c(100, 90, 70, 80) # upper bounds
NULL

#' @describeIn opt_1sided \emph{Recursive Neyman Algorithm} (\emph{RNA}) and its
#'   twin version, \emph{Lower Recursive Neyman Algorithm} (\emph{LRNA})
#'   dedicated to the allocation problem with one-sided lower-bounds constraints.
#'   The \emph{RNA} is described in Wesołowski et al. (2021), while \emph{LRNA} is
#'   introduced in Wójciak (2023).
#'
#' @param bounds (`numeric` or `NULL`) \cr optional lower bounds
#'   \eqn{m_1,\ldots,m_H}, or upper bounds \eqn{M_1,\ldots,M_H}, or `NULL` to
#'   indicate that there is no inequality constraints in the optimization
#'   problem considered.
#'   If not `NULL`, the `bounds` is to be treated either as:
#'   * lower bounds, if `check_violations = .Primitive("<=")`. In this case, it
#'     is required that `total_cost >= sum(unit_costs * bounds)`, \cr
#'   or
#'   * upper bounds, if `check_violations = .Primitive(">=")`. In this case, it
#'     is required that `total_cost <= sum(unit_costs * bounds)`.
#' @param unit_costs (`numeric`)\cr costs \eqn{c_1,\ldots,c_H}, of surveying one
#'   element in stratum. A strictly positive numbers. Can be also of length 1,
#'   if all unit costs are the same for all strata. In this case, the elements
#'   will be recycled to the length of `bounds`.
#' @param check_violations (`function`) \cr 2-arguments binary operator function
#'   that allows the comparison of values in atomic vectors. It must either be
#'   set to `.Primitive("<=")` or `.Primitive(">=")`. The first of these choices
#'   causes that `bounds` are treated as lower bounds and then `rna()` function
#'   performs the \emph{LRNA} algorithm. The latter option causes that `bounds`
#'   are treated as upper bounds, and then `rna()` function performs the
#'   \emph{RNA} algorithm. This argument is ignored when `bounds` is set to `NULL`.
#' @param details (`flag`) \cr should detailed information about strata
#'   assignments (either to take-Neyman or take-bound), values of set function
#'   \eqn{s} and number of iterations be added to the output?
#'
#' @export
#' @examples
#'
#' rna(total_cost = 190, A = A, bounds = M)
#' rna(total_cost = 190, A = A, bounds = m, check_violations = .Primitive("<="))
rna <- function(total_cost,
                A,
                bounds = NULL,
                unit_costs = 1,
                check_violations = .Primitive(">="),
                details = FALSE) {
  i <- 0L # Iteration index, for debugging purposes only.
  c_sqrt <- sqrt(unit_costs)
  A_c <- A * c_sqrt
  A_over_c <- A / c_sqrt
  s0 <- s <- total_cost / sum(A_c)
  x <- s * A_over_c # Neyman allocation.
  V <- check_violations(x, bounds)

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
      V <- check_violations(x, bounds[W]) # Indices of W for which x violates bounds.
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

#' @describeIn opt_1sided Stenger-Gabler type algorithm \emph{SGA}, described in
#'   Wesołowski et al. (2021) and in Stenger and Gabler (2005).
#'   This algorithm solves the problem with one-sided upper-bounds constraints.
#'   It also assumes unit costs are constant and equal to 1, i.e.
#'   \eqn{c_h = 1,\, h = 1,\ldots,H}.
#'
#' @export
#' @examples
#' sga(total_cost = 190, A = A, M = M)
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

#' @describeIn opt_1sided modified Stenger-Gabler type algorithm, described in
#'   Wójciak (2019) as \emph{Sequential Allocation (version 1)} algorithm.
#'   This algorithm solves the problem with one-sided upper-bounds constraints.
#'   It also assumes unit costs are constant and equal to 1, i.e.
#'   \eqn{c_h = 1,\, h = 1,\ldots,H}.
#'
#' @export
#' @examples
#' sgaplus(total_cost = 190, A = A, M = M)
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

#' @describeIn opt_1sided \emph{Change of Monotonicity Algorithm} (\emph{COMA}),
#'   described in Wesołowski et al. (2021).
#'   This algorithm solves the problem with one-sided upper-bounds constraints.
#'   It also assumes unit costs are constant and equal to 1, i.e.
#'   \eqn{c_h = 1,\, h = 1,\ldots,H}.
#'
#' @export
#' @examples
#' coma(total_cost = 190, A = A, M = M)
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

#' @title RNA - Recursive Implementation
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams rna
#'
#' @note this coded was not extensively tested.
#'
#' @export
#' @examples
#' A <- c(3000, 4000, 5000, 2000)
#' M <- c(100, 90, 70, 80) # upper bounds.
#' rna_rec(total_cost = 190, A = A, bounds = M)
#' rna_rec(total_cost = 312, A = A, bounds = M)
#' rna_rec(total_cost = 339, A = A, bounds = M)
#' rna_rec(total_cost = 340, A = A, bounds = M)
rna_rec <- function(total_cost, A, bounds = NULL, unit_costs = rep(1, length(A)), check_violations = .Primitive(">=")) {
  x <- (total_cost / sum(A * sqrt(unit_costs))) * (A / sqrt(unit_costs)) # Neyman allocation.
  t_bound <- check_violations(x, bounds) # take-bound

  if (any(t_bound)) {
    total_cost <- total_cost - sum(unit_costs[t_bound] * bounds[t_bound])
    t_neyman <- !t_bound # take-Neyman
    bounds[t_neyman] <- rna_rec(
      total_cost, A[t_neyman], bounds[t_neyman], unit_costs[t_neyman], check_violations
    )
    bounds
  } else {
    x
  }
}

#' @title RNA in version that uses prior information about violations
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This is the version of the RNA that makes use of additional information
#' about strata for which the allocation can possibly be violated. For all other
#' strata allocation will not be violated.
#'
#' @note this coded was not extensively tested.
#'
#' @inheritParams rna
#' @param check (`integer`)\cr strata indices for which the allocation can
#'   possible be violated. For other strata allocation cannot be violated.
#'
rna_prior <- function(total_cost, A, bounds = NULL, check = NULL, check_violations = .Primitive(">="), details = FALSE) {
  s <- total_cost / sum(A)
  V <- check_violations(s * A[check], bounds[check])

  if (any(V)) {
    check_ind <- seq_along(check)
    repeat {
      total_cost <- total_cost - sum(bounds[check[check_ind[V]]])
      check_ind <- check_ind[!V]
      tN <- -check[-check_ind]
      tB <- check[check_ind]
      s <- total_cost / sum(A[tN])
      V <- check_violations(s * A[tB], bounds[tB])
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
rna_prior1 <- function(total_cost, A, bounds = NULL, check = NULL, check_violations = .Primitive(">="), details = FALSE) {
  s <- total_cost / sum(A)
  V <- NULL
  repeat {
    Vi <- check_violations(s * A[check], bounds[check])
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
rna_prior2 <- function(total_cost, A, bounds = NULL, check = NULL, check_violations = .Primitive(">="), details = FALSE) {
  s <- total_cost / sum(A)
  tN_map <- !logical(length = length(A)) # Map with strata to take-Neyman
  repeat {
    V <- check_violations(s * A[check], bounds[check])
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
rna_prior3 <- function(total_cost, A, bounds = NULL, check = NULL, check_violations = .Primitive(">="), details = FALSE) {
  s <- total_cost / sum(A)
  tN <- seq_along(A) # Strata indices for take-Neyman.
  repeat {
    V <- check_violations(s * A[check], bounds[check])
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
rna_prior2a <- function(total_cost, A, bounds = NULL, check = seq_along(A), take_bound = NULL, check_violations = .Primitive(">="), details = FALSE) {
  tN_map <- !logical(length = length(A)) # Map with strata to take-Neyman
  tN_map[take_bound] <- FALSE
  check <- setdiff(check, take_bound)
  repeat {
    A_tN <- A[tN_map]
    s <- total_cost / sum(A_tN)
    V <- check_violations(s * A[check], bounds[check])
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
