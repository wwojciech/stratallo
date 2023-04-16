#' @include helpers.R
NULL

#' @title Recursive Neyman Algorithm for Optimal Sample Allocation in Stratified
#'   Sampling Schemes Under One-Sided Constraints
#'
#' @description `r lifecycle::badge("stable")`
#'
#' An internal function that implements the recursive Neyman optimal allocation
#' algorithms, `rna` and `lrna`, described in Wesołowski et al. (2021) and
#' Wójciak (2022) respectively. The `rna()` should not be used directly. Instead,
#' user function [dopt()] or [nopt()] should be used. \cr
#'
#' The `rna()` function computes:
#' \deqn{argmin D(x_1,...,x_H) = a^2_1/x_1 + ... + a^2_H/x_H - b,}
#' under the equality constraint imposed on total sample size:
#' \deqn{x_1 + ... + x_H = n,}
#' and optionally, under either
#' \deqn{x_w <= M_w,  w = 1,...,H, (Case I)}
#' or
#' \deqn{x_w >= m_w, w = 1,...,H, (Case II)}
#' where \eqn{H} denotes total number of strata, \eqn{x_1, ..., x_H} are the
#' strata sample sizes, and \eqn{m_w > 0} and \eqn{M_w > 0, w = 1, ..., H} are
#' lower and upper bounds respectively, optionally imposed on samples sizes in
#' strata.
#'
#' User of `rna()` can choose whether the inequality constraints will
#' be added to the optimization problem or not. This is achieved with the proper
#' use of `bounds` and `upper` arguments of the function. In case of no
#' inequality constraints to be added, `bounds` must be specified as `NULL`
#' (default).
#' If any bounds should be imposed on sample strata sizes, user must specify
#' these with `bounds` argument.
#' For the Case I of the upper bounds, `upper` flag must be set to `TRUE`
#' (default) and then the `rna()` performs the `rna`.
#' For the Case II of lower bounds, `upper` flag must be set to `FALSE` and then
#' the `rna()` performs the `lrna` algorithm.
#' The `upper` flag is ignored when `bounds` is `NULL`.
#'
#' @note For simple random sampling without replacement design in each stratum,
#'   parameters of the variance function \eqn{D} are
#'   \eqn{b = N_1 * S_1^2 + ... + N_H * S_H^2}, and \eqn{a_w = N_w * S_w}, where
#'   \eqn{N_w, S_w, w = 1, ..., H}, are strata sizes and standard deviations of
#'   a study variable in strata respectively. \cr
#'
#'   The `rna` and `lrna` are kind of more general versions of a popular
#'   recursive Neyman allocation procedure that is described in Remark 12.7.1 in
#'   Sarndal et al. (1992). It is a procedure of optimal sample allocation in
#'   stratified sampling scheme with simple random sampling without replacement
#'   design in each stratum while not exceeding strata sizes.
#'
#'   Note that in case no inequality constraints are added, the allocation is
#'   given as a closed form expression, known as Neyman allocation:
#'   \deqn{x_w = a_w * n / (a_1 + ... + a_H), w = 1, ..., H.}
#'
#' @param n (`number`)\cr total sample size. A strictly positive scalar.
#' @param a (`numeric`)\cr parameters \eqn{a_1, ..., a_H} of variance function
#'   \eqn{D}. Strictly positive numbers.
#' @param bounds (`numeric` or `NULL`) \cr optional one-sided lower or upper
#'   bounds constraints imposed on strata sample sizes. If `bounds` is not
#'   `NULL`, it is required that `n <= sum(bounds)` in case of `upper = TRUE`,
#'   and `n >= sum(bounds)`, in case of `upper = FALSE`. Strictly positive
#'   numbers.
#' @param upper (`flag`) \cr should values of `bounds` be treated as one-sided
#'   upper bounds constraints (default)? Otherwise, they are treated as lower
#'   bounds.
#' @param assignments (`flag`) \cr should information about strata assignments
#'   (either to take-Neyman or take-bound) be added to the output?
#'
#' @return Numeric vector with optimal sample allocations in strata or list
#'   with optimal sample allocations in strata and strata assignments (if
#'   `assignments` is `TRUE`).
#'
#' @seealso [dopt()], [nopt()], [sga()], [sgaplus()], [coma()], [rnabox()].
#'
#' @references
#'   Wesołowski, J., Wieczorkowski, R., Wójciak, W. (2021),
#'   Optimality of the recursive Neyman allocation,
#'   *Journal of Survey Statistics and Methodology*,
#'   \doi{10.1093/jssam/smab018},
#'   \doi{10.48550/arXiv.2105.14486} \cr
#'
#'   Wójciak, W. (2022),
#'   Minimum sample size allocation in stratified sampling under constraints on
#'   variance and strata sample sizes,
#'   \doi{10.48550/arXiv.2204.04035} \cr
#'
#'   Sarndal, C.-E., Swensson, B., and Wretman, J. (1992),
#'   *Model Assisted Survey Sampling*, New York, NY: Springer.
#'
#' @export
#' @examples
#' a <- c(3000, 4000, 5000, 2000)
#' bounds <- c(100, 90, 70, 80)
#' rna(n = 190, a = a, bounds = bounds)
rna <- function(n, a, bounds = NULL, upper = TRUE, assignments = FALSE) {
  which_violates <- h_get_which_violates(geq = upper)

  x <- (n / sum(a)) * a # Neyman allocation.
  Ri <- which_violates(x, bounds)

  if (length(Ri) != 0L) {
    W <- seq_along(a) # Set of strata original indices. To be shrunk in repeat loop.
    repeat {
      # R := W[Ri] is a set of strata original indices for which x violates bounds.
      n <- n - sum(bounds[W[Ri]])
      W <- W[-Ri] # W <- W \ R
      # Neyman allocation for W. Denominator could be accumulated (i.e.
      # denom <- denom - sum(a[R]), where first denom = a1 + a2 + ... + aH),
      # but resigned from this version due to finite precision arithmetic issues.
      x <- (n / sum(a[W])) * a[W]
      Ri <- which_violates(x, bounds[W]) # Indices of W for which x violates bounds.
      if (length(Ri) == 0L) {
        bounds[W] <- x
        break
      }
    }
    if (assignments) {
      take_bound <- if (length(W) == 0L) {
        seq_along(a) # Vertex allocation.
      } else {
        seq_along(a)[-W]
      }
      list(opt = bounds, take_neyman = W, take_bound = take_bound)
    } else {
      bounds
    }
  } else if (assignments) {
    list(opt = x, take_neyman = seq_along(a), take_bound = numeric(0))
  } else {
    x
  }
}

#' @title Algorithms for Optimum Sample Allocation in Stratified Sampling Under
#'   Under One-Sided Upper Bounds Constraints
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Internal functions that implement the optimal sample allocation algorithms:
#' `sga`, `sgaplus` and `coma`.
#' Functions from this family compute:
#' \deqn{argmin D(x_1,...,x_H) = a^2_1/x_1 + ... + a^2_H/x_H - b,}
#' under the equality constraint imposed on total sample size:
#' \deqn{x_1 + ... + x_H = n,}
#' and upper bounds constraints imposed on strata sample sizes:
#' \deqn{x_w <= M_w, w = 1,...,H.}
#' Here, \eqn{H} denotes total number of strata, \eqn{x_1, ..., x_H} are the
#' strata sample sizes, and \eqn{n > 0}, \eqn{b}, \eqn{a_w > 0},
#' \eqn{M_w > 0, w = 1, ..., H} are given numbers. \cr
#' The `sga()`, `sgaplus()` and `coma()` are internal implementations of the
#' algorithms in subject, and hence, users should not use any of these functions
#' directly. Instead, the [dopt()] should be used.
#'
#' @note For simple random sampling without replacement design in each stratum,
#'   parameters of the variance function \eqn{D} are
#'   \eqn{b = N_1 * S_1^2 + ... + N_H * S_H^2}, and \eqn{a_w = N_w * S_w}, where
#'   \eqn{N_w, S_w, w = 1, ..., H}, are strata sizes and standard deviations of
#'   a study variable in strata respectively.
#'
#' @param n (`number`)\cr total sample size. A strictly positive scalar.
#' @param a (`numeric`)\cr parameters \eqn{a_1, ..., a_H} of variance function
#'   \eqn{D}. Strictly positive numbers.
#' @param M (`numeric`) \cr upper bounds constraints imposed on strata sample
#'   sizes. It is required that `n <= sum(M)`. Strictly positive numbers.
#'
#' @return Numeric vector with optimal sample allocations in strata.
#'
#' @name dopt_upper
#' @seealso [dopt()], [rna()].
#'
#' @references
#'   Wesołowski, J., Wieczorkowski, R., Wójciak, W. (2021),
#'   Optimality of the recursive Neyman allocation,
#'   *Journal of Survey Statistics and Methodology*,
#'   \doi{10.1093/jssam/smab018},
#'   \doi{10.48550/arXiv.2105.14486} \cr
#'
#'   Stenger, H., Gabler, S. (2005),
#'   Combining random sampling and census strategies -
#'   Justification of inclusion probabilities equal 1,
#'   *Metrika*, 61, 137-156 \cr
#'
#'   Wójciak, W. (2019), Optimal allocation in stratified sampling schemes,
#'   *MSc Thesis*, Warsaw University of Technology, Warsaw, Poland.
#'   <http://home.elka.pw.edu.pl/~wwojciak/msc_optimal_allocation.pdf> \cr
#'
#' @examples
#' a <- c(3000, 4000, 5000, 2000)
#' M <- c(100, 90, 70, 80)
#' sga(n = 190, a = a, M = M)
#' sgaplus(n = 190, a = a, M = M)
#' coma(n = 190, a = a, M = M)
NULL

#' @describeIn dopt_upper implementation of the Stenger-Gabler type algorithm
#'   `SGa`, described in Wesołowski et al. (2021) and in Stenger and Gabler
#'   (2005).
#' @export
#'
sga <- function(n, a, M) {
  H <- length(a) # Number of strata.
  aM <- a / M
  # W - strata indices sorted with regard to non-increasing order of a/M.
  # The ordering preserves relative order of the elements with equivalent values.
  W <- order(aM, decreasing = TRUE)

  A <- sum(a)
  for (i in 1:H) {
    s_inv <- A / n # Inverse of s function.
    h <- W[i]
    if (aM[h] < s_inv) {
      break
    } else {
      n <- n - M[h]
      A <- A - a[h]
    }
  }

  if (i == 1L) { # To improve the performance, otherwise, else block only.
    a / s_inv
  } else {
    V <- W[i:H]
    M[V] <- a[V] / s_inv
    M
  }
}

#' @describeIn dopt_upper implementation of the modified Stenger-Gabler type
#'   algorithm, described in Wójciak (2019) as
#'   `Sequential Allocation (version 1)` algorithm.
#' @export
sgaplus <- function(n, a, M) {
  H <- length(a) # Number of strata.
  aM <- a / M
  # W - strata indices sorted with regard to non-increasing order of a/M.
  # The ordering preserves relative order of the elements with equivalent values.
  W <- order(aM, decreasing = TRUE)

  A <- sum(a)
  ksi_inv <- A / n
  i <- 1L
  j <- 1L
  repeat {
    if (aM[W[i]] < ksi_inv) { # Allocation for stratum W[i] does not exceed M.
      if (j < i) { # It must not be exceeded also when W = W[-(1:(i-1))]
        Rj <- W[j:(i - 1L)]
        n <- n - sum(M[Rj])
        A <- A - sum(a[Rj])
        ksi_inv <- A / n
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
    a / ksi_inv
  } else {
    V <- W[i:H]
    M[V] <- a[V] / ksi_inv
    M
  }
}

#' @describeIn dopt_upper implementation of the Change of Monotonicity
#'   Algorithm, or `coma`, described in Wesołowski et al. (2021).
#' @export
coma <- function(n, a, M) {
  H <- length(a) # Number of strata.
  if (H == 1L) {
    return(min(n, M))
  }
  # W - strata indices sorted with regard to non-increasing order of a/M.
  # The ordering preserves relative order of the elements with equivalent values.
  W <- order(a / M, decreasing = TRUE)

  A <- sum(a)
  s <- n / A
  for (i in 1:(H - 1)) {
    h <- W[i]
    n <- n - M[h]
    A <- A - a[h]
    s_next <- n / A

    if (s > s_next) { # Change of monotonicity found.
      break
    } else {
      s <- s_next
    }
  }

  i <- ifelse(i == H - 1 && s <= s_next, H, i)

  if (i == 1) { # To improve the performance, otherwise, else block only.
    a * s
  } else {
    V <- W[i:H]
    M[V] <- a[V] * s
    M
  }
}

#' @title RNA - Recursive Implementation
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams rna
#'
#' @note this coded was not extensively tested.
#'
#' @export
#' @examples
#' a <- c(3000, 4000, 5000, 2000)
#' bounds <- c(100, 90, 70, 80)
#' rna_rec(n = 190, a = a, bounds = bounds)
#' rna_rec(n = 312, a = a, bounds = bounds)
#' rna_rec(n = 339, a = a, bounds = bounds)
#' rna_rec(n = 340, a = a, bounds = bounds)
#'
rna_rec <- function(n, a, bounds = NULL, upper = TRUE) {
  which_violates <- h_get_which_violates(geq = upper)

  x <- (n / sum(a)) * a # Neyman allocation.
  R <- which_violates(x, bounds)

  if (length(R) != 0L) {
    n <- n - sum(bounds[R])
    bounds[-R] <- rna_rec(n, a[-R], bounds[-R], upper = upper)
    bounds
  } else {
    x
  }
}

#' @title rna version that computes values of set function s
#'
#' @description `r lifecycle::badge("experimental")`
#' @inheritParams rna
#'
#' @export
#' @examples
#' a <- c(3000, 4000, 5000, 2000)
#' bounds <- c(100, 90, 70, 80)
#' rna_s(n = 190, a = a, bounds = bounds)
#'
rna_s <- function(n, a, bounds = NULL, upper = TRUE, assignments = FALSE) {
  which_violates <- h_get_which_violates(geq = upper)

  s <- s0 <- n / sum(a)
  x <- a * s # Neyman allocation.
  Ri <- which_violates(x, bounds) # Indices in W for which x exceeds bounds.

  if (length(Ri) != 0L) {
    W <- seq_along(a) # Set of strata original indices. To be reduced in repeat loop.
    repeat {
      n <- n - sum(bounds[W[Ri]])
      W <- W[-Ri]
      s <- n / sum(a[W])
      x <- s * a[W] # Neyman allocation for (reduced) W.
      Ri <- which_violates(x, bounds[W]) # Indices of W for which x violates bounds.
      if (length(Ri) == 0L) {
        bounds[W] <- x
        break
      }
    }
    if (assignments) {
      take_bound <- if (length(W) == 0L) {
        seq_along(a) # Vertex allocation.
      } else {
        seq_along(a)[-W]
      }
      list(opt = bounds, take_neyman = W, take_bound = take_bound, s0 = s0, s = s)
    } else {
      bounds
    }
  } else if (assignments) {
    list(opt = x, take_neyman = seq_along(a), take_bound = numeric(0), s0 = s0, s = s)
  } else {
    x
  }
}

#' @title rna version that uses prior information about violations
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This is the version of the RNA that makes use of additional information
#' about strata for which the allocation can possibly be violated. For all other
#' strata allocation will not be violated.
#'
#' @inheritParams rna
#' @param check (`integer`)\cr strata indices for which the allocation can possible
#'   be violated. For other strata allocation cannot be violated.
#'
#' @export
#'
rna_prior <- function(n, a, bounds, upper = TRUE, check = NULL, assignments = FALSE) {
  which_violates <- h_get_which_violates(geq = upper)

  x <- n * (a / sum(a)) # Neyman allocation.
  Ri <- which_violates(x[check], bounds[check]) # Indices of `check` for which x violates bounds.

  if (length(Ri) != 0L) {
    W <- seq_along(a)
    repeat {
      R <- W[check[Ri]] # Set of strata native labels for which x violates bounds.
      W <- W[-Ri] # W = W \ R.
      n <- n - sum(bounds[R])
      x <- n * (a[W] / sum(a[W])) # Neyman allocation for W.
      Ri <- which_violates(x, bounds[W]) # Indices of W for which x violates bounds.
      if (length(Ri) == 0L) {
        bounds[W] <- x
        x <- bounds
        break
      }
    }
    if (assignments) {
      if (length(W) == 0L) {
        B <- seq_along(a)
      } else {
        B <- seq_along(a)[-W]
      }
      x <- list(opt = x, B = B, Bc = W)
    }
  } else if (assignments) {
    x <- list(opt = x, B = numeric(0), Bc = seq_along(a))
  }
  x
}
