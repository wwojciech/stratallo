#' @title Algorithms for Optimum Sample Allocation in Stratified Sampling Under
#'   Upper Bounds Constraints
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   Internal functions that implement the optimal sample allocation algorithms:
#'   `sga`, `sgaplus` and `coma`.
#'   Functions from this family compute
#'   \deqn{argmin D(x_1,...,x_H) = a^2_1/x_1 + ... + a^2_H/x_H - b,}
#'   under the equality constraint imposed on total sample size
#'   \deqn{x_1 + ... + x_H = n,}
#'   and upper bounds constraints imposed on strata sample sizes
#'   \deqn{x_w <= M_w, w = 1,...,H.}
#'   Here, \eqn{H} denotes total number of strata, \eqn{x_1, ..., x_H} are the
#'   strata sample sizes, and \eqn{n > 0}, \eqn{b}, \eqn{a_w > 0},
#'   \eqn{M_w > 0, w = 1, ..., H} are given numbers. \cr
#'   The `sga()`, `sgaplus()` and `coma()` are internal implementations of the
#'   algorithms in subject, and hence, users should not use any of these
#'   functions directly. Instead, the [dopt()] should be used.
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
#' @seealso [dopt()], [rna_one_sided()].
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
