#' @title Optimal Sample Allocation in Stratified Random Sampling Scheme
#'
#' @description
#' A classical problem in survey methodology in stratified sampling schemes is an optimum sample allocation problem.
#' The problem is formulated as the determination of the vector (called sample allocation),
#' that minimizes, under given constraints, the variance of the estimator of the population total of a study variable.
#' For simple random sampling without replacement design in each stratum, this variance takes the following form \cr
#' \out{<center>}\code{D(n_1,...,n_H) = \\sum_{h = 1}^{H} \\frac{N_h^2 S_h^2}{n_h} - \\sum_{h = 1}^{H} N_h S_h^2},\out{</center>} \cr
#' where \code{H} denotes total number of strata, \code{N_1,...,N_H} denote strata sizes, and
#' \code{S_1,...,S_H} denote standard deviations of a study variable in strata. \cr
#'
#' The \code{nopt} computes the \code{argmin D(n_1,...,n_H)}, under the following constraints
#' \out{<p style="margin-left: 40px">}\code{1: \\sum_{h=1}^{H} n_h = n} \cr
#' \code{2: n_h \\le M_h \\le N_h, h = 1,...,H}, \out{</p>}
#' where \code{n} denotes overall sample size, and \code{M_1,...,M_H} denote upper bounds optionally imposed on sample strata sizes. \cr
#'
#' There are four different underlying algorithms available to use, abbreviated as: \emph{"sa0"}, \emph{"sa1"}, \emph{"coma"}, \emph{"rNa"}.
#' The algorithms are described in detail in Wojciak (2019). \cr
#'
#'
#' @param n integer, total sample size. It must be \code{n > sum(M)}, otherwise \code{n} is truncated such that \code{n = sum(M)}, and therefore
#' allocation returned is equal to \code{M}. In case of truncation, the warning message is thrown.
#' @param N a numeric vector, strata sizes.
#' @param S a numeric vector, standard deviations of a study variable in strata.
#' @param M a numeric vector, upper bounds constraints on sample sizes in strata. It must be that \code{0 < M <= N}, where \code{N - strata sizes}.
#' @param method a character string indicating the algorithm to be used.
#' One of \emph{"sa0"} (default), \emph{"sa1"}, \emph{"coma"}, \emph{"rNa"}, can be abbreviated.
#' @param verbose logical, if \emph{TRUE}, more interim variables from chosen algorithm are returned.
#' @param variance logical, if \emph{TRUE}, the value of variance \code{D} corresponding to the optimal solution is returned.
#'
#' @return Numeric vector with optimal sample allocations in strata, or
#' list with numeric vector of optimal sample allocations in strata (1st element of the list), and corresponding interim variables and/or variance (further elements of the list).
#'
#' @references Wojciak W. (2019) \emph{Optimal allocation in stratified sampling schemes}.
#'   Master's diploma thesis, Warsaw University of Technology. \url{http://home.elka.pw.edu.pl/~wwojciak/msc_optimal_allocation.pdf} \cr
#'   Sarndal, C.-E., Swensson, B., and Wretman, J. (1992) \emph{Model Assisted Survey Sampling}. Chapter 3.7 \emph{Stratified Sampling}. Springer.
#'
#' @export
#' @examples
#' nopt(n = 190, N = c(3000, 4000, 5000, 2000), S = rep(1, 4), M = c(100, 90, 70, 80))
#' # 40.71 54.29 67.86 27.14
#' nopt(n = 270, N = c(3000, 4000, 5000, 2000), S = rep(1, 4), M = c(100, 90, 70, 80))
#' # 66.66 88.88 70.00 44.44
#' nopt(n = 300, N = c(3000, 4000, 5000, 2000), S = rep(1, 4), M = c(100, 90, 70, 80))
#' # 84 90 70 56
#' nopt(n = 330, N = c(3000, 4000, 5000, 2000), S = rep(1, 4), M = c(100, 90, 70, 80))
#' # 100  90  70  70
#' nopt(n = 340, N = c(3000, 4000, 5000, 2000), S = rep(1, 4), M = c(100, 90, 70, 80))
#' # 100  90  70  80
#'
#' \donttest{# Example of sample size n truncation, as it exceeds the sum of upper bounds, i.e. n > sum(M).
#' nopt(n = 350, N = c(3000, 4000, 5000, 2000), S = rep(1, 4), M = c(100, 90, 70, 80))
#' # 100  90  70  80}
#'
#' N <- pop969[, "N"]
#' S <- pop969[, "S"]
#' n <- floor(0.01 * sum(N))
#' nopt(n, N, S)
#' nopt(n, N, S, variance = TRUE)
#'
#' # Example of execution-time comparision of different algorithms using bench R package.
#' # nfrac <- seq(0.01, 0.9, 0.05)
#' # n <- setNames(nfrac * sum(N), nfrac)
#' # lapply(n, function(ni) bench::mark(nopt(ni, N, S, method = "sa0"),
#' #                                    nopt(ni, N, S, method = "sa1"),
#' #                                    nopt(ni, N, S, method = "coma"),
#' #                                    nopt(ni, N, S, method = "rNa"),
#' #                                    iterations = 200)[c(1,3)])
#'
nopt <- function(n, N, S, M = N, method = "sa0", verbose = FALSE, variance = FALSE) {

  if (n >= sum(M)) {
    if (n > sum(M))
      warning("Sample size n was truncated as it exceeds the sum of upper bounds.")
    return(M)
  }

  if(length(N) == 1) # no strata
    return(n)

  nopt <- do.call(method, list(d = N * S, M = M, n = n, verbose = verbose))

  if (variance) {
    if(is.list(nopt))
      nopt <- c(nopt, list(var = Dst(n = nopt$nopt, N = N, S = S)))
    else
      nopt <- list(nopt = nopt, var = Dst(n = nopt, N = N, S = S))
  }

  return(nopt)

}

#' @title Sequential Allocation (ver. 0) Algorithm for Optimal Sample Allocation in Stratified Sampling Scheme
#'
#' @description
#' Implementation of the Sequential Allocation (ver. 0) algorithm for optimal sample allocation problem in
#' stratified sampling scheme with simple random sampling without replacement design in each stratum.
#' The algorithm is described in Wojciak (2019).
#'
#' @param d a numeric vector, equal to (element-wise) multiplication of strata sizes by standard deviations of a study variable in strata.
#' @param M a numeric vector, upper bounds constraints on sample sizes in strata. It must be that \code{0 < M <= N}, where \code{N - strata sizes}.
#' @param n integer, total sample size.
#' @param verbose logical, if \emph{TRUE}, more interim variables from chosen algorithm are returned; in particular: \code{J} - set of strata indices sorted
#' in non-increasing order of \code{\\frac{d}{M}}, \code{J_} - set of strata indices for which the allocated sample size is strictly less than its upper bound constraint \code{M}.
#'
#' @return Numeric vector with optimal sample allocations in strata, or
#'  list with numeric vector of optimal sample allocations in strata (1st element of the list), and corresponding interim variables (further elements of the list).
#'
#' @references Wojciak W. (2019) \emph{Optimal allocation in stratified sampling schemes}.
#'   Master's diploma thesis, Warsaw University of Technology. \url{http://home.elka.pw.edu.pl/~wwojciak/msc_optimal_allocation.pdf}
#'
#' @export
#' @examples
#' N <- c(3000, 4000, 5000, 2000)
#' S <- rep(1, 4)
#' sa0(d = N * S, M = c(100, 90, 70, 80), n = 190)
#'
#'
sa0 <- function(d, M, n, verbose = FALSE) {

  dM <- d / M
  J <- order(dM, decreasing = TRUE)
  # J - set of srata indices sorted by d/M in non-increasing order
  # it preserves the relative order of elements with equivalent values

  D <- sum(d)
  ksi_1 <- D / n # ksi^{-1}
  for (k in 1:H) {
    h <- J[k]
    if (dM[h] < ksi_1)
      break
    else {
      n <- n - M[h]
      D <- D - d[h]
      ksi_1 <- D / n # ksi^{-1}
    }
  }

  ksi <- 1 / ksi_1
  if(k == 1) # if else construct used here only to improve the performance, otherwise it could be just the body of the else block
    nopt <-  ksi * d  # ksi * d < M for all strata
  else {
    H <- length(d)
    nopt <- M
    nopt[J[k:H]] <- ksi * d[J[k:H]]
  }

  if(verbose) # more detailed output
    nopt <- list(nopt = nopt, ksi = ksi, J = J, J_ = J[k:length(d)])

  return(nopt)

}

#' @title Sequential Allocation (ver. 1) Algorithm for Optimal Sample Allocation in Stratified Sampling Scheme
#'
#' @description
#' Implementation of the Sequential Allocation (ver. 1) algorithm for optimal sample allocation problem in
#' stratified sampling scheme with simple random sampling without replacement design in each stratum.
#' The algorithm is described in Wojciak (2019).
#'
#' @param d a numeric vector, equal to (element-wise) multiplication of strata sizes by standard deviations of a study variable in strata.
#' @param M a numeric vector, upper bounds constraints on sample sizes in strata. It must be that \code{0 < M <= N}, where \code{N - strata sizes}.
#' @param n integer, total sample size.
#' @param verbose logical, if \emph{TRUE}, more interim variables from chosen algorithm are returned; in particular: \code{J} - set of strata indices sorted
#' in non-increasing order of \code{\\frac{d}{M}}, \code{J_} - set of strata indices for which the allocated sample size is strictly less than its upper bound constraint \code{M}.
#'
#' @return Numeric vector with optimal sample allocations in strata, or
#'  list with numeric vector of optimal sample allocations in strata (1st element of the list), and corresponding interim variables (further elements of the list).
#'
#' @references Wojciak W. (2019) \emph{Optimal allocation in stratified sampling schemes}.
#'   Master's diploma thesis, Warsaw University of Technology. \url{http://home.elka.pw.edu.pl/~wwojciak/msc_optimal_allocation.pdf}
#'
#' @export
#' @examples
#' N <- c(3000, 4000, 5000, 2000)
#' S <- rep(1, 4)
#' sa1(d = N * S, M = c(100, 90, 70, 80), n = 190)
#'
#'
sa1 <- function(d, M, n, verbose = FALSE) {

  dM <- d / M
  J <- order(dM, decreasing = TRUE)
  # J - set of srata indices sorted by d/M in non-increasing order
  # it preserves the relative order of elements with equivalent values

  D <- sum(d)
  ksi_1 <- D / n # ksi^{-1}
  k <- 1
  g <- 1
  repeat {
    if (dM[J[k]] >= ksi_1) {
      k <- k + 1
    } else if (g < k) {
      h <- J[g:(k - 1)]
      n <- n - sum(M[h])
      D <- D - sum(d[h])
      ksi_1 <- D / n # ksi^{-1}
      g <- k
    } else
      break
  }

  ksi <- 1 / ksi_1
  if(k == 1) # if else construct used here only to improve the performance, otherwise it could be just the body of the else block
    nopt <-  ksi * d  # ksi * d < M for all strata
  else {
    nopt <- M
    h <- J[k:length(d)]
    nopt[h] <- ksi * d[h]
  }

  if(verbose) # more detailed output
    nopt <- list(nopt = nopt, ksi = ksi, J = J, J_ = J[k:length(d)])

  return(nopt)

}

#' @title Change of Monotonicity Algorithm for Optimal Sample Allocation in Stratified Sampling Scheme
#'
#' @description
#' Implementation of the Change of Monotonicity algorithm for optimal sample allocation problem in
#' stratified sampling scheme with simple random sampling without replacement design in each stratum.
#' The algorithm is described in Wojciak (2019), under the name Sequential Allocation (ver. 2).
#'
#' @param d a numeric vector, equal to (element-wise) multiplication of strata sizes by standard deviations of a study variable in strata.
#' @param M a numeric vector, upper bounds constraints on sample sizes in strata. It must be that \code{0 < M <= N}, where \code{N - strata sizes}.
#' @param n integer, total sample size.
#' @param verbose logical, if \emph{TRUE}, more interim variables from chosen algorithm are returned; in particular: \code{J} - set of strata indices sorted
#' in non-increasing order of \code{\\frac{d}{M}}, \code{J_} - set of strata indices for which the allocated sample size is strictly less than its upper bound constraint \code{M}.
#'
#' @return Numeric vector with optimal sample allocations in strata, or
#'  list with numeric vector of optimal sample allocations in strata (1st element of the list), and corresponding interim variables (further elements of the list).
#'
#' @references Wojciak W. (2019) \emph{Optimal allocation in stratified sampling schemes}.
#'   Master's diploma thesis, Warsaw University of Technology. \url{http://home.elka.pw.edu.pl/~wwojciak/msc_optimal_allocation.pdf}
#'
#' @export
#' @examples
#' N <- c(3000, 4000, 5000, 2000)
#' S <- rep(1, 4)
#' coma(d = N * S, M = c(100, 90, 70, 80), n = 190)
#'
coma <- function(d, M, n, verbose = FALSE) {

  H <- length(d)
  J <- order(d / M, decreasing = TRUE)
  # J - set of srata indices sorted by d/M in non-increasing order
  # it preserves the relative order of elements with equivalent values

  D <- sum(d)
  ksi <- n / D
  for (k in 1:(H - 1)) {
    h <- J[k]
    n <- n - M[h]
    D <- D - d[h]
    ksi1 <- n / D

    if (ksi > ksi1)
      break # change of monotonicity found
    else
      ksi <- ksi1
  }

  k <- ifelse(k == H - 1 && ksi <= ksi1, H, k)

  if(k == 1) # if else construct used here only to improve the performance, otherwise it could be just the body of the else block
    nopt <-  ksi * d  # ksi * d < M for all strata
  else {
    nopt <- M
    nopt[J[k:H]] <- ksi * d[J[k:H]]
  }

  if(verbose) # more detailed output
    nopt <- list(nopt = nopt, ksi = ksi, ksi1 = ksi1, J = J, J_ = J[k:H])

  return(nopt)

}

#' @title Recursive Neyman Optimal Allocation Algorithm for Optimal Sample Allocation in Stratified Sampling Scheme
#'
#' @description
#' Implementation of the Recursive Neyman optimal allocation algorithm, with enhancement allowing \code{M <= N} (without enhancement \code{M = N}), where \eqn{N} denotes vector of strata sizes.
#' The Recursion Neyman optimal allocation algorithm is described in Remark 12.7.1 in Sarndal et al. (1992), and it computes the optimal sample allocation in stratified sampling scheme with simple random sampling without replacement design in each stratum.
#' Details of the enhancement, including the proof are given in Wojciak (2019).
#'
#' @param d a numeric vector, equal to (element-wise) multiplication of strata sizes by standard deviations of a study variable in strata.
#' @param M a numeric vector, upper bounds constraints on sample sizes in strata. It must be that \code{0 < M <= N}, where \code{N - strata sizes}.
#' @param n integer, total sample size.
#' @param verbose logical, if \emph{TRUE}, more interim variables from chosen algorithm are returned. Not used in current version.
#'
#' @return Numeric vector with optimal sample allocations in strata.
#'
#' @references Wojciak W. (2019) \emph{Optimal allocation in stratified sampling schemes}.
#'   Master's diploma thesis, Warsaw University of Technology. \url{http://home.elka.pw.edu.pl/~wwojciak/msc_optimal_allocation.pdf} \cr
#'   Sarndal, C.-E., Swensson, B., and Wretman, J. (1992) \emph{Model Assisted Survey Sampling}. Springer.
#'
#' @export
#' @examples
#' N <- c(3000, 4000, 5000, 2000)
#' S <- rep(1, 4)
#' rNa(d = N * S, M = c(100, 90, 70, 80), n = 190)
#'
rNa <- function(d, M, n, verbose = FALSE) {

  J <- 1:(length(d))
  D <- sum(d)
  nopt_ <- (n / D) * d
  i <- which(nopt_ >= M)
  repeat {

    if (length(i) == 0)
      break
    else {
      h <- J[i] # h - strata ids (unique within rNa) for which nopt_ >= M
      J <- J[-i] # remove strata h from set J
      n <- n - sum(M[h])
      D <- D - sum(d[h])
      nopt_ <- (n/D) * d[J] # Neyman allocation
      i <- which(nopt_ >= M[J]) # i - indices of strata ids in dynamic set J, for which nopt_[J] >= M[J]
    }

  }

  if (length(J) == length(M)) # if else construct used here only to improve the performance, otherwise it could be just the body of the else block
    nopt <- nopt_ # nopt_ < M for all strata
  else {
    nopt <- M
    nopt[J] <- nopt_
  }

  return(nopt)

}

#' @title Variance of Stratified Pi-estimator of the Total
#'
#' @description
#' Compute the variance of the stratified pi-estimator of the population total of the study variable
#' under simple random sampling without replacement design in each stratum. This variance takes the following form \cr
#' \out{<center>}\code{D(n_1,...,n_H) = \\sum_{h = 1}^{H} \\frac{N_h^2 S_h^2}{n_h} - \\sum_{h = 1}^{H} N_h S_h^2},\out{</center>} \cr
#' where \code{H} denotes total number of strata, \code{N_1,...,N_H} denote strata sizes, and
#' \code{S_1,...,S_H} denote standard deviations of a study variable in strata. \cr
#'
#' @param n a numeric vector, sample allocations in strata.
#' @param N a numeric vector, strata sizes.
#' @param S a numeric vector, standard deviations of a study variable in strata.
#'
#' @return The value of the variance.
#'
#' @references Sarndal, C.-E., Swensson, B., and Wretman, J. (1992) \emph{Model Assisted Survey Sampling}. Chapter 3.7 \emph{Stratified Sampling}. Springer.
#'
#' @export
#' @examples
#' N <- c(3000, 4000, 5000, 2000)
#' S <- rep(1, 4)
#' n_ <- nopt(n = 190, N = N, S = S, M = c(100, 90, 70, 80))
#' Dst(n = n_, N, S)
#'
#'
Dst <- function(n, N, S) {

  D <- sum(N * (N - n) * S^2 / n)
  return(D)

}

#' @title Example Population with 507 Strata
#'
#' @description Artificial, example population with 507 strata,
#' defined by strata sizes and standard deviations of a variable under study in strata.
#'
#' @format A matrix with 507 rows and 2 columns
#' \describe{
#'   \item{N}{stratum sizes}
#'   \item{S}{standard deviations of a variable under study in strata}
#' }
"pop507"

#' @title Example Population with 969 Strata
#'
#' @description Artificial, example population with 969 strata,
#' defined by strata sizes and standard deviations of a variable under study in strata.
#'
#' @format A matrix with 969 rows and 2 columns
#' \describe{
#'   \item{N}{stratum sizes}
#'   \item{S}{standard deviations of a variable under study in strata}
#' }
"pop969"
