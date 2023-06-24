#' Integer-valued Optimal Univariate Allocation Under Constraints for Stratified
#' Sampling
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Simple algorithm from paper Friedrich et al. (2015) for integer-valued
#' optimal allocation in stratified sampling.
#'
#' @param n -  target sample size for allocation.
#' @param Nh - population sizes in strata.
#' @param Sh - standard deviations for given variable in strata.
#' @param mh - lower constraints for sample sizes in strata.
#' @param Mh - upper constraints for sample sizes in strata.
#' @param nh - initial allocation (if not given then nh=mh).
#'
#' @return A vector of optimal allocation sizes.
#'
#' @references
#'   Friedrich, U., Münnich, R., de Vries, S. and Wagner, M. (2015)
#'   Fast integer-valued algorithms for optimal allocations under constraints in
#'   stratified sampling,
#'   *Computational Statistics and Data Analysis*, 92, pp. 1–12.
#'   \url{https://www.sciencedirect.com/science/article/pii/S0167947315001413}
#'
SimpleGreedy <- function(n, Nh, Sh,
                         mh = rep(1, length(Nh)),
                         Mh = rep(Inf, length(Nh)),
                         nh = NULL) {
  if (is.null(nh)) nh <- mh

  if (any(nh > Mh)) {
    stop("There are no feasible solutions")
  }

  r <- 0L
  while (sum(nh) < n) {
    r <- r + 1
    Vh <- Nh * Sh / sqrt(nh * (nh + 1)) * (nh + 1 <= Mh)
    h <- which.max(Vh)
    nh[h] <- nh[h] + 1
  }

  return(nh)

  # v <- sum(Nh * (Nh - nh) * Sh^2 / nh)
  # return(list(nh = nh, v = v))
}

#' @describeIn SimpleGreedy
#'
#' @param v0 - upper limit for value of variance which must be attained for
#'   computed optimal allocation.
#'
SimpleGreedy2 <- function(v0, Nh, Sh,
                          mh = rep(1, length(Nh)),
                          Mh = Nh,
                          nh = NULL) {
  if (is.null(nh)) nh <- mh

  if (any(nh > Mh)) {
    stop("There are no feasible solutions")
  }

  r <- 0L
  v <- sum(Nh * (Nh - nh) * Sh^2 / nh)

  while (v > v0 && sum(nh) < sum(Nh)) {
    r <- r + 1
    Vh <- Nh * Sh / sqrt(nh * (nh + 1)) * (nh + 1 <= Mh)
    h <- which.max(Vh)
    nh[h] <- nh[h] + 1
    v <- sum(Nh * (Nh - nh) * Sh^2 / nh)
  }

  return(nh)

  # v <- sum(Nh * (Nh - nh) * Sh^2 / nh)
  # return(list(nh = nh, v = v))
}

#' Integer-valued Optimal Univariate Allocation Under Constraints for Stratified
#' Sampling
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Better algorithm from paper Friedrich et al. (2015) for integer-valued
#' optimal allocation in stratified sampling.
#'
#' @inheritParams SimpleGreedy
#' @inherit SimpleGreedy return references
#'
CapacityScaling <- function(n, Nh, Sh,
                            mh = rep(1, length(Nh)),
                            Mh = rep(Inf, length(Nh))) {
  nh <- mh

  if (any(nh > Mh)) {
    stop("There are no feasible solutions")
  }

  H <- length(Nh)
  s <- ceiling(n / (2 * H))

  while (s > 1) {
    r <- 0L
    while (sum(nh) < n) {
      r <- r + 1
      Vh <- Nh * Sh / sqrt(nh * (nh + 1)) * (nh + 1 <= Mh)
      h <- which.max(Vh)
      if (nh[h] + 1 <= Mh[h]) {
        if (nh[h] + s <= Mh[h]) {
          nh[h] <- nh[h] + s
        } else {
          nh[h] <- nh[h] + 1
        }
      }
    }

    nh <- pmax(nh - s, mh)
    s <- ceiling(s / 2)
  }

  SimpleGreedy(n, Nh, Sh, mh, Mh, nh)
  # v <- sum(Nh * (Nh - nh) * Sh^2 / nh)
  # return(list(nh = nh, v = v))
}

#' @describeIn CapacityScaling
#'
#' @param v0 - upper limit for value of variance which must be attained for
#'   computed optimal allocation.
#'
CapacityScaling2 <- function(v0, Nh, Sh,
                             mh = rep(1, length(Nh)),
                             Mh = rep(Inf, length(Nh))) {
  nh <- mh

  if (any(nh > Mh)) {
    stop("There are no feasible solutions")
  }

  dh <- Nh * Sh
  ah <- dh / sum(dh)
  n <- sum((Sh / ah) * Sh * Nh * Nh) / (v0 + sum(Nh * Sh * Sh))
  n <- round(n)

  H <- length(Nh)
  s <- ceiling(n / (2 * H))
  # cat("n s",n,s,"\n")

  while (!is.na(s) && s > 1) {
    r <- 0L
    v <- sum(Nh * (Nh - nh) * Sh^2 / nh)

    while (v > v0 && sum(nh) < sum(Nh)) {
      r <- r + 1
      Vh <- Nh * Sh / sqrt(nh * (nh + 1)) * (nh + 1 <= Mh)
      h <- which.max(Vh)
      if (nh[h] + 1 <= Mh[h]) {
        if (nh[h] + s <= Mh[h]) {
          nh[h] <- nh[h] + s
        } else {
          nh[h] <- nh[h] + 1
        }
      }
      v <- sum(Nh * (Nh - nh) * Sh^2 / nh)
    }

    nh <- pmax(nh - s, mh)
    s <- ceiling(s / 2)
  }

  SimpleGreedy2(v0, Nh, Sh, mh, Mh, nh)$nh
  # v <- sum(Nh * (Nh - nh) * Sh^2 / nh)
  # return(list(nh = nh, v = v))
}


#' Integer-valued Optimal Univariate Allocation Under Constraints for Stratified
#' Sampling
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Algorithm for optimal allocation in stratified sampling with lower and upper
#' constraints based on fixed point iteration.
#'
#' @param n target sample size for allocation.
#' @param Nh population sizes in strata.
#' @param Sh standard deviations for given variable in strata.
#' @param mh lower constraints for sample sizes in strata.
#' @param Mh upper constraints for sample sizes in strata.
#' @param lambda0 initial parameter 'lambda' (optional).
#' @param maxiter maximal number of iterations for algorithm.
#' @param tol the desired accuracy (convergence tolerance).
#'
#' @return A vector of optimal allocation sizes, and number of iterations.
#'
#' @references
#'   Münnich, R. T., Sachs, E.W. and Wagner, M. (2012)
#'   Numerical solution of optimal allocation problems in stratified sampling
#'   under box constraints,
#'   *AStA Advances in Statistical Analysis*, 96(3), pp. 435-450.
#'   \doi{10.1007/s10182-011-0176-z}
#'
fpia <- function(n, Nh, Sh, mh = NULL, Mh = NULL, lambda0 = NULL, maxiter = 100, tol = .Machine$double.eps * 1000) {
  H <- seq_along(Nh)
  dh <- Sh * Nh
  dh2mh2 <- (dh / mh)^2
  dh2Mh2 <- (dh / Mh)^2

  lambda <- if (is.null(lambda0)) {
    (sum(dh) / n)^2 # according to article MSW

    # # initial interval for searching 'lambda' - according to J.Wesolowski
    # r <- c(dh / mh, dh / Mh)
    # a <- min(r)^2
    # b <- max(r)^2
    # lambda <- uniroot(
    #   function(x) glambda(x, n, Nh, Sh, mh, Mh), lower = a, upper = b, maxiter = maxiter
    # )$root
    # lambda <- (a + b) / 2 # the simplest starting value for bisection
  } else {
    lambda0
  }

  iter <- 0
  while (1) {
    iter <- iter + 1
    L <- which(dh2mh2 <= lambda)
    U <- which(dh2Mh2 >= lambda)
    Hc <- H[-c(L, U)]

    lambda_n <- (sum(dh[Hc]) / (n - sum(mh[L]) - sum(Mh[U])))^2
    if (iter > maxiter || abs(lambda_n - lambda) < tol || is.nan(lambda_n)) {
      break
    }
    lambda <- lambda_n
    # cat("iteracja ",iter," lambda ",lambda,"\n")
  }

  nh <- dh / sqrt(lambda)
  nh[L] <- mh[L]
  nh[U] <- Mh[U]

  # v <- sum(Nh * (Nh - nh) * Sh^2 / nh)
  list(nh = nh, iter = iter)
}

# auxiliary functions

glambda <- function(lambda, n, Nh, Sh, mh = NULL, Mh = NULL) {
  H <- seq_along(Nh)
  dh <- Sh * Nh

  L <- which((dh / mh)^2 <= lambda)
  U <- which((dh / Mh)^2 >= lambda)

  nh <- dh / sqrt(lambda)
  nh[L] <- mh[L]
  nh[U] <- Mh[U]

  sum(nh) - n
}

philambda <- function(lambda, n, Nh, Sh, mh = NULL, Mh = NULL) {
  H <- seq_along(Nh)
  dh <- Sh * Nh

  L <- which((dh / mh)^2 <= lambda)
  U <- which((dh / Mh)^2 >= lambda)
  Hc <- H[-c(L, U)]

  # cat("L: ",L," U: ",U,"\n")
  (sum(dh[Hc]) / (n - sum(mh[L]) - sum(Mh[U])))^2 # lambda_n
}
