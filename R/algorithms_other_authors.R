#' Integer-valued Optimal Univariate Allocation Under Constraints for Stratified
#' Sampling
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Simple algorithm from paper Friedrich et al. (2015) for integer-valued
#' optimal allocation in stratified sampling.
#'
#' @param n -  target sample size for allocation.
#' @param Ah - population strata sizes * standard deviations of a given variable in strata.
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
SimpleGreedy <- function(n, Ah,
                         mh = rep(1, length(Ah)),
                         Mh = rep(Inf, length(Ah)),
                         nh = mh) {
  if (any(nh > Mh) || any(mh > Mh)) {
    stop("There are no feasible solutions")
  }

  r <- 0L
  while (sum(nh) < n) {
    r <- r + 1
    Vh <- Ah / sqrt(nh * (nh + 1)) * (nh + 1 <= Mh)
    h <- which.max(Vh)
    nh[h] <- nh[h] + 1
  }

  nh
}

#' @describeIn SimpleGreedy
#'
#' @param Nh - population strata sizes.
#' @param Sh - standard deviations of a given variable in strata.
#' @param v0 - upper limit for value of variance which must be attained for
#'   computed optimal allocation.
#'
SimpleGreedy2 <- function(v0, Nh, Sh,
                          mh = rep(1, length(Nh)),
                          Mh = Nh,
                          nh = mh) {
  if (any(nh > Mh) || any(mh > Mh)) {
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
  nh
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
CapacityScaling <- function(n, Ah,
                            mh = rep(1, length(Ah)),
                            Mh = rep(Inf, length(Ah))) {
  if (any(mh > Mh)) {
    stop("There are no feasible solutions")
  } else {
    nh <- mh
  }

  H <- length(Ah)
  s <- ceiling(n / (2 * H))

  while (s > 1) {
    r <- 0L
    while (sum(nh) < n) {
      r <- r + 1
      Vh <- Ah / sqrt(nh * (nh + 1)) * (nh + 1 <= Mh)
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

  SimpleGreedy(n, Ah, mh, Mh, nh)
}

#' @describeIn CapacityScaling
#'
#' @inheritParams SimpleGreedy2
#'
CapacityScaling2 <- function(v0, Nh, Sh,
                             mh = rep(1, length(Nh)),
                             Mh = rep(Inf, length(Nh))) {
  if (any(mh > Mh)) {
    stop("There are no feasible solutions")
  } else {
    nh <- mh
  }

  Ah <- Nh * Sh
  n <- sum(Ah)^2 / (v0 + sum(Ah * Sh))
  n <- round(n)

  s <- ceiling(n / (2 * length(Ah)))
  # cat("n s", n, s, "\n")

  while (!is.na(s) && s > 1) {
    r <- 0L
    v <- sum(Ah * Sh * (Nh - nh) / nh)

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
      v <- sum(Ah * Sh * (Nh - nh) / nh)
    }

    nh <- pmax(nh - s, mh)
    s <- ceiling(s / 2)
  }

  SimpleGreedy2(v0, Nh, Sh, mh, Mh, nh)
}


#' Optimal Univariate Allocation Under Constraints for Stratified
#' Sampling
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Algorithm for optimal allocation in stratified sampling with lower and upper
#' constraints based on fixed point iteration.
#'
#' @inheritParams SimpleGreedy
#' @param lambda0 - initial parameter 'lambda' (optional).
#' @param maxiter - maximal number of iterations for algorithm.
#' @param tol - the desired accuracy (convergence tolerance).
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
fpia <- function(n, Ah, mh = NULL, Mh = NULL, lambda0 = NULL, maxiter = 100, tol = .Machine$double.eps * 1000) {
  H <- seq_along(Ah)
  Ah2mh2 <- (Ah / mh)^2
  Ah2Mh2 <- (Ah / Mh)^2

  lambda <- if (is.null(lambda0)) {
    (sum(Ah) / n)^2 # according to article MSW

    # # initial interval for searching 'lambda' - according to J.Wesolowski
    # r <- c(Ah / mh, Ah / Mh)
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
    L <- which(Ah2mh2 <= lambda)
    U <- which(Ah2Mh2 >= lambda)
    Hc <- H[-c(L, U)]

    lambda_n <- (sum(Ah[Hc]) / (n - sum(mh[L]) - sum(Mh[U])))^2
    if (iter > maxiter || abs(lambda_n - lambda) < tol || is.nan(lambda_n)) {
      break
    }
    lambda <- lambda_n
    # cat("iteracja ",iter," lambda ",lambda,"\n")
  }

  nh <- Ah / sqrt(lambda)
  nh[L] <- mh[L]
  nh[U] <- Mh[U]

  list(nh = nh, iter = iter)
}

#' @describeIn fpia
#'
#' @inheritParams fpia
#'
fpia2 <- function(v0, Nh, Sh, mh = NULL, Mh = NULL, lambda0 = NULL, maxiter = 100) {
  Sh[Sh == 0] <- 1e-8
  # if (is.null(lambda0)) lambda0 <- 1e-6
  Ah <- Sh * Nh
  nh <- fpia(v0 + sum(Ah * Sh), Ah, Ah^2 / Mh, Ah^2 / mh, lambda0 = lambda0, maxiter = maxiter)$nh
  (Ah^2) / nh
}

# auxiliary functions

glambda <- function(lambda, n, Ah, mh = NULL, Mh = NULL) {
  L <- which((Ah / mh)^2 <= lambda)
  U <- which((Ah / Mh)^2 >= lambda)

  nh <- Ah / sqrt(lambda)
  nh[L] <- mh[L]
  nh[U] <- Mh[U]

  sum(nh) - n
}

philambda <- function(lambda, n, Ah, mh = NULL, Mh = NULL) {
  H <- seq_along(Ah)

  L <- which((Ah / mh)^2 <= lambda)
  U <- which((Ah / Mh)^2 >= lambda)
  Hc <- H[-c(L, U)]

  # cat("L: ",L," U: ",U,"\n")
  (sum(Ah[Hc]) / (n - sum(mh[L]) - sum(Mh[U])))^2 # lambda_n
}
