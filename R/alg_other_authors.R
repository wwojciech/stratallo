# Fast integer-valued algorithms by Friedrich (2015) ----

#' @name simplegreedy-capacityscaling
#'
#' @title SimpleGreedy and CapacityScaling Algorithms
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Fast integer-valued algorithms for optimum allocations under constraints
#' in stratified sampling proposed in \insertCite{Friedrich;textual}{stratallo}.
#'
#' @param n (`integerish(1)`)\cr total sample size.
#' @param v0 (`numeric(1)`)\cr upper bound on the variance of the estimator.
#' @param Nh (`numeric`)\cr population sizes in strata.
#' @param Sh (`numeric`)\cr standard deviations of the study variable in strata.
#' @param Ah (`numeric`)\cr products of population stratum sizes and standard
#'   deviations of the study variable, \eqn{A_h = N_h S_h}.
#' @param mh (`integerish`)\cr lower bounds on stratum sample sizes.
#' @param Mh (`integerish`)\cr upper bounds on stratum sample sizes.
#' @param nh (`integerish`)\cr initial allocation. Defaults to `mh`.
#'
#' @return
#' For the `fpia()` - an integer vector of optimum sample sizes allocated to
#' each stratum.
#'
#' @references
#' \insertRef{Friedrich}{stratallo}
#'
NULL

#' @describeIn simplegreedy-capacityscaling
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

#' @describeIn simplegreedy-capacityscaling
#'
#' Variant of the \emph{SimpleGreedy} algorithm based on a variance stopping
#' rule.
#'
SimpleGreedy2 <- function(v0,
                          Nh,
                          Sh,
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

#' @describeIn simplegreedy-capacityscaling
#'
CapacityScaling <- function(n,
                            Ah,
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

#' @describeIn simplegreedy-capacityscaling
#'
#' Variant of the \emph{CapacityScaling} algorithm based on a variance stopping
#' rule.
#'
CapacityScaling2 <- function(v0,
                             Nh,
                             Sh,
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

# FPIA ----

#' @name fpia
#'
#' @title Fixed-Point Iteration Algorithm
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Algorithm for optimum sample allocation in stratified sampling under lower-
#' and upper-bound constraints, based on fixed-point iteration.
#'
#' @inheritParams SimpleGreedy
#' @param mh (`numeric` or `NULL`)\cr lower bounds on stratum sample sizes (optional).
#' @param Mh (`numeric` or `NULL`)\cr upper bounds on stratum sample sizes (optional).
#' @param lambda0 (`numeric(1)`)\cr initial value of the parameter \eqn{\lambda} (optional).
#' @param lambda (`numeric(1)`)\cr \eqn{\lambda}.
#' @param maxiter (`integerish(1)`)\cr maximum number of iterations.
#' @param tol (`numeric(1)`)\cr desired convergence tolerance.
#'
#' @return
#' A list with elements:
#' \describe{
#'   \item{nh}{Vector of optimal allocation sizes.}
#'   \item{iter}{Number of iterations performed.}
#' }
#'
#' @references
#' \insertRef{MSW}{stratallo}
#'
NULL

#' @describeIn fpia
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
#' Variant of `fpia()` using variance-based parametrization.
#' @param v0 variance
#'
fpia2 <- function(v0, Nh, Sh, mh = NULL, Mh = NULL, lambda0 = NULL, maxiter = 100) {
  Sh[Sh == 0] <- 1e-8
  # if (is.null(lambda0)) lambda0 <- 1e-6
  Ah <- Sh * Nh
  nh <- fpia(v0 + sum(Ah * Sh), Ah, Ah^2 / Mh, Ah^2 / mh, lambda0 = lambda0, maxiter = maxiter)$nh
  (Ah^2) / nh
}

## auxiliary functions ----

#' @describeIn fpia
#'
#' Helper function for the `fpia()`
#'
glambda <- function(lambda, n, Ah, mh = NULL, Mh = NULL) {
  L <- which((Ah / mh)^2 <= lambda)
  U <- which((Ah / Mh)^2 >= lambda)

  nh <- Ah / sqrt(lambda)
  nh[L] <- mh[L]
  nh[U] <- Mh[U]

  sum(nh) - n
}

#' @describeIn fpia
#'
#' Helper function for the `fpia()`.
#'
philambda <- function(lambda, n, Ah, mh = NULL, Mh = NULL) {
  H <- seq_along(Ah)

  L <- which((Ah / mh)^2 <= lambda)
  U <- which((Ah / Mh)^2 >= lambda)
  Hc <- H[-c(L, U)]

  # cat("L: ",L," U: ",U,"\n")
  (sum(Ah[Hc]) / (n - sum(mh[L]) - sum(Mh[U])))^2 # lambda_n
}
