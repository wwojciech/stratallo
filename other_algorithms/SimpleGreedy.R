#' @title Integer-valued optimal univariate allocation under constraints for stratified sampling
#' @description Simple algorithm from paper Friedrich et al. (2015) for integer-valued optimal
#'   allocation in stratified sampling
#'
#' @param n -  target sample size for allocation
#' @param Nh - population sizes in strata
#' @param Sh - standard deviations for given variable in strata
#' @param mh - lower constraints for sample sizes in strata
#' @param Mh - upper constraints for sample sizes in strata
#' @param nh - initial allocation (if not given then nh=mh)
#'
#' @return  vector of optimal allocation sizes
#'
#' @references Ulf Friedrich, Ralf Munnich, Sven de Vries, Matthias Wagner,
#' Fast integer-valued algorithms for optimal allocations under constraints in stratified sampling,
#' Computational Statistics and Data Analysis 92 (2015), 1-12
#'
#' @export
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


#' @title Integer-valued optimal univariate allocation under constraints for stratified sampling
#' @description Simple algorithm from paper Friedrich et al. (2015) for integer-valued optimal
#'   allocation in stratified sampling
#'
#' @param v0 - upper limit for value of variance which must be attained for computed optimal allocation
#' @param Nh - population sizes in strata
#' @param Sh - standard deviations for given variable in strata
#' @param mh - lower constraints for sample sizes in strata
#' @param Mh - upper constraints for sample sizes in strata
#' @param nh - initial allocation (if not given then nh=mh)
#'
#' @return  vector of optimal allocation sizes and value of variance for obtained allocation
#'
#' @references Ulf Friedrich, Ralf Münnich, Sven de Vries, Matthias Wagner,
#' Fast integer-valued algorithms for optimal allocations under constraints in stratified sampling,
#' Computational Statistics and Data Analysis 92 (2015), 1-12
#'
#' @export
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
