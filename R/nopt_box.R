rNa_m <- function(d, m, n, verbose = FALSE) {

  J <- 1:(length(d))
  D <- sum(d)
  nopt_ <- (n / D) * d
  i <- which(nopt_ <= m)
  repeat {

    if (length(i) == 0)
      break
    else {
      h <- J[i] # h - strata ids (unique within rNa) for which nopt_ >= M
      J <- J[-i] # remove strata h from set J
      n <- n - sum(m[h])
      D <- D - sum(d[h])
      nopt_ <- (n/D) * d[J] # Neyman allocation
      i <- which(nopt_ <= m[J]) # i - indices of strata ids in dynamic set J, for which nopt_[J] >= M[J]
    }

  }

  if (length(J) == length(m)) # if else construct used here only to improve the performance, otherwise it could be just the body of the else block
    nopt <- nopt_ # nopt_ < M for all strata
  else {
    nopt <- m
    nopt[J] <- nopt_
  }

  return(nopt)

}
rNa_mM <- function(d, m, M, n, verbose = FALSE) {

  J <- 1:(length(d))

  repeat {
    nopt_candidate <- rNa_m(d[J], m[J], n)
    i <- which(nopt_candidate >= M[J])
    if (length(i) == 0)
      break
    else {
      n <- n - sum(M[J[i]])
      J <- J[-i] # remove strata h from set J
    }
  }

  if (length(J) == length(M)) # if else construct used here only to improve the performance, otherwise it could be just the body of the else block
    nopt <- nopt_candidate # nopt_ < M for all strata
  else {
    nopt <- M
    nopt[J] <- nopt_candidate
  }

  return(nopt)

}
