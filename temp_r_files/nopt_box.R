#install.packages("stratallo")
#library(stratallo)

rNa_mM <- function(d, m, M, n, verbose = FALSE) {

  W <- 1:length(d) # step 0

  repeat {
    nopt_candidate <- rNa(d[W], M[W], n) # step 1
    L <- which(nopt_candidate <= m[W]) # step 2
    if (length(L) == 0) # step 3
      break
    else {
      n <- n - sum(m[W[L]])
      W <- W[-L]
    }
  }

  if (length(W) == length(m)) # if else construct used here only to improve the performance, otherwise it could be just in the body of the else block
    nopt <- nopt_candidate
  else {
    nopt <- m
    nopt[W] <- nopt_candidate
  }

  return(nopt)

}

# rekursyjny Neyman dla tylko ograniczen dolnych m
rNa_L <- function(d, m, n, verbose = FALSE) {

  J <- 1:(length(d))
  W <- J
  D <- sum(d)
  nopt_ <- (n / D) * d
  i <- which(nopt_ <= m)
  repeat {

    if (length(i) == 0)
      break
    else {
      h <- J[i] # h - strata ids (unique within rNa) for which nopt_ <= m
      J <- J[-i] # remove strata h from set J
      n <- n - sum(m[h])
      D <- D - sum(d[h])
      nopt_ <- (n/D) * d[J] # Neyman allocation nopt = s(W\J) * d[J]
      i <- which(nopt_ <= m[J]) # i - indices of strata ids in dynamic set J, for which nopt_[J] <= M[J]
    }

  }

  if (length(J) == length(m)) # if else construct used here only to improve the performance, otherwise it could be just the body of the else block
    nopt <- nopt_ # nopt_ > m  for all strata
  else {
    nopt <- m
    nopt[J] <- nopt_
  }

  if(length(J) == 0)
    L <- W
  else
    L <- W[-J]

  return(list(L = L, nopt = nopt))

}

rNa_U <- function(d, M, n, verbose = FALSE) {

  J <- 1:(length(d))
  W <- J
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

  if(length(J) == 0)
    U <- W
  else
    U <- W[-J]

  return(list(U = U, n = n, nopt = nopt))

}

rNa2_mM <- function(d, m, M, n, verbose = FALSE) {

  K <- length(d)
  W <- 1:K # step 0

  repeat {
    #print("l")
    Upper <- rNa_U(d[W], M[W], n) # step 1
    Uc <- W[-Upper$U]

    if(length(Uc) == 0)
      break

    Lower <- rNa_L(d[Uc], m[Uc], Upper$n) # step 2

    if (length(Lower$L) == 0) # step 3
      break
    else {
      n <- n - sum(m[Uc[Lower$L]])
      W <- setdiff(W, Uc[Lower$L])
    }
  }

  if (length(W) == K) # if else construct used here only to improve the performance, otherwise it could be just in the body of the else block
    nopt <- Upper$nopt
  else {
    nopt <- m
    nopt[W] <- Upper$nopt
  }

  return(nopt)

}
