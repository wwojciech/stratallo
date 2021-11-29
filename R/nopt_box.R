install.packages("stratallo")
library(stratallo)

rNa_mM <- function(d, m, M, n, verbose = FALSE) {

  W <- 1:(length(d)) # step 0

  repeat {
    nopt_candidate <- rNa(d[W], M[W], n) # step 1
    i <- which(nopt_candidate <= m[W]) # step 2
    if (length(i) == 0)
      break
    else {
      n <- n - sum(m[W[i]])
      W <- W[-i]
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
