A <- c(3000, 4000, 5000, 2000)
m <- c(100, 90, 70, 50)
M <- c(300, 400, 200, 90)

# One-sided lower bounds.
opt(n = 340, A = A, m = m)
opt(n = 400, A = A, m = m)
opt(n = 700, A = A, m = m)

# One-sided upper bounds.
opt(n = 190, A = A, M = M)
opt(n = 700, A = A, M = M)

# Box-constraints.
opt(n = 340, A = A, m = m, M = M)
opt(n = 500, A = A, m = m, M = M)
xopt <- opt(n = 800, A = A, m = m, M = M)
xopt
var_st(x = xopt, A = A, A0 = 45000) # Value of the variance for allocation xopt.

# Execution-time comparisons of different algorithms with microbenchmark R package.
\dontrun{
N <- pop969[, "N"]
S <- pop969[, "S"]
A <- N * S
nfrac <- c(0.005, seq(0.05, 0.95, 0.05))
n <- setNames(as.integer(nfrac * sum(N)), nfrac)
lapply(
  n,
  function(ni) {
    microbenchmark::microbenchmark(
      RNA = opt(ni, A, M = N, M_algorithm = "rna"),
      SGA = opt(ni, A, M = N, M_algorithm = "sga"),
      SGAPLUS = opt(ni, A, M = N, M_algorithm = "sgaplus"),
      COMA = opt(ni, A, M = N, M_algorithm = "coma"),
      times = 200,
      unit = "us"
    )
  }
)
}
