a <- c(3000, 4000, 5000, 2000)
m <- c(100, 90, 70, 50)
M <- c(300, 400, 200, 90)

# One-sided lower bounds.
opt(n = 340, a = a, m = m)
opt(n = 400, a = a, m = m)
opt(n = 700, a = a, m = m)

# One-sided upper bounds.
opt(n = 190, a = a, M = M)
opt(n = 700, a = a, M = M)

# Box-constraints.
opt(n = 340, a = a, m = m, M = M)
opt(n = 500, a = a, m = m, M = M)
xopt <- opt(n = 800, a = a, m = m, M = M)
xopt
var_st(x = xopt, a = a, a0 = 45000) # Value of the variance for allocation xopt.

# Execution-time comparisons of different algorithms with R package microbenchmark.
\dontrun{
N <- pop969[, "N"]
S <- pop969[, "S"]
a <- N * S
nfrac <- c(0.005, seq(0.05, 0.95, 0.05))
n <- setNames(as.integer(nfrac * sum(N)), nfrac)
lapply(
  n,
  function(ni) {
    microbenchmark::microbenchmark(
      RNA = opt(ni, a, M = N, M_algorithm = "rna"),
      SGA = opt(ni, a, M = N, M_algorithm = "sga"),
      SGAPLUS = opt(ni, a, M = N, M_algorithm = "sgaplus"),
      COMA = opt(ni, a, M = N, M_algorithm = "coma"),
      times = 200,
      unit = "us"
    )
  }
)
}
