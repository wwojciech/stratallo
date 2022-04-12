a <- c(3000, 4000, 5000, 2000)
bounds <- c(100, 90, 70, 80)

# Only lower bounds.
dopt(n = 340, a = a, m = bounds)
dopt(n = 400, a = a, m = bounds)
dopt(n = 600, a = a, m = bounds)

# Only upper bounds.
dopt(n = 190, a = a, M = bounds)
dopt(n = 300, a = a, M = bounds)
dopt(n = 340, a = a, M = bounds)

# Example of execution-time comparison of different algorithms
# using bench R package.
\dontrun{
N <- pop969[, "N"]
S <- pop969[, "S"]
a <- N * S
nfrac <- seq(0.01, 0.9, 0.05)
n <- setNames(as.integer(nfrac * sum(N)), nfrac)
lapply(
  n,
  function(ni) {
    bench::mark(
      dopt(ni, a, M = N, M_method = "rna"),
      dopt(ni, a, M = N, M_method = "sga"),
      dopt(ni, a, M = N, M_method = "sgaplus"),
      dopt(ni, a, M = N, M_method = "coma"),
      iterations = 200
    )[c(1, 3)]
  }
)
}
