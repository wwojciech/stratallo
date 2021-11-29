# zbior data_mM jest w katalogu ./data
# load(file = "./data/data_mM.Rdata")

d <- data_mM[, "N"] * data_mM[, "S"]
m <- data_mM[, "m"]
M <- data_mM[, "M"]
sum(m) # minimalne feasible n
sum(M) # maksymalne feasible n
# 1073+1 - 3347-1

# np. dla n = 2000
n <- 2000
noptcond_sufficient(d = d, l = m, u = M, n = n)
rNa_mM(d = d, m = m, M = M, n = n)

bench::mark(
  noptcond_sufficient(d = d, l = m, u = M, n = n),
  rNa_mM(d = d, m = m, M = M, n = n),
  check = TRUE,
  iterations = 10
)[1:6]

# w funcji n od 1074 do 3346
all(
  sapply(1074:3346, function(n)
    all.equal(
      noptcond_sufficient(d = d, l = m, u = M, n = n),
      rNa_mM(d = d, m = m, M = M, n = n)
    )
  )
)


