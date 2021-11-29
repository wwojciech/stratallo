# zbior data_mM jest w katalogu ./data
# load(file = "./data/data_mM.Rdata")

d <- data_mM[, "N"] * data_mM[, "S"]
m <- data_mM[, "m"]
M <- data_mM[, "M"]
c(sum(m), sum(M)) # (minimalne, maksymalne) feasible n
# 1073; 3347

###############
# poprawnosc
###############

# np. dla n = 2000
n <- 2000
noptcond_sufficient(d = d, l = m, u = M, n = n)
rNa_mM(d = d, m = m, M = M, n = n)


# w funcji n, ktore przebiega od 1074 do 3346
n <- (sum(m)+1):(sum(M)-1)
all(
  sapply(n, function(n_i)
    all.equal(
      noptcond_sufficient(d = d, l = m, u = M, n = n_i),
      rNa_mM(d = d, m = m, M = M, n = n_i)
    )
  )
)

###############
# czasy obliczen
###############
bench::mark(
  noptcond_sufficient(d = d, l = m, u = M, n = n),
  rNa_mM(d = d, m = m, M = M, n = n),
  check = TRUE,
  iterations = 10
)[1:6]

