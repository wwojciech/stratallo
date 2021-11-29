#################
# dane
#################

# zbior data_mM jest w katalogu ./data
# load(file = "./data/data_mM.Rdata")

d <- data_mM[, "N"] * data_mM[, "S"]
m <- data_mM[, "m"]
M <- data_mM[, "M"]
n_feasible <- (sum(m)+1):(sum(M)-1) # wektor feasible n (z wylaczeniem dwoch trywialnych przypadkow)
# 1073; 3347

#################
# poprawnosc
#################

# n - skalar
n <- sample(n_feasible, size = 1)
noptcond_sufficient(d = d, l = m, u = M, n = n)
rNa_mM(d = d, m = m, M = M, n = n)

# w funcji n, ktore przebiega przez wszystkie elementy w wektorze n_feasible
all(
  sapply(n_feasible, function(n_i)
    all.equal(
      noptcond_sufficient(d = d, l = m, u = M, n = n_i),
      rNa_mM(d = d, m = m, M = M, n = n_i)
    )
  )
)

#################
# czasy obliczen
#################

# n - skalar
n <- sample(n_feasible, size = 1)
bench::mark(
  noptcond_sufficient(d = d, l = m, u = M, n = n),
  rNa_mM(d = d, m = m, M = M, n = n),
  check = TRUE,
  iterations = 10
)[1:6]

