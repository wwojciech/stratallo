#################
# dane
#################

# Zbior pop10_mM jest w pakiecie stratallo.

a <- pop10_mM[, "N"] * pop10_mM[, "S"]
m <- pop10_mM[, "m"]
M <- pop10_mM[, "M"]
n_feasible <- sum(m):sum(M) # wektor feasible n

#################
# poprawnosc
#################

## 1)

# n - skalar
n <- sample(n_feasible, size = 1)
all.equal(
  noptcond_sufficient(d = a, l = m, u = M, n = n),
  rna_box_perf(n, a, m, M)
)

# w funcji n, ktore przebiega przez wszystkie elementy w wektorze n_feasible
nopt_ <- sapply(
  n_feasible,
  function(n_i) {
    all.equal(
      noptcond_sufficient(d = a, l = m, u = M, n = n_i),
      rna_box_perf(n_i, a, m, M)
    )
  }
)
all(nopt_)

## 2)

m <- sample(x = 10:10^3, size = 10^3, replace = TRUE)
M <- m + sample(x = 10:10^2, size = length(m), replace = TRUE)
a <- sample(x = 10:10^2, size = length(m), replace = TRUE)
n_feasible <- sum(m):sum(M) # wektor feasible n

nopt_ <- sapply(
  n_feasible,
  function(n_i) {
    all.equal(
      noptcond_sufficient(d = a, l = m, u = M, n = n_i),
      rna_box_perf(n_i, a, m, M)
    )
  }
)
all(nopt_)

#################
# czasy obliczen
#################

# n - skalar
n <- sample(n_feasible, size = 1)
bench::mark(
  noptcond_sufficient(d = a, l = m, u = M, n = n),
  rna_box_perf(n, a, m, M),
  check = TRUE,
  iterations = 50
)[1:6]
