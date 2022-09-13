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
  rnabox(n, a, m, M)
)

# w funcji n, ktore przebiega przez wszystkie elementy w wektorze n_feasible
nopt_ <- sapply(
  n_feasible,
  function(n_i) {
    all.equal(
      noptcond_sufficient(d = a, l = m, u = M, n = n_i),
      rnabox(n_i, a, m, M)
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
      rnabox(n_i, a, m, M)
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
  rnabox(n, a, m, M),
  check = TRUE,
  iterations = 50
)[1:6]

# Dr RW przyklad problemow z powodu finite arithmetic prec. na niekorzysc algorytmow rek.

Nh <- rep(1000,20)
Sh <- 10^(1:20)
Sh <- sample(Sh,20)
mh <- rep(100,20)
Mh <- Nh
dh <- Sh*Nh

N <- sum(Nh)
(n <- round(0.8*N))

alloc0 <- CapacityScaling(n, Nh, Sh, mh = mh, Mh = Mh)
nh_rnabox <- rnabox(n, dh, mh, Mh)
nh_noptcond <- noptcond_sufficient(dh, mh, Mh, n)

sum(nh_rnabox)

which(alloc0 != round(nh_rnabox))

if (max(abs(alloc0 - nh_rnabox)) > 1) {
  stop("Difference compared to integer allocation !")
}

# Ten problem pochodzi nawet z samego rNa dla ograniczeń tylko jednostronnych gornych,
# poniewaz akurat warstwy do L, są prawidlowo klasyfikowane przez rNaBox już w
# pierwszej iteracji. Ponizsze dane wchodza do rNa (jednostronnego) w drugiej
# iteracji algorytmu (po odcięciu warstw 11, 13, 16, 20 do L - czyli prawidlowo -
# poniewaz wlaśnie te warstwy wchodzą do take-min w CapacityScalling)

a <- c(
  1000000000000, 1000000000000000, 1000000000, 1000000000000000000000, 100000000,
  100000000000000000, 10000000000000000000, 100000000000000000000, 100000000000,
  10000000000000000000000, 10000000000000000, 10000000000000, 99999999999999991611392,
  10000000000, 1000000000000000000, 100000000000000
)

M <- c(
  1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000,
  1000, 1000, 1000
)

n <- 15600

stratallo:::rna_onesided(n, a, M)
stratallo:::sga(n, a, M)
stratallo:::sgaplus(n, a, M)
stratallo:::coma(n, a, M)


# A dokladnie problem jest z takim kodem. Oczywiscie wynik teoretyczny powinien byc == 1.
a[5] / (sum(a) - sum(a[-5]))
# [1] 0.9934107

# Zmienie te czesc kodu w rNa, gdzie liczymy wartości funkcji s(U), mianownik z obecnej:
# A <- A - sum(a[R])
# na nowa:
# A <- sum(a[W])


########################################
## Testy dla s()
########################################

n_min <- sum(m) + 1
n_max <- sum(M) - 1
n <- seq(from = n_min, to = n_max, by = 1)

all(sapply(n, function(ni) {
  opt <- rnabox_debug(ni, a, m, M)
  all(diff(sapply(opt$UL, `[[`, "s0")) < 0)
}))

all(sapply(n, function(ni) {
  opt <- rnabox_debug(ni, a, m, M)
  all(diff(na.omit(sapply(opt$UL, `[[`, "s"))) < 0)
}))


N <- stratallo::pop10_mM[, 1]
S <- stratallo::pop10_mM[, 2]
a <- N * S
m <- stratallo::pop10_mM[, 3]
M <- stratallo::pop10_mM[, 4]

n_min <- sum(m) + 1
n_max <- sum(M) - 1
n <- seq(from = n_min, to = n_max, by = 1)

all(sapply(n, function(ni) {
  opt <- rnabox_debug(ni, a, m, M)
  all(diff(sapply(opt$UL, `[[`, "s0")) < 0)
}))

all(sapply(n, function(ni) {
  opt <- rnabox_debug(ni, a, m, M)
  all(diff(na.omit(sapply(opt$UL, `[[`, "s"))) < 0)
}))
