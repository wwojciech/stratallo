noptcond_sufficient(
  d = data_mM[, "N"] * data_mM[, "N"],
  l = data_mM[, "m"],
  u = data_mM[, "M"],
  n = 2000
)

rNa_mM(
  d = data_mM[, "N"] * data_mM[, "N"],
  m = data_mM[, "m"],
  M = data_mM[, "M"],
  n = 2000
)

sum(data_mM[, "m"]) # n min
sum(data_mM[, "M"]) # n max
# 1073+1 - 33471-1


all(sapply(1074:33470, function(n) {
  ggm <- noptcond_sufficient(
    d = data_mM[, "N"] * data_mM[, "N"],
    l = data_mM[, "m"],
    u = data_mM[, "M"],
    n = 2000
  )
  rna <- rNa_mM(
    d = data_mM[, "N"] * data_mM[, "N"],
    m = data_mM[, "m"],
    M = data_mM[, "M"],
    n = 2000
  )
  all.equal(ggm, rna)
}))


bench::mark(
  noptcond_sufficient(
    d = data_mM[, "N"] * data_mM[, "N"],
    l = data_mM[, "m"],
    u = data_mM[, "M"],
    n = 2000
  ),
  rNa_mM(
    d = data_mM[, "N"] * data_mM[, "N"],
    m = data_mM[, "m"],
    M = data_mM[, "M"],
    n = 2000
  ),
  check = TRUE,
  iterations = 10
)[1:6]
