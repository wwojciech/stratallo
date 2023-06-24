a <- c(3000, 4000, 5000, 2000)
a0 <- 70000
bounds <- c(100, 90, 70, 80)
m <- c(100, 90, 70, 50)
M <- bounds
ucosts <- c(0.7, 15, 2, 3) # unit costs

N_pop507 <- pop507[, "N"]
a_pop507 <- N_pop507 * pop507[, "S"]
ucosts_pop507 <- pop507[, "unit_cost"]
