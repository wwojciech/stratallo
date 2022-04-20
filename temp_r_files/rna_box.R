# install.packages("stratallo")
# library(stratallo)
h_get_which_violated <- stratallo:::h_get_which_violated

# Wersja podstawowa.
rna_box_v0 <- function(n, a, m, M) {
  W <- seq_along(a)

  repeat {
    x <- rna_one_sided(n, a[W], M[W]) # step 1
    L <- which(x <= m[W]) # step 2
    if (length(L) == 0L) { # step 3
      break
    } else {
      n <- n - sum(m[W[L]])
      W <- W[-L]
    }
  }

  # To improve the performance, otherwise, else block only.
  if (length(W) == length(m)) {
    x
  } else {
    m[W] <- x
    m
  }
}

# Taka sama logika jak rna_box_v0, sprytniejsze zakodowanie
# (pierwszy pierwsza iteracja nie jest subsetowana przez W - tak jest szybciej)
rna_box <- function(n, a, m, M) {
  x <- rna_one_sided(n, a, M) # step 1
  L <- which(x <= m) # step 2
  if (length(L) == 0L) { # step 3
    x
  } else {
    W <- seq_along(a)
    repeat {
      n <- n - sum(m[W[L]])
      W <- W[-L]
      x <- rna_one_sided(n, a[W], M[W]) # step 1
      L <- which(x <= m[W]) # step 2
      if (length(L) == 0L) { # step 3
        m[W] <- x
        break
      }
    }
    m
  }
}

# Wersja, ktora nie sprawdza w kroku 2: x_w >= m_w dla w: x_w = M_w.
rna_box_perf_v0 <- function(n, a, m, M) {
  W <- seq_along(a)

  repeat {
    upper <- rna_one_sided_v2(n, a[W], M[W]) # step 1
    Uc_rel_W <- upper$Bc
    Uc <- W[Uc_rel_W] # take-Neyman

    L <- which(upper$nopt[Uc_rel_W] <= m[Uc]) # step 2
    if (length(L) == 0L) { # step 3
      break
    } else {
      n <- n - sum(m[Uc[L]])
      W <- W[-Uc_rel_W[L]]
    }
  }

  # To improve the performance, otherwise, else block only.
  if (length(W) == length(m)) {
    upper$nopt
  } else {
    m[W] <- upper$nopt
    m
  }
}

# Taka sama logika jak rna_box_perf_v0, sprytniejsze zakodowanie
# (pierwszy pierwsza iteracja nie jest subsetowana przez W - tak jest szybciej)
rna_box_perf <- function(n, a, m, M) {
  opt_upper <- rna_one_sided_v2(n, a, M, extended = TRUE) # step 1
  Uc <- opt_upper$Bc # take-Neyman
  L <- which(opt_upper$opt[Uc] <= m[Uc]) # step 2
  if (length(L) == 0L) { # step 3
    opt_upper$opt
  } else {
    W <- seq_along(a)
    Uc_rel_W <- Uc
    repeat {
      n <- n - sum(m[Uc[L]])
      W <- W[-Uc_rel_W[L]]
      opt_upper <- rna_one_sided_v2(n, a[W], M[W], extended = TRUE) # step 1
      Uc_rel_W <- opt_upper$Bc
      Uc <- W[Uc_rel_W]
      L <- which(opt_upper$opt[Uc_rel_W] <= m[Uc]) # step 2
      if (length(L) == 0L) { # step 3
        m[W] <- opt_upper$opt
        break
      }
    }
    m
  }
}

################ rna_one_sided #####################

rna_one_sided_v2 <- function(n, a, bounds, upper = TRUE, extended = FALSE) {
  which_violated <- h_get_which_violated(geq = upper)

  A <- sum(a)
  x <- (n / A) * a # Neyman allocation.
  Ri <- which_violated(x, bounds) # Indices of W for which x violates bounds.

  if (length(Ri) != 0L) {
    W <- seq_along(a)
    repeat {
      R <- W[Ri] # Set of strata native labels for which x violates bounds.
      W <- W[-Ri] # W = W \ R.
      n <- n - sum(bounds[R])
      A <- A - sum(a[R])
      x <- (n / A) * a[W] # Neyman allocation for W.
      Ri <- which_violated(x, bounds[W]) # Indices of W for which x violates bounds.
      if (length(Ri) == 0L) {
        bounds[W] <- x
        x <- bounds
        break
      }
    }
    if (extended) {
      if (length(W) == 0L) {
        B <- seq_along(a)
      } else {
        B <- seq_along(a)[-W]
      }
      x <- list(opt = x, B = B, Bc = W)
    }
  } else if (extended) {
    x <- list(opt = x, B = numeric(0), Bc = seq_along(a))
  }
  x
}

################################################################################
# TESTOWANIE: przeplatanka rNa, rNaL - niestety daje zle alokacje, szczegoly
# ponizej.

rna_one_sided_test <- function(n, a, bounds, upper = TRUE) {
  which_violated <- h_get_which_violated(geq = upper)

  W <- seq_along(a) # Strata native labels. Can be shrunk in the repeat loop.
  A <- sum(a)
  x <- (n / A) * a # Neyman allocation.
  Ri <- which_violated(x, bounds) # Indices of W for which x violates bounds.

  if (length(Ri) == 0L) {
    B <- numeric(0)
  } else {
    W_all <- W
    repeat {
      R <- W[Ri] # Set of strata native labels for which x violates bounds.
      W <- W[-Ri] # W = W \ R.
      n <- n - sum(bounds[R])
      A <- A - sum(a[R])
      x <- (n / A) * a[W] # Neyman allocation for W.
      Ri <- which_violated(x, bounds[W]) # Indices of W for which x violates bounds.
      if (length(Ri) == 0L) {
        bounds[W] <- x
        x <- bounds
        if (length(W) == 0) {
          B <- W_all
        } else {
          B <- W_all[-W]
        }
        break
      }
    }
  }

  return(list(B = B, Bc = W, nopt = x, n = n))
}

# Nie dziala, np dla
# mh <- c(10, 10, 10, 10)
# Mh <- c(30, 30, 30, 30)
# Sh <- c(10.4, 1.8, 0.3, 0.9)
# Nh <- c(100, 100, 100, 100)
# ah <- Nh * Sh
# n <- 80
# x_przep <- rna_box_przeplatanka(n, ah, mh, Mh)
# x_ok <- rna_box_perf_v2(n, ah, mh, Mh)
# Nie dziala pomimo, ze w zadnej iteracji nie otrzymujemy upper$n < sum(m[Uc])
rna_box_przeplatanka <- function(n, a, m, M) {
  H <- length(a)
  W <- 1:H # step 0

  repeat {
    # print("l")
    upper <- rna_one_sided_test(n, a[W], M[W]) # step 1
    Uc_rel_W <- upper$Bc
    Uc <- W[Uc_rel_W] # take-Neyman

    if (length(Uc) == 0) {
      break
    }

    # UWAGA: moze sie zdarzyc, ze upper$n < sum(m[Uc]), np. dla
    # rna_box_przeplatanka(1100, pop10_mM[, 1] * pop10_mM[, 2], pop10_mM[, 3], pop10_mM[, 4])
    lower <- rna_one_sided_test(upper$n, a[Uc], m[Uc], upper = FALSE) # step 2
    L <- lower$B
    if (length(L) == 0L) { # step 3
      break
    } else {
      n <- n - sum(m[Uc[L]])
      W <- W[-Uc_rel_W[L]]
    }
  }

  if (length(W) == H) {
    upper$nopt
  } else {
    m[W] <- upper$nopt
    m
  }
}
