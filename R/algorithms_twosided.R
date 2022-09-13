#' @include helpers.R
NULL

#' @title Recursive Neyman Algorithm for Optimal Sample Allocation Under
#'   Box-Constraints in Stratified Sampling Schemes
#'
#' @description `r lifecycle::badge("stable")`
#'
#' An internal function that implements recursive Neyman optimal allocation
#' algorithm, `rnabox`. The `rnabox()` should not be used
#' directly. Instead, user function [dopt()] should be used. \cr
#'
#' The `rnabox()` function computes:
#' \deqn{argmin D(x_1,...,x_H) = a^2_1/x_1 + ... + a^2_H/x_H - b,}
#' under the equality constraint imposed on total sample size:
#' \deqn{x_1 + ... + x_H = n,}
#' and (optionally) the following set of inequality constraints:
#' \deqn{m_w <= x_w <= M_w,  w = 1,...,H.}
#' Here, \eqn{H} denotes total number of strata, \eqn{x_1, ..., x_H} are the
#' strata sample sizes, and \eqn{m_w > 0, M_w > 0, w = 1, ..., H} are the lower
#' and upper bounds respectively, optionally imposed on sample sizes in
#' strata. \cr
#'
#' User of `rnabox()` can choose which or whether the inequality constraints
#' will be added to the optimization problem or not. In case of no inequality
#' constraints to be added, `m` or `M` (or both) must be specified as `NULL`
#' (default).
#' If any bounds should be imposed on sample strata sizes, user must specify
#' these with `m` and `M` arguments.
#'
#' @note For simple random sampling without replacement design in each stratum,
#'   parameters of the variance function \eqn{D} are
#'   \eqn{b = N_1 * S_1^2 + ... + N_H * S_H^2}, and \eqn{a_w = N_w * S_w}, where
#'   \eqn{N_w, S_w, w = 1, ..., H}, are strata sizes and standard deviations of
#'   a study variable in strata respectively. \cr
#'
#'   The `rnabox` is a generalization of a popular recursive
#'   Neyman allocation procedure that is described in Remark 12.7.1 in Sarndal
#'   et al. (1992). It is a procedure of optimal sample allocation in stratified
#'   sampling scheme with simple random sampling without replacement design in
#'   each stratum while not exceeding strata sizes.
#'
#'   Note that in case when no inequality constraints are added, the allocation
#'   is given as a closed form expression, known as Neyman allocation:
#'   \deqn{x_w = a_w * n / (a_1 + ... + a_H), w = 1, ..., H.}
#'
#' @param n (`number`)\cr total sample size. A strictly positive scalar.
#' @param a (`numeric`)\cr parameters \eqn{a_1, ..., a_H} of variance function
#'   \eqn{D}. Strictly positive numbers.
#' @param m (`numeric` or `NULL`) \cr optional lower bounds constraints imposed
#'   on strata sample sizes. If it is not `NULL`, it is required that
#'   `n >= sum(m)`. Strictly positive numbers.
#' @param M (`numeric` or `NULL`) \cr optional upper bounds constraints imposed
#'   on strata sample sizes. If it is not `NULL`, it is required that
#'   `n <= sum(M)`. Strictly positive numbers.
#'
#' @return Numeric vector with optimal sample allocations in strata.
#'
#' @seealso [dopt()], [nopt()], [sga()], [sgaplus()], [coma()].
#'
#' @references
#'   To be added soon.
#'
#' @export
#' @examples
#' # Artificial population and the bounds that to be imposed.
#' N <- c(454, 10, 116, 2500, 2240, 260, 39, 3000, 2500, 400)
#' S <- c(0.9, 5000, 32, 0.1, 3, 5, 300, 13, 20, 7)
#' a <- N * S
#' m <- c(322, 3, 57, 207, 715, 121, 9, 1246, 1095, 294)
#' M <- N
#'
#' # An example of a regular allocation.
#' n <- 6000
#' opt_regular <- rnabox(n, a, m, M)
#'
#' # An example of a vertex allocation.
#' n <- 4076
#' opt_vertex <- rnabox(n, a, m, M)
rnabox <- function(n, a, m = NULL, M = NULL) {
  opt1 <- rna_onesided(n, a, M, assignments = TRUE)
  TN <- opt1$take_neyman # Strata original indices for which allocation is take-Neyman.
  Lii <- which(opt1$opt[TN] <= m[TN]) # Indices of TN for which x violates lower bounds.

  if (length(Lii) == 0L) {
    opt1$opt
  } else {
    W <- seq_along(a) # Set of strata original indices. To be shrunk in repeat loop.
    TNi <- TN
    repeat {
      n <- n - sum(m[TN[Lii]])
      W <- W[-TNi[Lii]] # W = W \ R (R - original indices for which x violates lower bounds).
      opt1 <- rna_onesided(n, a[W], M[W], assignments = TRUE)
      TNi <- opt1$take_neyman # Indices of W for which the allocation is take-Neyman.
      TN <- W[TNi] # Strata original indices for which allocation is take-Neyman.
      Lii <- which(opt1$opt[TNi] <= m[TN])
      if (length(Lii) == 0L) {
        m[W] <- opt1$opt
        break
      }
    }
    m
  }
}

# TESTY ----

# install.packages("stratallo")
# library(stratallo)
# h_get_which_violates <- stratallo:::h_get_which_violates

## rnabox_debug_summary ----

rnabox_debug_summary <- function(a, m, M, assignments) {
  d <- lapply(assignments, function(i) {
    c(length(i$L_cum), length(i$U_rna), i$s0, i$s)
  })
  d <- data.frame(do.call(rbind, d))
  colnames(d) <- c("L_size", "U_size", "s(L, 0)", "s(L, U)")
  d
}

## rnabox ----

rnabox_debug <- function(n, a, m, M) {
  W <- seq_along(a)

  debug_data <- list()
  L <- NULL
  repeat {
    x <- rna_onesided_v2(n, a[W], M[W], extended = TRUE) # step 1
    Li <- which(x$opt <= m[W]) # step 2
    debug_data <- c(debug_data, list(list(L = L, U = W[x$B], Li = W[Li], s0 = x$s0, s = x$s)))
    L <- c(L, W[Li])
    if (length(Li) == 0L) { # step 3
      m[W] <- x$opt
      break
    } else {
      n <- n - sum(m[W[Li]])
      W <- W[-Li]
    }
  }
  names(debug_data) <- paste0("Iteration_", seq_along(debug_data))
  list(x = m, details = debug_data)
}

# Wersja podstawowa.
rnabox_v0 <- function(n, a, m, M) {
  W <- seq_along(a)

  repeat {
    x <- rna_onesided_v2(n, a[W], M[W]) # step 1
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

# Taka sama logika jak rnabox_v0, sprytniejsze zakodowanie
# (pierwsza pierwsza iteracja nie jest subsetowana przez W - tak jest szybciej)
rnabox_v01 <- function(n, a, m, M) {
  x <- rna_onesided_v2(n, a, M) # step 1
  L <- which(x <= m) # step 2
  if (length(L) == 0L) { # step 3
    x
  } else {
    W <- seq_along(a)
    repeat {
      n <- n - sum(m[W[L]])
      W <- W[-L]
      x <- rna_onesided_v2(n, a[W], M[W]) # step 1
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
rnabox_v1 <- function(n, a, m, M) {
  W <- seq_along(a)

  repeat {
    upper <- rna_onesided_v2(n, a[W], M[W]) # step 1
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

# Taka sama logika jak rnabox_v1, sprytniejsze zakodowanie
# (pierwsza iteracja nie jest subsetowana przez W - tak jest szybciej)
# Ta wersja jest aktualnie w pakiecie stratallo pod nazwa rnabox.
rnabox_v11 <- function(n, a, m, M) {
  opt_upper <- rna_onesided_v2(n, a, M, extended = TRUE) # step 1
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
      opt_upper <- rna_onesided_v2(n, a[W], M[W], extended = TRUE) # step 1
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

rnabox_v2 <- function(n, a, m, M) {
  opt_upper <- rna_onesided_v2(n, a, M, extended = TRUE) # step 1
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
      opt_upper <- rna_onesided_prior(n, a[W], M[W], check = "TODO", extended = TRUE) # step 1
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

## rna_onesided ----

rna_onesided_v2 <- function(n, a, bounds, upper = TRUE, extended = FALSE) {
  which_violates <- h_get_which_violates(geq = upper)

  s <- s0 <- n / sum(a)
  x <- a * s # Neyman allocation.
  Ri <- which_violates(x, bounds) # Indices in W for which x exceeds bounds.

  if (length(Ri) != 0L) {
    W <- seq_along(a)
    repeat {
      R <- W[Ri] # Original strata indices for which x exceeds bounds.
      W <- W[-Ri] # W = W \ R.
      n <- n - sum(bounds[R])
      s <- n / sum(a[W])
      x <- s * a[W] # Neyman allocation for W.
      Ri <- which_violates(x, bounds[W]) # Indices of W for which x exceeds bounds.
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
      x <- list(opt = x, B = B, Bc = W, s0 = s0, s = s)
    }
  } else if (extended) {
    x <- list(opt = x, B = numeric(0), Bc = seq_along(a), s0 = s0, s = s)
  }
  x
}

rna_onesided_prior <- function(n, a, bounds, upper = TRUE, check = NULL, extended = FALSE) {
  which_violates <- h_get_which_violates(geq = upper)

  x <- n * (a / sum(a)) # Neyman allocation.
  Ri <- which_violates(x[check], bounds[check]) # Indices of `check` for which x violates bounds.

  if (length(Ri) != 0L) {
    W <- seq_along(a)
    repeat {
      R <- W[check[Ri]] # Set of strata native labels for which x violates bounds.
      W <- W[-Ri] # W = W \ R.
      n <- n - sum(bounds[R])
      x <- n * (a[W] / sum(a[W])) # Neyman allocation for W.
      Ri <- which_violates(x, bounds[W]) # Indices of W for which x violates bounds.
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
