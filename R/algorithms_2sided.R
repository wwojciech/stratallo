#' @include helpers.R
NULL

#' Recursive Neyman Algorithm for Optimal Sample Allocation Under Box Constraints
#'
#' @description `r lifecycle::badge("stable")`
#'
#' An internal function that implements the `RNABOX` algorithm that solves the
#' following optimal allocation problem, formulated below in the language of
#' mathematical optimization.
#'
#' Minimize
#' \deqn{f(x_1,\ldots,x_H) = \sum_{h=1}^H \frac{A^2_h}{x_h}}
#' subject to
#' \deqn{\sum_{h=1}^H x_h = n}
#' \deqn{m_h \leq x_h \leq M_h, \quad h = 1,\ldots,H,}
#' where \eqn{n > 0,\, A_h > 0,\, m_h > 0,\, M_h > 0,\, h = 1, \ldots ,H}, such
#' that \eqn{\sum_{h=1}^H m_h \leq n \leq \sum_{h=1}^H M_h} are given numbers.
#' The minimization is on \eqn{\mathbb R_+^H}.
#' Inequality constraints are optional and can be skipped.
#'
#' `rnabox() function should not be called directly by the user. Use [opt()]
#' instead.
#'
#' @param n (`number`)\cr total sample size. A strictly positive scalar.
#'   If `m` is not `NULL`, it is then is required that `n >= sum(m)`.
#'   If `M` is not `NULL`, it is then is required that `n <= sum(m)`.
#' @param a (`numeric`)\cr population constants \eqn{A_1,\ldots,A_H}. Strictly
#'   positive numbers.
#' @param m (`numeric` or `NULL`)\cr lower bounds \eqn{m_1,\ldots,m_H},
#'   optionally imposed on sample sizes in strata. If no lower bounds should be
#'   imposed, then `m` must be set to `NULL`.
#' @param M (`numeric` or `NULL`)\cr upper bounds \eqn{M_1,\ldots,M_H},
#'   optionally imposed on sample sizes in strata. If no upper bounds should be
#'   imposed, then `M` must be set to `NULL`.
#'
#' @return Numeric vector with optimal sample allocations in strata.
#'
#' @seealso [opt()], [optcost()], [sga()], [sgaplus()], [coma()].
#'
#' @references
#'   To be added soon.
#'
#' @export
#' @examples
#' N <- c(454, 10, 116, 2500, 2240, 260, 39, 3000, 2500, 400)
#' S <- c(0.9, 5000, 32, 0.1, 3, 5, 300, 13, 20, 7)
#' a <- N * S
#' m <- c(322, 3, 57, 207, 715, 121, 9, 1246, 1095, 294) # lower bounds
#' M <- N # upper bounds
#'
#' # An example of a regular allocation.
#' n <- 6000
#' opt_regular <- rnabox(n, a, m, M)
#'
#' # An example of a vertex allocation.
#' n <- 4076
#' opt_vertex <- rnabox(n, a, m, M)
rnabox <- function(n, a, m = NULL, M = NULL) {
  x <- rna(n, a, M, details = TRUE)
  tN <- x$take_neyman # Strata original indices for which allocation is take-Neyman.
  L_rel_tN <- x$opt[tN] <= m[tN] # (logical) Indices of tN for which x violates l. bounds.

  if (any(L_rel_tN)) {
    W <- seq_along(a) # Set of strata original indices. To be shrunk in repeat loop.
    tN_rel_W <- tN
    repeat {
      n <- n - sum(m[tN[L_rel_tN]])
      W <- W[-tN_rel_W[L_rel_tN]] # W = W \ R (R - original indices for which x violates l. bounds).
      x <- rna(n, a[W], M[W], details = TRUE)
      tN_rel_W <- x$take_neyman # Indices of W for which the allocation is take-Neyman.
      tN <- W[tN_rel_W] # Strata original indices for which allocation is take-Neyman.
      L_rel_tN <- x$opt[tN_rel_W] <= m[tN]
      if (!any(L_rel_tN)) {
        m[W] <- x$opt
        break
      }
    }
    m
  } else {
    x$opt
  }
}

# TESTS ----

rnabox_debug <- function(n, a, m, M) {
  W <- seq_along(a)

  debug_data <- list()
  L <- NULL
  repeat {
    x <- rna(n, a[W], M[W], details = TRUE) # step 1
    Li <- x$opt <= m[W] # step 2
    debug_data <- c(
      debug_data,
      list(list(L = L, U = W[x$take_bound], U_iter = x$iter, Li = W[Li], s0 = x$s0, s = x$s))
    )
    L <- c(L, W[Li])
    if (any(Li)) { # step 3
      n <- n - sum(m[W[Li]])
      W <- W[!Li]
    } else {
      m[W] <- x$opt
      break
    }
  }
  names(debug_data) <- paste0("Iteration_", seq_along(debug_data))
  list(x = m, details = debug_data)
}

# extracts detailed debug information from the output for rnabox_debug.
# this can be a full info about assignments of every stratum (if short is FALSE)
# or just a short summary table with the number of elements on assignments
rnabox_debug_summary <- function(assignments, a, m, M, short = TRUE) {
  if (short) {
    short_df <- t(sapply(assignments, function(i) {
      c(
        L_size = length(i$L),
        U_size = length(i$U),
        U_iter = i$U_iter,
        Li_size = length(i$Li),
        "s(L, 0)" = i$s0,
        "s(L, U)" = i$s
      )
    }))
    short_df <- cbind.data.frame(
      Iteration = as.integer(gsub("^Iteration_", "", rownames(short_df))),
      short_df
    )
    rownames(short_df) <- NULL
    short_df
  } else {
    assign_df <- lapply(names(assignments), function(iter_name) {
      i <- assignments[[iter_name]]
      data.frame(
        Iteration = as.integer(gsub("^Iteration_", "", iter_name)),
        h = c(i$L, i$U),
        Type = c(rep("L", length(i$L)), rep("U", length(i$U)))
      )
    })
    data.frame(do.call(rbind, assign_df))
  }
}

# Wersja podstawowa.
rnabox_v0 <- function(n, a, m, M) {
  W <- seq_along(a)

  repeat {
    x <- rna(n, a[W], M[W]) # step 1
    L <- x <= m[W] # step 2
    if (any(L)) { # step 3
      n <- n - sum(m[W[L]])
      W <- W[!L]
    } else {
      break
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
  x <- rna(n, a, M) # step 1
  L <- x <= m # step 2
  if (any(L)) { # step 3
    W <- seq_along(a)
    repeat {
      n <- n - sum(m[W[L]])
      W <- W[!L]
      x <- rna(n, a[W], M[W]) # step 1
      L <- x <= m[W] # step 2
      if (!any(L)) { # step 3
        m[W] <- x
        break
      }
    }
    m
  } else {
    x
  }
}

# Wersja, ktora nie sprawdza w kroku 2: x_h >= m_h dla w: x_h = M_h.
rnabox_v1 <- function(n, a, m, M) {
  W <- seq_along(a)

  repeat {
    x <- rna(n, a[W], M[W], details = TRUE) # step 1
    Uc_rel_W <- x$take_neyman
    Uc <- W[Uc_rel_W] # take-Neyman

    L <- x$opt[Uc_rel_W] <= m[Uc] # step 2
    if (any(L)) { # step 3
      n <- n - sum(m[Uc[L]])
      W <- W[-Uc_rel_W[L]]
    } else {
      break
    }
  }

  # To improve the performance, otherwise, else block only.
  if (length(W) == length(m)) {
    x$opt
  } else {
    m[W] <- x$opt
    m
  }
}

# Taka sama logika jak rnabox_v1, sprytniejsze zakodowanie
# (pierwsza iteracja nie jest subsetowana przez W - tak jest szybciej)
# Ta wersja jest aktualnie w pakiecie stratallo pod nazwa rnabox.
rnabox_v11 <- function(n, a, m, M) {
  x <- rna(n, a, M, details = TRUE) # step 1
  Uc <- x$take_neyman
  L <- x$opt[Uc] <= m[Uc] # step 2

  if (any(L)) { # step 3
    W <- seq_along(a)
    Uc_rel_W <- Uc
    repeat {
      n <- n - sum(m[Uc[L]])
      W <- W[-Uc_rel_W[L]]
      x <- rna(n, a[W], M[W], details = TRUE) # step 1
      Uc_rel_W <- x$take_neyman
      Uc <- W[Uc_rel_W]
      L <- x$opt[Uc_rel_W] <= m[Uc] # step 2
      if (!any(L)) { # step 3
        m[W] <- x$opt
        break
      }
    }
    m
  } else {
    x$opt
  }
}

# TODO with prior information for RNA. Teraz nie dziala.
rnabox_v2 <- function(n, a, m, M) {
  x <- rna(n, a, M, details = TRUE) # step 1
  Uc <- x$take_neyman
  L <- x$opt[Uc] <= m[Uc] # step 2

  if (any(L)) { # step 3
    W <- seq_along(a)
    Uc_rel_W <- Uc
    repeat {
      n <- n - sum(m[Uc[L]])
      W <- W[-Uc_rel_W[L]]
      x <- rna_prior(n, a[W], M[W], check = "TODO", details = TRUE) # step 1
      Uc_rel_W <- x$take_neyman
      Uc <- W[Uc_rel_W]
      L <- x$opt[Uc_rel_W] <= m[Uc] # step 2
      if (!any(L)) { # step 3
        m[W] <- x$opt
        break
      }
    }
    m
  } else {
    x$opt
  }
}

# w trakcie pisania
rnabox_v3_sym <- function(n, a, m, M) {
  W <- seq_along(a)

  repeat {
    x <- rna(n, a[W], M[W]) # step 1
    L <- x <= m[W] # step 2
    if (length(L) == 0L) { # step 3
      break
    } else {
      x <- rna(n, a[W], m[W], .Primitive(">=")) # step 1
      n <- n - sum(m[W[L]])
      W <- W[!L]
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
