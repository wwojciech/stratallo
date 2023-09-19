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
#' where \eqn{n > 0,\, A_h > 0,\, m_h > 0,\, M_h > 0}, such that
#' \eqn{m_h < M_h,\, h = 1,\ldots,H}, and
#' \eqn{\sum_{h=1}^H m_h \leq n \leq \sum_{h=1}^H M_h}, are given numbers.
#' The minimization is on \eqn{\mathbb R_+^H}.
#' Inequality constraints are optional and can be skipped.
#'
#' `rnabox()` function should not be called directly by the user. Use [opt()]
#' instead.
#'
#' @param n (`number`)\cr total sample size. A strictly positive scalar.
#'   If `bounds1` is not `NULL`, it is then required that `n >= sum(bounds1)`
#'   (given that `bounds1` are treated as lower bounds) or `n <= sum(bounds1)`
#'   (given that `bounds1` are treated as upper bounds).
#'   If `bounds2` is not `NULL`, it is then required that `n >= sum(bounds2)`
#'   (given that `bounds2` are treated as lower bounds) or `n <= sum(bounds2)`
#'   (given that `bounds2` are treated as upper bounds).
#' @param A (`numeric`)\cr population constants \eqn{A_1,\ldots,A_H}. Strictly
#'   positive numbers.
#' @param bounds1 (`numeric` or `NULL`)\cr lower bounds \eqn{m_1,\ldots,m_H},
#'   or upper bounds \eqn{M_1,\ldots,M_H} optionally imposed on sample sizes in
#'   strata. The interpretation of `bounds1` depends on the value of
#'   `check_violations1`. If no one-sided bounds 1 should be imposed, then
#'   `bounds1` must be set to `NULL`. If `bounds2` is not `NULL`, it is then
#'   required that either `bounds1 < bounds2` (in case when `bounds1` is treated
#'   as lower bounds) or `bounds1 > bounds2` (in the opposite case).
#' @param bounds2 (`numeric` or `NULL`)\cr lower bounds \eqn{m_1,\ldots,m_H},
#'   or upper bounds \eqn{M_1,\ldots,M_H} optionally imposed on sample sizes in
#'   strata. The interpretation of `bounds2` depends on the value of
#'   `check_violations2`. If no one-sided bounds 2 should be imposed, then
#'   `bounds2` must be set to `NULL`. If `bounds2` is not `NULL`, it is then
#'   required that either `bounds1 < bounds2` (in case when `bounds1` is treated
#'   as lower bounds) or `bounds1 > bounds2` (in the opposite case).
#' @param check_violations1 (`function`) \cr 2-arguments binary operator function
#'   that allows the comparison of values in atomic vectors. It must either be
#'   set to `.Primitive("<=")` or `.Primitive(">=")`.
#'   The first of these choices causes that `bounds1` are treated as lower
#'   bounds and the `rnabox()` uses the \emph{LRNA} algorithm as in interim
#'   algorithm for the allocation problem with one-sided lower bounds `bounds1`.
#'   The latter option causes that `bounds1` are treated as upper bounds and the
#'   `rnabox()` uses the \emph{RNA} algorithm as in interim algorithm for the
#'   allocation problem with one-sided upper bounds `bounds1`.
#'   This parameter is correlated with `check_violations2`. That is, these
#'   arguments must be set against each other.
#'   `check_violations1` is ignored when `bounds1` is set to `NULL`.
#' @param check_violations2 (`function`) \cr 2-arguments binary operator function
#'   that allows the comparison of values in atomic vectors. It must either be
#'   set to `.Primitive("<=")` or `.Primitive(">=")`.
#'   The first of these choices causes that `bounds2` are treated as lower
#'   bounds and the `rnabox()` uses the \emph{LRNA} algorithm as in interim
#'   algorithm for the allocation problem with one-sided lower bounds `bounds2`.
#'   The latter option causes that `bounds2` are treated as upper bounds and the
#'   `rnabox()` uses the \emph{RNA} algorithm as in interim algorithm for the
#'   allocation problem with one-sided upper bounds `bounds2`.
#'   This parameter is correlated with `check_violations1`. That is, these
#'   arguments must be set against each other.
#'   `check_violations2` is ignored when `bounds2` is set to `NULL`.
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
#' A <- N * S
#' m <- c(322, 3, 57, 207, 715, 121, 9, 1246, 1095, 294) # lower bounds
#' M <- N # upper bounds
#'
#' # Regular allocation.
#' n <- 6000
#' opt_regular <- rnabox(n, A, M, m)
#'
#' # Vertex allocation.
#' n <- 4076
#' opt_vertex <- rnabox(n, A, M, m)
rnabox <- function(n,
                   A,
                   bounds1 = NULL,
                   bounds2 = NULL,
                   check_violations1 = .Primitive(">="),
                   check_violations2 = .Primitive("<=")) {
  x <- rna(n, A, bounds1, check_violations = check_violations1, details = TRUE)
  tN <- x$take_neyman # Strata original indices for which the allocation is of take-Neyman.
  tB2_rel_tN <- check_violations2(x$opt[tN], bounds2[tN]) # Strata (logical) indices,
  # relative to tN, for which the allocation is of take-bounds2.

  if (any(tB2_rel_tN)) {
    W <- seq_along(A) # Set of strata original indices. To be shrunk in repeat loop.
    tN_rel_W <- tN
    repeat {
      n <- n - sum(bounds2[tN[tB2_rel_tN]])
      W <- W[-tN_rel_W[tB2_rel_tN]] # W = W \ tB2 (tB2 - original strata ind. with take-bounds2 alloc.).
      x <- rna(n, A[W], bounds1[W], check_violations = check_violations1, details = TRUE)
      tN_rel_W <- x$take_neyman # Indices of W for which the allocation is of take-Neyman.
      tN <- W[tN_rel_W] # Strata original indices for which the allocation is of take-Neyman.
      tB2_rel_tN <- check_violations2(x$opt[tN_rel_W], bounds2[tN])
      if (!any(tB2_rel_tN)) {
        bounds2[W] <- x$opt
        break
      }
    }
    bounds2
  } else {
    x$opt
  }
}

# Debug  ----

rnabox_debug <- function(n,
                         A,
                         bounds1 = NULL,
                         bounds2 = NULL,
                         check_violations1 = .Primitive(">="),
                         check_violations2 = .Primitive("<=")) {
  x <- rna(n, A, bounds1, check_violations = check_violations1, details = TRUE)
  tN <- x$take_neyman # Strata original indices for which the allocation is of take-Neyman.
  tB2_rel_tN <- check_violations2(x$opt[tN], bounds2[tN]) # Strata (logical) indices,
  # relative to tN, for which the allocation is of take-bounds2.

  # Collect debug data.
  debug_data <- list(
    list(
      tB1 = x$take_bound,
      tB2 = tN[tB2_rel_tN],
      tB2i = tN[tB2_rel_tN],
      tB1_iter = x$iter,
      s0 = x$s0,
      s = x$s
    )
  )
  tB2 <- tN[tB2_rel_tN]

  xopt <- if (any(tB2_rel_tN)) {
    W <- seq_along(A) # Set of strata original indices. To be shrunk in repeat loop.
    tN_rel_W <- tN
    repeat {
      n <- n - sum(bounds2[tN[tB2_rel_tN]])
      W <- W[-tN_rel_W[tB2_rel_tN]] # W = W \ tB2 (tB2 - original strata ind. with take-bounds2 alloc.).
      x <- rna(n, A[W], bounds1[W], check_violations = check_violations1, details = TRUE)
      tN_rel_W <- x$take_neyman # Indices of W for which the allocation is of take-Neyman.
      tN <- W[tN_rel_W] # Strata original indices for which the allocation is of take-Neyman.
      tB2_rel_tN <- check_violations2(x$opt[tN_rel_W], bounds2[tN])

      # Collect debug data.
      debug_data <- c(
        debug_data,
        list(
          list(
            tB1 = W[x$take_bound],
            tB2 = tB2,
            tB2i = tN[tB2_rel_tN],
            tB1_iter = x$iter,
            s0 = x$s0,
            s = x$s
          )
        )
      )
      tB2 <- c(tB2, tN[tB2_rel_tN])

      if (!any(tB2_rel_tN)) {
        bounds2[W] <- x$opt
        break
      }
    }
    bounds2
  } else {
    x$opt
  }

  names(debug_data) <- paste0("Iteration_", seq_along(debug_data))
  list(x = xopt, details = debug_data)
}

# extracts detailed debug information from the output for rnabox_debug.
# this can be a full info about assignments of every stratum (if short is FALSE)
# or just a short summary table with the number of elements on assignments
rnabox_debug_summary <- function(assignments, short = TRUE) {
  if (short) {
    short_df <- t(sapply(assignments, function(i) {
      c(
        tB1_size = length(i$tB1),
        tB2_size = length(i$tB2),
        tB2i_size = length(i$tB2i),
        tB1_iter = i$tB1_iter,
        "s_before_rna" = i$s0,
        "s_after_rna" = i$s
      )
    }))
    short_df <- cbind.data.frame(
      iteration = as.integer(gsub("^Iteration_", "", rownames(short_df))),
      short_df
    )
    rownames(short_df) <- NULL
    short_df
  } else {
    assign_df <- lapply(names(assignments), function(iter_name) {
      i <- assignments[[iter_name]]
      data.frame(
        iteration = as.integer(gsub("^Iteration_", "", iter_name)),
        h = c(i$tB1, i$tB2),
        type = c(rep("tB1", length(i$tB1)), rep("tB2", length(i$tB2)))
      )
    })
    data.frame(do.call(rbind, assign_df))
  }
}

# rnabox_debug_old <- function(n, A, m, M) {
#   W <- seq_along(A)
#
#   debug_data <- list()
#   L <- NULL
#   repeat {
#     x <- rna(n, A[W], M[W], details = TRUE) # step 1
#     Li <- x$opt <= m[W] # step 2
#     debug_data <- c(
#       debug_data,
#       list(list(L = L, U = W[x$take_bound], U_iter = x$iter, Li = W[Li], s0 = x$s0, s = x$s))
#     )
#     L <- c(L, W[Li])
#     if (any(Li)) { # step 3
#       n <- n - sum(m[W[Li]])
#       W <- W[!Li]
#     } else {
#       m[W] <- x$opt
#       break
#     }
#   }
#   names(debug_data) <- paste0("Iteration_", seq_along(debug_data))
#   list(x = m, details = debug_data)
# }
#
# # extracts detailed debug information from the output for rnabox_debug.
# # this can be a full info about assignments of every stratum (if short is FALSE)
# # or just a short summary table with the number of elements on assignments
# rnabox_debug_summary_old <- function(assignments, short = TRUE) {
#   if (short) {
#     short_df <- t(sapply(assignments, function(i) {
#       c(
#         L_size = length(i$L),
#         U_size = length(i$U),
#         U_iter = i$U_iter,
#         Li_size = length(i$Li),
#         "s(L, 0)" = i$s0,
#         "s(L, U)" = i$s
#       )
#     }))
#     short_df <- cbind.data.frame(
#       Iteration = as.integer(gsub("^Iteration_", "", rownames(short_df))),
#       short_df
#     )
#     rownames(short_df) <- NULL
#     short_df
#   } else {
#     assign_df <- lapply(names(assignments), function(iter_name) {
#       i <- assignments[[iter_name]]
#       data.frame(
#         Iteration = as.integer(gsub("^Iteration_", "", iter_name)),
#         h = c(i$L, i$U),
#         Type = c(rep("L", length(i$L)), rep("U", length(i$U)))
#       )
#     })
#     data.frame(do.call(rbind, assign_df))
#   }
# }

# Test versions  ----

# Wersja podstawowa.
rnabox_v0 <- function(n, A, m, M) {
  W <- seq_along(A)

  repeat {
    x <- rna(n, A[W], M[W]) # step 1
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
rnabox_v01 <- function(n, A, m, M) {
  x <- rna(n, A, M) # step 1
  L <- x <= m # step 2
  if (any(L)) { # step 3
    W <- seq_along(A)
    repeat {
      n <- n - sum(m[W[L]])
      W <- W[!L]
      x <- rna(n, A[W], M[W]) # step 1
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
rnabox_v1 <- function(n, A, m, M) {
  W <- seq_along(A)

  repeat {
    x <- rna(n, A[W], M[W], details = TRUE) # step 1
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
rnabox_v11 <- function(n, A, m, M) {
  x <- rna(n, A, M, details = TRUE) # step 1
  Uc <- x$take_neyman
  L <- x$opt[Uc] <= m[Uc] # step 2

  if (any(L)) { # step 3
    W <- seq_along(A)
    Uc_rel_W <- Uc
    repeat {
      n <- n - sum(m[Uc[L]])
      W <- W[-Uc_rel_W[L]]
      x <- rna(n, A[W], M[W], details = TRUE) # step 1
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
rnabox_v2 <- function(n, A, m, M) {
  x <- rna(n, A, M, details = TRUE) # step 1
  Uc <- x$take_neyman
  L <- x$opt[Uc] <= m[Uc] # step 2

  if (any(L)) { # step 3
    W <- seq_along(A)
    Uc_rel_W <- Uc
    repeat {
      n <- n - sum(m[Uc[L]])
      W <- W[-Uc_rel_W[L]]
      x <- rna_prior(n, A[W], M[W], check = "TODO", details = TRUE) # step 1
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
rnabox_v3_sym <- function(n, A, m, M) {
  W <- seq_along(A)

  repeat {
    x <- rna(n, A[W], M[W]) # step 1
    L <- x <= m[W] # step 2
    if (length(L) == 0L) { # step 3
      break
    } else {
      x <- rna(n, A[W], m[W], .Primitive(">=")) # step 1
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
