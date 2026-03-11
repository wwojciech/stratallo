# STABLE ----

#' @include utils.R
NULL

#' @name rnabox
#'
#' @title Recursive Neyman Algorithm for Optimum Sample Allocation under Box
#'   Constraints (RNABOX)
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Implements the Recursive Neyman Algorithm for Optimum Sample Allocation under
#' Box Constraints (RNABOX), as proposed in \insertCite{rnabox;textual}{stratallo}.
#' The algorithm solves the following optimum allocation problem, formulated in
#' mathematical optimization terms:
#'
#' Minimize
#' \deqn{f(x_1,\ldots,x_H) = \sum_{h=1}^H \frac{A^2_h}{x_h}}
#' over \eqn{\mathbb R_+^H}, subject to
#' \deqn{\sum_{h=1}^H x_h = n,}
#' \deqn{m_h \leq x_h \leq M_h, \qquad h = 1,\ldots,H,}
#' where \eqn{n > 0,\, A_h > 0,\, m_h > 0,\, M_h > 0}, such that
#' \eqn{m_h < M_h,\, h = 1,\ldots,H}, and
#' \eqn{\sum_{h=1}^H m_h \leq n \leq \sum_{h=1}^H M_h}, are given numbers.
#' Inequality constraints are optional and may be omitted.
#'
#' @note The `rnabox()` function is optimized for internal use and should
#'   typically not be called directly by users. Use [opt()] instead.
#'
#' @param n (`integerish(1)`)\cr total sample size.
#'   Must satisfy `n > 0`.
#'   Additionally:
#'   * If `bounds_inner` is not `NULL`, then
#'     `n >= sum(bounds_inner)` when `bounds_inner` are treated as lower bounds, or
#'     `n <= sum(bounds_inner)` when treated as upper bounds.
#'   * If `bounds_outer` is not `NULL`, then
#'     `n >= sum(bounds_outer)` when `bounds_outer` are treated as lower bounds, or
#'     `n <= sum(bounds_outer)` when treated as upper bounds.
#'
#' @param A (`numeric`)\cr population constants \eqn{A_1,\ldots,A_H}.
#'   All values must be strictly positive.
#'
#' @param bounds_inner (`numeric` or `NULL`)\cr optional bounds on sample sizes
#'   in strata for the **interim (inner) allocation** phase of `rnabox()`.
#'   These can be either lower bounds \eqn{m_1,\ldots,m_H} or upper bounds
#'   \eqn{M_1,\ldots,M_H}, depending on the value of `cmp_inner`.
#'   If `bounds_inner` is `NULL`, no bounds are imposed during the interim
#'   allocation phase.
#'
#'   If both `bounds_inner` and `bounds_outer` are not `NULL`, the following
#'   element-wise relationships must hold:
#'   * If `bounds_inner` are treated as lower bounds, then `bounds_inner < bounds_outer`.
#'   * If `bounds_inner` are treated as upper bounds, then `bounds_inner > bounds_outer`.
#'
#'   `bounds_inner` is passed by `rnabox()` to [rna()] as the `bounds` argument,
#'   serving as an interim allocation step applied before enforcing the outer
#'   `bounds_outer`.
#'
#' @param bounds_outer (`numeric` or `NULL`)\cr optional bounds on sample sizes
#'   in strata, checked during the **violation check (outer)** phase of the
#'   iterative `rnabox()` loop.
#'   These can be either lower bounds \eqn{m_1,\ldots,m_H} or upper bounds
#'   \eqn{M_1,\ldots,M_H}, depending on the value of `cmp_outer`.
#'   If `bounds_outer` is `NULL`, no bounds are imposed during the outer phase.
#'
#'   If both `bounds_outer` and `bounds_inner` are not `NULL`, the following
#'   element-wise relationships must hold:
#'   * If `bounds_outer` are treated as lower bounds, then `bounds_outer < bounds_inner`.
#'   * If `bounds_outer` are treated as upper bounds, then `bounds_outer > bounds_inner`.
#'
#' @param cmp_inner (`function`)\cr
#'   a binary comparison operator used to check for violations of `bounds_inner`.
#'   Must be either `.Primitive("<=")` or `.Primitive(">=")`.
#'   This operator determines how `bounds_inner` is handled:
#'   * `.Primitive("<=")` treats `bounds_inner` as lower bounds and causes
#'     `rnabox()` to apply the LRNA algorithm as an inner allocation step.
#'   * `.Primitive(">=")` treats `bounds_inner` as upper bounds and causes
#'     `rnabox()` to apply the RNA algorithm as an inner allocation step.
#'
#'   The value of this argument has no effect if `bounds_inner` is `NULL`.
#'
#'   `cmp_inner` is passed by `rnabox()` to [rna()] as the `cmp` argument,
#'   serving as an interim allocation step applied before enforcing the outer
#'   `bounds_outer`.
#'
#' @param cmp_outer (`function`)\cr
#'   a binary comparison operator used to check for violations of `bounds_outer`.
#'   It must be the logical complement of `cmp_inner` (up to equality):
#'   if `cmp_inner = .Primitive("<=")`, then `cmp_outer = .Primitive(">=")`,
#'   and vice versa.
#'   It determines how `bounds_outer` is handled:
#'   * `.Primitive("<=")` treats `bounds_outer` as lower bounds.
#'   * `.Primitive(">=")` treats `bounds_outer` as upper bounds.
#'
#'   The value of this argument has no effect if `bounds_outer` is `NULL`.
#'
#'   `cmp_outer` is provided solely for computational efficiency, as its value
#'   is fully determined by `cmp_inner`.
#'
#' @return A numeric vector of optimum sample allocations in strata.
#'
#' @seealso [opt()], [optcost()], [rna()], [sga()], [sgaplus()], [coma()]
#'
#' @references
#' \insertRef{rnabox}{stratallo}
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
                   bounds_inner = NULL,
                   bounds_outer = NULL,
                   cmp_inner = .Primitive(">="),
                   cmp_outer = .Primitive("<=")) {
  x <- rna(n, A, bounds = bounds_inner, cmp = cmp_inner, details = TRUE)
  tN <- x$take_neyman # Strata original indices for which the allocation is of take-Neyman.
  tB2_rel_tN <- cmp_outer(x$opt[tN], bounds_outer[tN]) # Strata (logical) indices,
  # relative to tN, for which the allocation is of take-bounds_outer.

  if (any(tB2_rel_tN)) {
    W <- seq_along(A) # Set of strata original indices. To be shrunk in repeat loop.
    tN_rel_W <- tN
    repeat {
      n <- n - sum(bounds_outer[tN[tB2_rel_tN]])
      W <- W[-tN_rel_W[tB2_rel_tN]] # W = W \ tB2 (tB2 - original strata ind. with take-bounds_outer alloc.).
      x <- rna(n, A[W], bounds = bounds_inner[W], cmp = cmp_inner, details = TRUE)
      tN_rel_W <- x$take_neyman # Indices of W for which the allocation is of take-Neyman.
      tN <- W[tN_rel_W] # Strata original indices for which the allocation is of take-Neyman.
      tB2_rel_tN <- cmp_outer(x$opt[tN_rel_W], bounds_outer[tN])
      if (!any(tB2_rel_tN)) {
        bounds_outer[W] <- x$opt
        break
      }
    }
    bounds_outer
  } else {
    x$opt
  }
}

# DEBUG VERSION  ----

rnabox_debug <- function(n,
                         A,
                         bounds_inner = NULL,
                         bounds_outer = NULL,
                         cmp_inner = .Primitive(">="),
                         cmp_outer = .Primitive("<=")) {
  x <- rna(n, A, bounds = bounds_inner, cmp = cmp_inner, details = TRUE)
  tN <- x$take_neyman # Strata original indices for which the allocation is of take-Neyman.
  tB2_rel_tN <- cmp_outer(x$opt[tN], bounds_outer[tN]) # Strata (logical) indices,
  # relative to tN, for which the allocation is of take-bounds_outer.

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
      n <- n - sum(bounds_outer[tN[tB2_rel_tN]])
      W <- W[-tN_rel_W[tB2_rel_tN]] # W = W \ tB2 (tB2 - original strata ind. with take-bounds_outer alloc.).
      x <- rna(n, A[W], bounds = bounds_inner[W], cmp = cmp_inner, details = TRUE)
      tN_rel_W <- x$take_neyman # Indices of W for which the allocation is of take-Neyman.
      tN <- W[tN_rel_W] # Strata original indices for which the allocation is of take-Neyman.
      tB2_rel_tN <- cmp_outer(x$opt[tN_rel_W], bounds_outer[tN])

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
        bounds_outer[W] <- x$opt
        break
      }
    }
    bounds_outer
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

# EXPERIMENTAL ----

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
