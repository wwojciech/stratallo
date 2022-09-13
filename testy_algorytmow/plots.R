plot_rnabox <- function(assignments, a, m, M, n_title = NULL) {
  rmin <- .Machine$double.xmin
  assert_list(assignments)
  assert_numeric(a, lower = rmin, finite = TRUE, any.missing = FALSE, min.len = 2)
  assert_numeric(m, lower = rmin, finite = TRUE, any.missing = FALSE, min.len = 2)
  assert_numeric(M, lower = rmin, finite = TRUE, any.missing = FALSE, min.len = 2)

  m_ordered <- c(0, order(a / m, decreasing = FALSE))
  M_ordered <- c(0, order(a / M, decreasing = TRUE)) # nolintr

  # Check correctness.
  check_asign <- sapply(assignments, function(i) {
    c(
      all(sort(match(i[["L"]], m_ordered[-1])) == seq_along(i[["L"]])),
      all(sort(match(i[["U"]], M_ordered[-1])) == seq_along(i[["U"]]))
    )
  })
  if (!all(check_asign)) {
    stop("Layers were not assigned to take-min/take-max correctly.")
  }

  # Data with points.
  data_p <- lapply(assignments, function(i) {
    nL <- length(i$L)
    nU <- length(i$U)
    if (nU > 0) {
      data.frame(nL = c(nL, nL), nU = c(0, nU), s = c(i$s0, i$s), Result = c("step 3: L", "step 2: U"))
    } else {
      stopifnot(i$s0 == i$s) # It cannot be that nU = 0 and s0 != s.
      data.frame(nL = nL, nU = nU, s = i$s, Result = "step 3: L")
    }
  })
  data_p <- data.frame(do.call(rbind, data_p))
  data_p[1, "Result"] <- "step 1: init"

  # Prepare data with blocked strata.
  data_b <- lapply(seq_len(nrow(data_p)), function(i) {
    nL <- data_p[i, "nL"]
    nU <- data_p[i, "nU"]
    blckd <- NULL
    if (nU > 0) {
      blckd <- cbind(
        l = M_ordered[(1:nU) + 1],
        u = M_ordered[nU + 1],
        Result = "blocked single for L"
      )
    }
    if (nL > 0) {
      blckd <- rbind(
        blckd,
        cbind(
          l = m_ordered[nL + 1],
          u = m_ordered[(1:nL) + 1],
          Result = "blocked single for U"
        )
      )
    }
    blckd
  })
  data_b <- data.frame(do.call(rbind, data_b))
  data_b <- unique(data_b)

  # Convert nL, nU into strata indices relative to m_ordered + 1, M_ordered + 1.
  data_p$l <- factor(m_ordered[data_p$nL + 1], level = m_ordered)
  data_p$u <- factor(M_ordered[data_p$nU + 1], level = M_ordered)

  # data with values of s function.
  lu_names <- c("l", "u")
  data_s <- data_p[, c(lu_names, "s")]

  # data for segments.
  data_seg <- cbind(
    data_p[-nrow(data_p), lu_names],
    data_p[-1, lu_names]
  )
  colnames(data_seg)[3:4] <- paste0(lu_names, "end")

  # rbind data_p with blocked points - data_b
  data_p <- rbind(data_p[, c(lu_names, "Result")], data_b[, c(lu_names, "Result")])

  # Prepare colors for take-min/take-max strata.
  H <- length(a)
  niter <- length(assignments)
  nU <- length(assignments[[niter]]$U)
  nL <- length(assignments[[niter]]$L)
  x_text_fill <- c(NA, rep("peachpuff2", nU), rep(NA, H - nU))
  y_text_fill <- c(NA, rep("sienna1", nL), rep(NA, H - nL))

  # Plot it.
  if (!is.null(n_title)) {
    if (is.numeric(n_title)) {
      n_title <- formatC(n_title, digits = 0, format = "f")
      n_title <- paste0("n = ", n_title)
    }
    n_title <- paste0(", ", n_title, ").")
  } else {
    n_title <- ")."
  }

  ggplot(data_p, aes(x = u, y = l)) +
    geom_point(aes(shape = Result), size = 3) +
    geom_segment(
      data = data_seg,
      mapping = aes(xend = uend, yend = lend),
      lineend = "round",
      linejoin = "bevel",
      size = 0.2,
      arrow = arrow(angle = 12, length = unit(0.4, "cm"))
    ) +
    geom_text(
      data = data_s,
      # aes(label = formatC(s, digits = 1, width = 1, format = "e")),
      mapping = aes(label = formatC(s, digits = 3, width = 1)),
      size = 3.5,
      nudge_x = 0.2,
      nudge_y = 0.2
    ) +
    theme(
      axis.text.x = ggtext::element_markdown(
        fill = x_text_fill,
        padding = unit(rep(4, 4), "pt"),
        r = unit(5, "pt")
      ),
      axis.text.y = ggtext::element_markdown(
        fill = y_text_fill,
        padding = unit(rep(4, 4), "pt"),
        r = unit(5, "pt")
      )
    ) +
    scale_shape_manual(
      values = c(1, 15, 16, 13, 7),
      breaks = c("step 1: init", "step 2: U", "step 3: L", "blocked single for L", "blocked single for U")
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    ggtitle(
      paste0(
        "Assignments of strata into take-min/take-max in the RNABOX algorithm ",
        "(", H, " strata", n_title
      )
    ) +
    xlab(expression(atop(sigma(W), "take-max strata (U)"))) +
    ylab(expression(atop("take-min strata (L)", tau(W))))
}

plot_rnabox_old <- function(assignments, a, m, M, n) {
  rmin <- .Machine$double.xmin
  assert_numeric(a, lower = rmin, finite = TRUE, any.missing = FALSE, min.len = 2)
  assert_numeric(m, lower = rmin, finite = TRUE, any.missing = FALSE, min.len = 2)
  assert_numeric(M, lower = rmin, finite = TRUE, any.missing = FALSE, min.len = 2)

  m_order <- c(0, order(a / m, decreasing = FALSE))
  M_order <- c(0, order(a / M, decreasing = TRUE)) # nolintr

  # check correctness.
  check_asign <- sapply(assignments, function(i) {
    c(
      all(sort(match(i[["L_cum"]], m_order[-1])) == seq_along(i[["L_cum"]])),
      all(sort(match(i[["U_rna"]], M_order[-1])) == seq_along(i[["U_rna"]]))
    )
  })
  if (!all(check_asign)) {
    stop("Layers were not assigned to take-min/take-max correctly.")
  }

  # data with points.
  data_p <- lapply(assignments, function(i) {
    c(L_cum = length(i$L_cum), U_rna = length(i$U_rna), s0 = i$s0, s = i$s)
  })
  data_p <- lapply(data_p, function(i) {
    if (i[["U_rna"]] > 0) {
      rbind(
        c(l = i[["L_cum"]], u = 0, s = i[["s0"]]),
        i[c("L_cum", "U_rna", "s")]
      )
    } else {
      if (i[["s"]] != i[["s0"]]) {
        stop(paste("Wrong s values", i[["s"]], ", ", i[["s0"]]))
      }
      i[c("L_cum", "U_rna", "s")]
    }
  })
  data_p <- data.frame(do.call(rbind, data_p))

  Llen <- tail(data_p, 1)[, "l"] # TODO, moze byc tak, ze nie ma l i u
  Ulen <- tail(data_p, 1)[, "u"]

  # data with blocked strata.
  data_b <- do.call(
    rbind,
    lapply(1:nrow(data_p), function(i) { # nolintr
      di <- data_p[i, ]
      blckd <- data.frame()
      if (di$u > 0) {
        blckd <- rbind(blckd, cbind(l = M_order[(1:di$u) + 1], u = M_order[di$u + 1]))
      }
      if (di$l > 0) {
        blckd <- rbind(blckd, cbind(l = m_order[di$l + 1], u = m_order[(1:di$l) + 1]))
      }
      blckd
    })
  )
  data_b <- unique(data_b)

  data_p[, "l"] <- factor(m_order[data_p$l + 1], level = m_order)
  data_p[, "u"] <- factor(M_order[data_p$u + 1], level = M_order)

  # data with values of s function.
  data_s <- data_p
  cols_lu <- c("l", "u")
  data_p <- data_p[, cols_lu]

  # data for segments.
  data_seg <- cbind(head(data_p, -1), tail(data_p, -1))
  colnames(data_seg)[3:4] <- paste0(cols_lu, "end")

  # add type into data points.
  data_p$Strata <- ifelse(data_p$u == 0, "step 2: RNA", "step 3: <= m")
  data_b$Strata <- "blocked"

  data_p <- rbind(data_p[, c(cols_lu, "Strata")], data_b[, c(cols_lu, "Strata")])

  # prepare colors for take-min/take-max strata.
  H <- length(a)
  x_text_fill <- c(NA, rep("peachpuff2", Ulen), rep(NA, H - Ulen))
  y_text_fill <- c(NA, rep("sienna1", Llen), rep(NA, H - Llen))

  # plot it.
  ggplot(data_p, aes(x = u, y = l)) +
    geom_point(aes(shape = Strata), size = 3) +
    geom_segment(
      aes(xend = uend, yend = lend),
      data_seg,
      lineend = "round",
      linejoin = "bevel",
      size = 0.2,
      arrow = arrow(angle = 12, length = unit(0.4, "cm"))
    ) +
    geom_text(
      # aes(label = formatC(s, digits = 1, width = 1, format = "e")),
      aes(label = formatC(s, digits = 3, width = 1)),
      data = data_s,
      size = 3.5,
      nudge_x = 0.2,
      nudge_y = 0.2
    ) +
    theme(
      axis.text.x = ggtext::element_markdown(
        fill = x_text_fill,
        padding = unit(rep(4, 4), "pt"),
        r = unit(5, "pt")
      ),
      axis.text.y = ggtext::element_markdown(
        fill = y_text_fill,
        padding = unit(rep(4, 4), "pt"),
        r = unit(5, "pt")
      )
    ) +
    scale_shape_manual(
      values = c(16, 15, 4),
      breaks = c("step 2: RNA", "step 3: <= m", "blocked")
    ) +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    ggtitle(
      paste0(
        "Assignments of strata into take-min/take-max in the RNABOX algorithm (",
        H,
        " strata, n = ",
        formatC(n, digits = 2, format = "f"),
        "N)"
      )
    ) +
    xlab(expression(atop(sigma(W), "take-max strata"))) +
    ylab(expression(atop("take-min strata", tau(W))))
}

