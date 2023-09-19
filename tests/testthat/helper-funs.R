sfun <- function(n, A, bounds = NULL, ucosts = 1, R = NULL) {
  if (!is.null(bounds)) {
    assert_true(length(A) == length(bounds))
  }
  if (setequal(seq_along(A), R)) {
    bounds
  } else if (is.null(R)) {
    Rc <- seq_along(A)
    n / sum(A * sqrt(ucosts))
  } else {
    Rc <- seq_along(A)[-R]
    (n - sum((ucosts * bounds)[R])) / sum((A * sqrt(ucosts))[Rc])
  }
}

leq <- .Primitive("<=")
geq <- .Primitive(">=")
