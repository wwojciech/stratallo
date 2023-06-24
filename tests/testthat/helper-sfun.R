sfun <- function(n, a, bounds = NULL, ucosts = 1, R = NULL) {
  if (!is.null(bounds)) {
    assert_true(length(a) == length(bounds))
  }
  if (setequal(seq_along(a), R)) {
    bounds
  } else if (is.null(R)) {
    Rc <- seq_along(a)
    n / sum(a * sqrt(ucosts))
  } else {
    Rc <- seq_along(a)[-R]
    (n - sum((ucosts * bounds)[R])) / sum((a * sqrt(ucosts))[Rc])
  }
}
