#' @name obj_emptiness
#'
#' @title Object Emptiness (`NULL` or zero-length)
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Utility functions for checking whether an object is empty, where emptiness
#' is defined as being `NULL` or having length 0.
#'
#' @param x object to test.
#'
#' @examples
#' # internal functions (not exported) – examples skipped
#'
NULL

#' @describeIn obj_emptiness
#' Returns `TRUE` if the object is `NULL` or has length 0, and `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' is_empty(NULL)
#' is_empty(character(0))
#' is_empty(1)
#' }
#'
is_empty <- function(x) {
  length(x) == 0L
}

#' @describeIn obj_emptiness
#' Logical negation of [is_empty()].
#' This function directly checks if `length(x) > 0L` for performance reasons,
#' avoiding the extra negation step that would occur if using `!is_empty(x)`.
#' It is optimized for repeated use in algorithms where [is_nonempty()] is called
#' many times.
#'
#' @examples
#' \dontrun{
#' is_nonempty(NULL)
#' is_nonempty(character(0))
#' is_nonempty(1)
#' }
#'
is_nonempty <- function(x) {
  length(x) > 0L
}

#' @title Check Numeric Equality within a Tolerance
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Compares two numeric vectors element-wise using an adaptive tolerance
#' sequence ranging from `10^-19` to `10^tol_max`. The smallest tolerance at
#' which the values are considered equal is returned as the corresponding name
#' in the output.
#'
#' @param x (`numeric`)\cr first vector to compare.
#' @param y (`numeric`)\cr second vector to compare.
#' @param tol_max (`integerish(1)`)\cr the maximum tolerance exponent
#'   (as a power of 10).
#'
#' @return A logical vector indicating whether each element pair is equal
#'   within the detected tolerance. The names reflect the tolerance used.
#'
#' @examples
#' # internal functions (not exported) – examples skipped
#' \dontrun{
#' is_equal(c(3, 4), c(3, 4))
#' is_equal(c(3, 4), c(3.01, 4.11))
#' is_equal(c(3, 4), c(3.01, 4.11), tol_max = 0)
#' }
#'
is_equal <- function(x, y, tol_max = -1) {
  abs_diff <- abs(x - y)
  tolerances <- 10^((-19):tol_max)

  results <- rep(FALSE, length(abs_diff))
  names(results) <- rep(
    paste0("tol=", formatC(10^tol_max, format = "e", digits = 0)),
    length(abs_diff)
  )

  for (tol in tolerances) {
    idx <- !results & abs_diff <= tol
    if (any(idx)) {
      results[idx] <- TRUE
      names(results)[idx] <- paste0("tol=", formatC(tol, format = "e", digits = 0))
    }
    if (all(results)) {
      break
    }
  }

  results
}

#' @title Check for Mixed Signs in a Numeric Vector
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Determines whether a numeric vector contains both negative and positive
#' values. Zero (`0`) is treated as neutral and does not count as either sign.
#'
#' @param x (`numeric`)\cr a vector to check.
#'
#' @return `TRUE` if the vector contains both positive and negative values,
#'   `FALSE` otherwise.
#'
#' @examples
#' # internal functions (not exported) – examples skipped
#' \dontrun{
#' has_mixed_signs(1:5)
#' has_mixed_signs(-(1:5))
#' has_mixed_signs(c(-1, -2, 3))
#' has_mixed_signs(c(0, -1))
#' has_mixed_signs(c(0, 1))
#' has_mixed_signs(c(0, 1, -1))
#' }
#'
has_mixed_signs <- function(x) {
  any(x < 0) && any(x > 0)
}
