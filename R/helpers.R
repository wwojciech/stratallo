#' @title Get Proper Version of `which_violated` Function.
#'
#' @description `r lifecycle::badge("stable")`
#'
#'   Internal function that prepares a simple 2-arguments wrapper of
#'   [`base::which()`] that checks whether its first argument exceeds the second
#'   one. Both arguments are numeric. This excess is hard coded in the returned
#'   wrapper function and it is defined either as \eqn{>=} ("greater or equal")
#'   or \eqn{<=} ("lower or equal"), depending on the value of the `geq` flag.
#'
#' @param geq (`flag`) \cr if `TRUE`, then "greater or equal" condition is set.
#'   Otherwise, "less then or equal" is set.
#'
#' @return 2-arguments function that checks whether its first argument exceeds
#'   the second one. Both arguments must be numeric.
#'
#' @seealso [rna_one_sided()].
#'
#' @examples
#' which_violated <- stratallo:::h_get_which_violated()
#' which_violated(1:3, 3:1)
#'
#' which_violated <- stratallo:::h_get_which_violated(geq = FALSE)
#' which_violated(1:3, 3:1)
h_get_which_violated <- function(geq = TRUE) {
  assert_flag(geq)
  if (geq) {
    function(x, y) {
      which(x >= y)
    }
  } else {
    function(x, y) {
      which(x <= y)
    }
  }
}
