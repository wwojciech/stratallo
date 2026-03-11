#' @name rounding
#'
#' @title Rounding of Numbers
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' @param x (`numeric`)\cr numeric vector.
#'
#' @examples
#' x <- c(4.5, 4.1, 4.9)
#'
#' @return An integer vector.
NULL

#' @describeIn rounding
#' Random rounding of numbers.
#' A number \eqn{x} is rounded to an integer \eqn{y} according to the following
#' rule:
#'
#' \deqn{y = \left\lfloor x \right\rfloor +
#'   I\!\left(u < x - \left\lfloor x \right\rfloor\right),}
#' where the indicator function \eqn{I : \{FALSE,\, TRUE\} \to \{0,\, 1\}} is
#' defined as
#' \deqn{
#'   I(b) :=
#'   \begin{cases}
#'     0, & b \text{ is } FALSE, \\
#'     1, & b \text{ is } TRUE,
#'   \end{cases}
#' }
#' and \eqn{u} is a random number drawn from the \eqn{\mathrm{Uniform}(0, 1)}
#' distribution.
#'
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' set.seed(5)
#' round_ran(x) # 5 4 4
#'
#' set.seed(6)
#' round_ran(x) # 4 4 5
#'
round_ran <- function(x) {
  assert_numeric(x)
  as.integer(floor(x) + (runif(length(x)) < (x - floor(x))))
}

#' @describeIn rounding
#' Optimal rounding under integer constraints, as proposed by
#' \insertCite{Cont;textual}{stratallo}.
#'
#' @references
#' \insertRef{Cont}{stratallo}
#'
#' @export
#'
#' @examples
#' round_oric(x) # 4 4 5
#'
round_oric <- function(x) {
  m <- floor(x)
  y <- x - m
  Ix <- sum(y)

  if (Ix == 0) {
    as.integer(x)
  } else {
    iy <- order(-y)
    u <- unique(y[iy])
    z <- integer(length(x))
    for (i in seq_along(u)) z[iy] <- z[iy] + (y[iy] == u[i]) * i
    iy2 <- order(-y, z, -m)
    # m[iy][iy2][1:Ix] <- ceiling(x[iy][iy2][1:Ix])
    m[iy2][1:Ix] <- (m[iy2][1:Ix]) + 1
    as.integer(m)
  }
}
