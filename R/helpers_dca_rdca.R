#' @name helpers_dca_rdca
#'
#' @title Internal Helper Functions for `dca0()`, `dca()`, and `rdca()`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Internal utility functions used by [dca0()], [dca()], and [rdca()] that
#' perform operations on sets of domain–strata indices and manage the mapping
#' between strata and domains.
#'
#' @note These functions are internal and should typically not be called
#'   directly by users.
#'
#' @param H_counts (`integerish`)\cr strata counts in each domain.
#'
#' @examples
#' H_counts <- c(2, 2, 3) # three domains with 2, 2, and 3 strata respectively
#'
NULL

#' @describeIn helpers_dca_rdca
#' Creates a vector of domain indicators from a vector of strata counts per
#' domain;
#' each element of the vector is the index of the domain to which the
#' corresponding stratum belongs.
#'
#' @examples
#' # internal functions (not exported) – examples skipped
#' \dontrun{
#' H_cnt2dind(H_counts) # 1 1 2 2 3 3 3
#' }
#'
H_cnt2dind <- function(H_counts) {
  # if (is.list(H_counts)) {
  #   # # Using a list input
  #   # H_counts_list <- list(a = 1:2, b = 1:2, c = 1:3)
  #   # H_cnt2dind(H_counts_list) # 1 1 2 2 3 3 3
  #   rep(seq_along(H_counts), times = sapply(H_counts, length))
  # } else {
  rep(seq_along(H_counts), times = H_counts)
  # }
}

#' @describeIn helpers_dca_rdca
#' Creates unique indices for strata across multiple domains.
#' Returns a `list` of integer vectors, where the \eqn{d}-th element contains
#' the unique indices of the strata in domain \eqn{d}.
#'
#' @examples
#' # internal functions (not exported) – examples skipped
#' \dontrun{
#' H_cnt2glbidx(H_counts)
#' }
#'
H_cnt2glbidx <- function(H_counts) {
  mapply(
    function(to, len) seq.int(to = to, length.out = len, by = 1L),
    cumsum(H_counts), H_counts,
    SIMPLIFY = FALSE
  )
}

#' @describeIn helpers_dca_rdca
#' Get the globally unique indices of strata belonging to a specific domain.
#'
#' @param d (`integerish(1)`) domain index. Must satisfy
#'   `0 < d <= length(H_counts)`.
#'
#' @examples
#' # internal functions (not exported) – examples skipped
#' \dontrun{
#' H_get_strata_indices(H_counts, 3) # 5 6 7
#' }
#'
H_get_strata_indices <- function(H_counts, d) {
  H_dind <- H_cnt2dind(H_counts)
  which(H_dind == d)
}
