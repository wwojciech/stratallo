#' @name data
#' @docType data
#'
#' @title Datasets with Example Populations
#'
#' @description `r lifecycle::badge("stable")`
#'
#' Example datasets containing artificial populations for testing and
#' demonstrating optimum sample allocation algorithms.
#'
NULL

#' @rdname data
#'
#' @format pop10s_bounds_ucost: Population with 10 strata, lower and upper bounds
#'   on sample sizes, and associated surveying costs.
#'   A matrix with 10 rows and 5 variables:
#' \describe{
#'   \item{N}{stratum size}
#'   \item{S}{standard deviation of study variable in the stratum.}
#'   \item{m}{lower bound for sample size in the stratum.}
#'   \item{M}{upper bound for sample size in the stratum.}
#'   \item{unit_cost}{cost of surveying one element in the stratum.}
#' }
#'
"pop10s_bounds_ucost"

#' @rdname data
#'
#' @format pop507s_ucost: Population with 507 strata and associated surveying costs.
#'   A matrix with 507 rows and 3 columns:
#' \describe{
#'   \item{N}{stratum size.}
#'   \item{S}{standard deviation of study variable in the stratum.}
#'   \item{unit_cost}{cost of surveying one element in the stratum.}
#' }
#'
"pop507s_ucost"

#' @rdname data
#'
#' @format pop969s_ucost: Population with 969 strata and associated surveying costs.
#'   A matrix with 969 rows and 3 columns:
#' \describe{
#'   \item{N}{stratum size.}
#'   \item{S}{standard deviation of study variable in the stratum.}
#'   \item{unit_cost}{cost of surveying one element in the stratum.}
#' }
#'
"pop969s_ucost"

#' @rdname data
#'
#' @format pop2d4s: Population with 2 domains and 4 strata.
#'   A list with the following elements:
#' \describe{
#'   \item{H_counts}{strata counts in each domain.}
#'   \item{N}{stratum sizes.}
#'   \item{S}{standard deviations of study variable in strata.}
#'   \item{total}{totals in domains, i.e., the sum of the study variable values
#'   for population elements in each domain.}
#'   \item{kappa}{priority weights for domains.}
#'   \item{rho}{`total * sqrt(kappa)`.}
#'   \item{rho2}{`total^2 * kappa`.}
#'   \item{n_max}{See [dca_nmax()] or [dca()].}
#' }
#'
"pop2d4s"

#' @rdname data
#'
#' @format pop9d278s: Population with 9 domains and 278 strata.
#'   A list with the following elements:
#' \describe{
#'   \item{H_counts}{strata counts in each domain.}
#'   \item{N}{stratum sizes.}
#'   \item{S}{standard deviations of study variable in strata.}
#'   \item{total}{totals in domains, i.e., the sum of the study variable values
#'   for population elements in each domain.}
#'   \item{kappa}{priority weights for domains.}
#'   \item{rho}{`total * sqrt(kappa)`.}
#'   \item{rho2}{`total^2 * kappa`.}
#'   \item{n_max}{See [dca_nmax()] or [dca()].}
#' }
#'
"pop9d278s"
