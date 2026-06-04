# Datasets with Example Populations

**\[stable\]**

Example datasets containing artificial populations for testing and
demonstrating optimum sample allocation algorithms.

## Usage

``` r
pop10s_bounds_ucost

pop507s_ucost

pop969s_ucost

pop2d4s

pop9d278s
```

## Format

pop10s_bounds_ucost: Population with 10 strata, lower and upper bounds
on sample sizes, and associated surveying costs. A matrix with 10 rows
and 5 variables:

- N:

  stratum size

- S:

  standard deviation of study variable in the stratum.

- m:

  lower bound for sample size in the stratum.

- M:

  upper bound for sample size in the stratum.

- unit_cost:

  cost of surveying one element in the stratum.

pop507s_ucost: Population with 507 strata and associated surveying
costs. A matrix with 507 rows and 3 columns:

- N:

  stratum size.

- S:

  standard deviation of study variable in the stratum.

- unit_cost:

  cost of surveying one element in the stratum.

pop969s_ucost: Population with 969 strata and associated surveying
costs. A matrix with 969 rows and 3 columns:

- N:

  stratum size.

- S:

  standard deviation of study variable in the stratum.

- unit_cost:

  cost of surveying one element in the stratum.

pop2d4s: Population with 2 domains and 4 strata. A list with the
following elements:

- H_counts:

  strata counts in each domain.

- N:

  stratum sizes.

- S:

  standard deviations of study variable in strata.

- total:

  totals in domains, i.e., the sum of the study variable values for
  population elements in each domain.

- kappa:

  priority weights for domains.

- rho:

  `total * sqrt(kappa)`.

- rho2:

  `total^2 * kappa`.

- n_max:

  See [`dca_nmax()`](https://wwojciech.github.io/junco/reference/dca.md)
  or [`dca()`](https://wwojciech.github.io/junco/reference/dca.md).

pop9d278s: Population with 9 domains and 278 strata. A list with the
following elements:

- H_counts:

  strata counts in each domain.

- N:

  stratum sizes.

- S:

  standard deviations of study variable in strata.

- total:

  totals in domains, i.e., the sum of the study variable values for
  population elements in each domain.

- kappa:

  priority weights for domains.

- rho:

  `total * sqrt(kappa)`.

- rho2:

  `total^2 * kappa`.

- n_max:

  See [`dca_nmax()`](https://wwojciech.github.io/junco/reference/dca.md)
  or [`dca()`](https://wwojciech.github.io/junco/reference/dca.md).
