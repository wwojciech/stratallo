# Fixed-Point Iteration Algorithm

**\[experimental\]**

Algorithm for optimum sample allocation in stratified sampling under
lower- and upper-bound constraints, based on fixed-point iteration.

## Usage

``` r
fpia2(v0, Nh, Sh, mh = NULL, Mh = NULL, lambda0 = NULL, maxiter = 100)

glambda(lambda, n, Ah, mh = NULL, Mh = NULL)

philambda(lambda, n, Ah, mh = NULL, Mh = NULL)
```

## Arguments

- v0:

  variance

- mh:

  (`numeric` or `NULL`)  
  lower bounds on stratum sample sizes (optional).

- Mh:

  (`numeric` or `NULL`)  
  upper bounds on stratum sample sizes (optional).

- lambda0:

  (`numeric(1)`)  
  initial value of the parameter \\\lambda\\ (optional).

- maxiter:

  (`integerish(1)`)  
  maximum number of iterations.

- lambda:

  (`numeric(1)`)  
  \\\lambda\\.

- tol:

  (`numeric(1)`)  
  desired convergence tolerance.

## Value

A list with elements:

- nh:

  Vector of optimal allocation sizes.

- iter:

  Number of iterations performed.

## Functions

- `fpia2()`: Variant of `fpia()` using variance-based parametrization.

- `glambda()`: Helper function for the `fpia()`

- `philambda()`: Helper function for the `fpia()`.

## References

Münnich RT, Sachs EW, Wagner M (2012). “Numerical solution of optimal
allocation problems in stratified sampling under box constraints.” *AStA
Advances in Statistical Analysis*, **96**(3), 435–450.
[doi:10.1007/s10182-011-0176-z](https://doi.org/10.1007/s10182-011-0176-z)
.
