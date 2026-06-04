# Minimum-Cost Allocation in Stratified Sampling

**\[stable\]**

Computes stratum sample sizes that minimize the total survey cost for a
given target variance of a stratified estimator, optionally subject to
one-sided upper bounds on the stratum sample sizes. Specifically, the
function solves the following optimization problem:

Minimize \$\$c(x_1,\ldots,x_H) = \sum\_{h=1}^H c_h x_h\$\$ over
\\\mathbb R\_+^H\\, subject to \$\$\sum\_{h=1}^H \frac{A^2_h}{x_h} - A_0
= V,\$\$ \$\$x_h \leq M_h, \qquad h = 1,\ldots,H,\$\$ where \\A_0,\\ A_h
\> 0,\\ c_h \> 0,\\ M_h \> 0,\\ h = 1,\ldots,H\\, and \\V \>
\sum\_{h=1}^H \frac{A^2_h}{M_h} - A_0\\, are given numbers.

The upper-bound constraints \\x_h \leq M_h\\ are optional. If they are
not imposed, it is only required that \\V \> 0\\.

## Usage

``` r
optcost(V, A, A0, M = NULL, unit_costs = 1)
```

## Arguments

- V:

  (`number`)  
  parameter \\V\\ in the variance constraint. If upper bounds are
  imposed (`M` is not `NULL`), it must satisfy `V > sum(A^2/M) - A_0`.
  Otherwise, `V > 0`.

- A:

  (`numeric`)  
  population constants \\A_1,\ldots,A_H\\. All values must be strictly
  positive.

- A0:

  (`number`)  
  population constant \\A_0\\.

- M:

  (`numeric` or `NULL`)  
  optional upper bounds \\M_1,\ldots,M_H\\ on the stratum sample sizes.
  If no upper bounds are imposed, set `M = NULL`.

- unit_costs:

  (`numeric`)  
  costs \\c_1,\ldots,c_H\\ of surveying one element in each stratum.
  Strictly positive values. May also be of length 1, in which case the
  value is recycled to match the length of `bounds`.

## Value

A numeric vector containing the optimal sample allocation for each
stratum.

## Details

The allocation is computed using the **LRNA algorithm**, described in
Wójciak (2023) .

The solution is valid for stratified sampling designs in which the
variance \\V\_{st}\\ of the stratified estimator can be expressed as
\$\$V\_{st} = \sum\_{h=1}^H \frac{A^2_h}{x_h} - A_0,\$\$ where \\H\\ is
the number of strata, \\x_1,\ldots,x_H\\ are the stratum sample sizes,
and \\A_0,\\ A_h \> 0\\ do not depend on \\x_h\\.

## Note

For the *stratified \\\pi\\-estimator* of the population total under
*stratified simple random sampling without replacement* design, the
parameters take the form \$\$A_h = N_h S_h, \qquad h = 1,\ldots,H,\$\$
\$\$A_0 = \sum\_{h=1}^H N_h S_h^2,\$\$ where \\N_h\\ is the size of
stratum \\h\\ and \\S_h\\ is the standard deviation of the study
variable in stratum \\h\\.

## References

Wójciak W (2023). “Another Solution for Some Optimum Allocation
Problem.” *Statistics in Transition new series*, **24**(5), 203-219.
[doi:10.59170/stattrans-2023-071](https://doi.org/10.59170/stattrans-2023-071)
.

## See also

[`rna()`](https://wwojciech.github.io/junco/reference/alg_1sided.md),
[`opt()`](https://wwojciech.github.io/junco/reference/opt.md).

## Examples

``` r
A <- c(3000, 4000, 5000, 2000)
M <- c(100, 90, 70, 80)
x <- optcost(1017579, A = A, A0 = 579, M = M)
x
#> [1] 41.25096 55.00129 68.75161 27.50064
```
