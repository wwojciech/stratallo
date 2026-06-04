# Algorithms for Optimum Sample Allocation Under One-Sided Bound Constraints

**\[stable\]**

Functions implementing selected optimum allocation algorithms for
solving the optimum sample allocation problem, formulated as follows:

Minimize \$\$f(x_1,\ldots,x_H) = \sum\_{h=1}^H \frac{A^2_h}{x_h}\$\$
over \\\mathbb R\_+^H\\, subject to \$\$\sum\_{h=1}^H c_h x_h = c,\$\$
and either \$\$x_h \leq M_h, \qquad h = 1,\ldots,H,\$\$ or \$\$x_h \geq
m_h, \qquad h = 1,\ldots,H,\$\$ where \\c \> 0,\\ c_h \> 0,\\ A_h \>
0,\\ m_h \> 0,\\ M_h \> 0,\\ h = 1,\ldots,H\\, are given numbers.

The following is a list of all available algorithms along with the
functions that implement them:

- RNA - `rna()`,

- LRNA - `rna()`,

- SGA - `sga()`,

- SGAPLUS - `sgaplus()`,

- COMA - `coma()`.

See the documentation of a specific function for details.

The inequality constraints are optional. The user can choose whether and
how they are imposed in the optimization problem, depending on the
chosen algorithm:

- Lower bounds \\m_1, \ldots, m_H\\ can be specified only for the LRNA
  algorithm (by setting `cmp = .Primitive("<=")` for `rna()`).

- Upper bounds \\M_1, \ldots, M_H\\ are supported by all other
  algorithms.

- Simultaneous constraints (both lower and upper bounds) are not
  supported by these functions.

The costs \\c_1, \ldots, c_H\\ of surveying one element in a stratum can
be specified by the user only for the RNA and LRNA algorithms. For the
remaining algorithms, these costs are fixed at 1, i.e., \\c_h = 1,\\ h =
1,\ldots,H\\.

## Usage

``` r
rna(
  total_cost,
  A,
  bounds = NULL,
  unit_costs = 1,
  cmp = .Primitive(">="),
  details = FALSE
)

sga(total_cost, A, M)

sgaplus(total_cost, A, M)

coma(total_cost, A, M)
```

## Arguments

- total_cost:

  (`numeric(1)`)  
  total survey cost \\c\\. Must be strictly positive. Additionally:

  - If one-sided lower bounds \\m_1, \ldots, m_H\\ are imposed, it is
    required that \\c \geq \sum\_{h=1}^H c_h m_h\\, i.e.
    `total_cost >= sum(unit_costs * bounds)`.

  - If one-sided upper bounds \\M_1, \ldots, M_H\\ are imposed, it is
    required that \\c \leq \sum\_{h=1}^H c_h M_h\\, i.e.
    `total_cost <= sum(unit_costs * bounds)`.

- A:

  (`numeric`)  
  population constants \\A_1,\ldots,A_H\\. All values must be strictly
  positive.

- bounds:

  (`numeric` or `NULL`)  
  optional lower bounds \\m_1,\ldots,m_H\\, or upper bounds
  \\M_1,\ldots,M_H\\, or `NULL` to indicate that no inequality
  constraints are imposed. If not `NULL`, `bounds` is interpreted as:

  - lower bounds, if `cmp = .Primitive("<=")`, or

  - upper bounds, if `cmp = .Primitive(">=")`.

  See also `total_cost`.

- unit_costs:

  (`numeric`)  
  costs \\c_1,\ldots,c_H\\ of surveying one element in each stratum.
  Strictly positive values. May also be of length 1, in which case the
  value is recycled to match the length of `bounds`.

- cmp:

  (`function`)  
  a binary comparison operator used to check for violations of `bounds`.
  Must be either `.Primitive("<=")` (treating `bounds` as lower bounds
  and invoking the LRNA algorithm) or `.Primitive(">=")` (treating
  `bounds` as upper bounds and invoking the RNA algorithm).

  The value of this argument has no effect if `bounds` is `NULL`.

- details:

  (`logical(1)`)  
  should detailed information on stratum assignments (either take-Neyman
  or take-bound), values of the set function \\s\\, and the number of
  iterations be included in the output?

- M:

  (`numeric` or `NULL`)  
  upper bounds \\M_1,\ldots,M_H\\ optionally imposed on sample sizes in
  strata. Set to `NULL` if no upper bounds are imposed. Otherwise, it is
  required that `total_cost <= sum(unit_costs * M)`.

## Value

A numeric vector of optimum sample allocations in strata. In the case of
`rna()` only, the return value may also be a
[`list`](https://rdrr.io/r/base/list.html) containing the optimum
allocations and strata assignments.

## Details

If no inequality constraints are imposed, the allocation is given by the
Neyman allocation: \$\$x_h = \frac{A_h}{\sqrt{c_h}}
\frac{c}{\sum\_{i=1}^H A_i \sqrt{c_i}}, \qquad h = 1,\ldots,H.\$\$

For the *stratified \\\pi\\-estimator* of the population total under
*stratified simple random sampling without replacement* design, the
parameters of the objective function \\f\\ are \$\$A_h = N_h S_h, \qquad
h = 1,\ldots,H,\$\$ where \\N_h\\ denotes the size of stratum \\h\\ and
\\S_h\\ is the standard deviation of the study variable in stratum
\\h\\.

## Functions

- `rna()`: Implements the Recursive Neyman Algorithm (RNA) and its
  counterpart, the Lower Recursive Neyman Algorithm (LRNA), designed for
  the optimum allocation problem with one-sided lower-bound constraints.
  The RNA is described in Wesołowski et al. (2022) , whereas the LRNA is
  introduced in Wójciak (2023) .

- `sga()`: The Stenger-Gabler (SGA) algorithm, as proposed by Stenger
  and Gabler (2005) and described in Wesołowski et al. (2022) . This
  algorithm solves the optimum allocation problem with one-sided
  upper-bound constraints. It assumes unit costs are constant and equal
  to 1, i.e., \\c_h = 1,\\ h = 1,\ldots,H\\.

- `sgaplus()`: A modified Stenger-Gabler-type algorithm, described in
  Wójciak (2019) , implemented as the Sequential Allocation (version 1)
  algorithm. This algorithm solves the optimum allocation problem with
  one-sided upper-bound constraints. It assumes unit costs are constant
  and equal to 1, i.e., \\c_h = 1,\\ h = 1,\ldots,H\\.

- `coma()`: The Change of Monotonicity Algorithm (COMA), described in
  Wesołowski et al. (2022) , solves the optimum allocation problem with
  one-sided upper-bound constraints. It assumes unit costs are constant
  and equal to 1, i.e., \\c_h = 1,\\ h = 1,\ldots,H\\.

## Note

These functions are optimized for internal use and should typically not
be called directly by users. Use
[`opt()`](https://wwojciech.github.io/junco/reference/opt.md) or
[`optcost()`](https://wwojciech.github.io/junco/reference/optcost.md)
instead.

## References

Wójciak W (2023). “Another Solution for Some Optimum Allocation
Problem.” *Statistics in Transition new series*, **24**(5), 203-219.
[doi:10.59170/stattrans-2023-071](https://doi.org/10.59170/stattrans-2023-071)
.

Wesołowski J, Wieczorkowski R, Wójciak W (2022). “Optimality of the
Recursive Neyman Allocation.” *Journal of Survey Statistics and
Methodology*, **10**(5), 1263-1275. ISSN 2325-0984.
[doi:10.1093/jssam/smab018](https://doi.org/10.1093/jssam/smab018) .

Wójciak W (2019). *Optimal Allocation in Stratified Sampling Schemes*.
Master's thesis, Warsaw University of Technology.
<http://home.elka.pw.edu.pl/~wwojciak/msc_wwojciech_optimum_alloc.pdf>.

Stenger H, Gabler S (2005). “Combining random sampling and census
strategies - Justification of inclusion probabilities equal to 1.”
*Metrika*, **61**(2), 137–156.
[doi:10.1007/s001840400328](https://doi.org/10.1007/s001840400328) .

Särndal C, Swensson B, Wretman J (1992). *Model Assisted Survey
Sampling*. Springer New York, NY. ISBN 978-0-387-40620-6.

## See also

[`opt()`](https://wwojciech.github.io/junco/reference/opt.md),
[`optcost()`](https://wwojciech.github.io/junco/reference/optcost.md),
[`rnabox()`](https://wwojciech.github.io/junco/reference/rnabox.md).

## Examples

``` r
A <- c(3000, 4000, 5000, 2000)
m <- c(50, 40, 10, 30) # lower bounds
M <- c(100, 90, 70, 80) # upper bounds

rna(total_cost = 190, A = A, bounds = M)
#> [1] 40.71429 54.28571 67.85714 27.14286

rna(total_cost = 190, A = A, bounds = m, cmp = .Primitive("<="))
#> [1] 50.00000 48.88889 61.11111 30.00000

rna(total_cost = 300, A = A, bounds = M)
#> [1] 84 90 70 56

sga(total_cost = 190, A = A, M = M)
#> [1] 40.71429 54.28571 67.85714 27.14286

sgaplus(total_cost = 190, A = A, M = M)
#> [1] 40.71429 54.28571 67.85714 27.14286

coma(total_cost = 190, A = A, M = M)
#> [1] 40.71429 54.28571 67.85714 27.14286
```
