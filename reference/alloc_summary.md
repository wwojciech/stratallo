# Summarizing the Allocation

**\[stable\]**

A utility that returns a simple
[`data.frame`](https://rdrr.io/r/base/data.frame.html) summarizing the
allocation returned by
[`opt()`](https://wwojciech.github.io/junco/reference/opt.md) or
[`optcost()`](https://wwojciech.github.io/junco/reference/optcost.md).

## Usage

``` r
alloc_summary(x, A, m = NULL, M = NULL)
```

## Arguments

- x:

  (`numeric`)  
  sample allocations \\x_1,\ldots,x_H\\.

- A:

  (`numeric`)  
  population constants \\A_1,\ldots,A_H\\.

- m:

  (`numeric` or `NULL`)  
  optional lower bounds \\m_1,\ldots,m_H\\.

- M:

  (`numeric` or `NULL`)  
  optional upper bounds \\M_1,\ldots,M_H\\.

## Value

A [`data.frame`](https://rdrr.io/r/base/data.frame.html) with \\H + 1\\
rows and up to seven variables, where \\H\\ is the number of strata. The
first \\H\\ rows correspond to strata \\h = 1,\ldots,H\\, while the last
row contains column totals (where applicable). The columns include:

- A:

  Population constant \\A_h\\.

- m\*:

  Lower bound \\m_h\\ (if provided).

- M\*:

  Upper bound \\M_h\\ (if provided).

- allocation:

  The optimized sample size \\x_h\\.

- take_min\*:

  Boolean indicator: \\x_h = m_h\\.

- take_max\*:

  Boolean indicator: \\x_h = M_h\\.

- take_Neyman:

  Boolean indicator: \\m_h \< x_h \< M_h\\ (or simply the internal
  Neyman allocation if no bounds were violated).

## See also

[`opt()`](https://wwojciech.github.io/junco/reference/opt.md),
[`optcost()`](https://wwojciech.github.io/junco/reference/optcost.md).

## Examples

``` r
A <- c(3000, 4000, 5000, 2000)
m <- c(100, 90, 70, 80)
M <- c(200, 150, 300, 210)

xopt_1 <- opt(n = 400, A, m)
alloc_summary(xopt_1, A, m)
#>              A   m allocation take_min take_neyman
#> Stratum_1 3000 100  100.00000        *            
#> Stratum_2 4000  90   97.77778                    *
#> Stratum_3 5000  70  122.22222                    *
#> Stratum_4 2000  80   80.00000        *            
#> SUM         NA 340  400.00000        2           2

xopt_2 <- opt(n = 540, A, m, M)
alloc_summary(xopt_2, A, m, M)
#>              A   m   M allocation take_min take_max take_neyman
#> Stratum_1 3000 100 200     116.25                             *
#> Stratum_2 4000  90 150     150.00                 *            
#> Stratum_3 5000  70 300     193.75                             *
#> Stratum_4 2000  80 210      80.00        *                     
#> SUM         NA 340 860     540.00        1        1           2
```
