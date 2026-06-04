# Variance of the Stratified \\\pi\\ Estimator of the Population Total

**\[stable\]**

Computes the value of the variance function of the stratified \\\pi\\
estimator of the population total, which has the following generic form:
\$\$V\_{st} = \sum\_{h=1}^H \frac{A_h^2}{x_h} - A_0,\$\$ where \\H\\
denotes the total number of strata, \\x_1,\ldots,x_H\\ are the stratum
sample sizes, and \\A_0\\ and \\A_h \> 0\\, for \\h = 1,\ldots,H\\, are
population constants that do not depend on the \\x_h\\.

## Usage

``` r
var_st(x, A, A0)

var_stsi(x, N, S)
```

## Arguments

- x:

  (`numeric`)  
  sample allocations \\x_1,\ldots,x_H\\.

- A:

  (`numeric`)  
  population constants \\A_1,\ldots,A_H\\.

- A0:

  (`numeric(1)`)  
  population constant \\A_0\\.

- N:

  (`integerish`)  
  strata sizes \\N_1,\ldots,N_H\\.

- S:

  (`numeric`)  
  strata standard deviations of a given study variable
  \\S_1,\ldots,S_H\\.

## Value

The value of the variance \\V\_{st}\\ for a given allocation vector
\\x_1,\ldots,x_H\\.

## Functions

- `var_st()`: The value of the variance \\V\_{st}\\.

- `var_stsi()`: The value of the variance \\V\_{st}\\ for the case of
  *simple random sampling without replacement* design within each
  stratum.

  This particular case yields: \$\$A_h = N_h S_h, \qquad h =
  1,\ldots,H,\$\$ \$\$A_0 = \sum\_{h=1}^H N_h S_h^2,\$\$ where \\N_h\\
  denotes the size of stratum \\h\\ and \\S_h\\ is the corresponding
  stratum standard deviation of the study variable, for \\h =
  1,\ldots,H\\.

## References

Särndal C, Swensson B, Wretman J (1992). *Model Assisted Survey
Sampling*. Springer New York, NY. ISBN 978-0-387-40620-6.

## Examples

``` r
N <- c(300, 400, 500, 200)
S <- c(2, 5, 3, 1)
x <- c(27, 88, 66, 9)
A <- N * S
A0 <- sum(N * S^2)

var_st(x, A, A0)
#> [1] 81423.23

N <- c(3000, 4000, 5000, 2000)
S <- rep(1, 4)
M <- c(100, 90, 70, 80)
x <- opt(n = 320, A = N * S, M = M)

var_stsi(x = x, N, S)
#> [1] 677170.6
```
