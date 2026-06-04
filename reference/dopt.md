# Multi-Domain Optimum Sample Allocation with Controlled-Precision under Upper-Bound Constraints

**\[stable\]**

Computes the optimum allocation for the following multi-domain optimum
allocation problem, formulated in mathematical optimization terms:

Minimize \$\$f(T,\\ \boldsymbol x) = T\$\$ over \\\mathbb R \times
\mathbb R\_+^{\lvert \mathcal H \rvert}\\, subject to \$\$\sum\_{(d,h)
\in \mathcal H} x\_{d,h} = n,\$\$ \$\$\sum\_{h \in \mathcal H_d}
(\frac{1}{x\_{d,h}} - \frac{1}{N\_{d,h}}) \frac{N\_{d,h}^2
S\_{d,h}^2}{\rho_d^2} = T, \qquad d \in \mathcal D,\$\$ \$\$x\_{d,h}
\leq N\_{d,h}, \qquad (d,h) \in \mathcal H,\$\$ where:

- \\(T,\\ \boldsymbol x) = (T,\\ (x\_{d,h},\\ (d,h) \in \mathcal H))\\:

  the optimization variable,

- \\\mathcal H \subset \mathbb N^2\\:

  the set of domain-stratum indices,

- \\\mathcal D := \\d \in \mathbb N \colon\\ \exists h,\\ (d,h) \in
  \mathcal H\\\\:

  the set of domain indices,

- \\\mathcal H_d := \\h \in \mathbb N \colon\\ (d,h) \in \mathcal H\\\\:

  the set of strata indices in domain \\d\\,

- \\N\_{d,h} \> 0\\:

  size of stratum \\(d,h)\\,

- \\S\_{d,h} \> 0\\:

  standard deviation of the study variable in stratum \\(d,h)\\,

- \\\rho_d := t_d\\ \sqrt{\kappa_d}\\:

  where \\t_d\\ denotes the total in domain \\d\\, i.e., the sum of the
  values of the study variable for population elements in domain \\d\\,
  and \\\kappa_d\\ is a priority weight for domain \\d\\,

- \\n \in (0,\\ \sum\_{(d,h) \in \mathcal H} N\_{d,h}\]\\:

  total sample size.

## Usage

``` r
dopt(n, H_counts, N, S, total, kappa, return_T = FALSE)
```

## Arguments

- n:

  (`integerish(1)`)  
  total sample size \\n\\. Must satisfy `0 < n <= sum(N)`.

- H_counts:

  (`integerish`)  
  strata counts in each domain.

- N:

  (`integerish`)  
  strata sizes \\(N\_{d,h},\\ (d,h) \in \mathcal H)\\.

- S:

  (`numeric`)  
  standard deviations \\(S\_{d,h},\\ (d,h) \in \mathcal H)\\ of surveyed
  variable in strata.

- total:

  (`numeric`)  
  vector of domain totals, \\t_d,\\ d \in \mathcal D\\, i.e., the sum of
  the study variable over all population elements in each domain.

- kappa:

  (`numeric`)  
  vector of priority weights for the domains, \\\kappa_d,\\ d \in
  \mathcal D\\.

- return_T:

  (`logical(1)`)  
  If `TRUE`, the function returns a list containing the optimal
  allocation and the optimal value of the objective function \\T\\. If
  `FALSE` (default), only the optimal allocation vector is returned.

## Value

If `return_T = FALSE` (default), a numeric vector containing the optimal
sample allocations \\x\_{d,h}\\ for each stratum \\(d,h) \in \mathcal
H\\.

If `return_T = TRUE`, a list with components:

- xopt:

  numeric vector of optimal sample allocations.

- Topt:

  optimal value of the objective function \\T\\.

## Details

The `dopt()` function uses the RDCA algorithm implemented in
[`rdca()`](https://wwojciech.github.io/junco/reference/rdca.md).

## References

Wójciak W (2026). *Multi-Domain Optimum Sample Allocation with
Controlled-Precision under Upper-Bound Constraints*. Ph.D. thesis,
Warsaw University of Technology.
<http://home.elka.pw.edu.pl/~wwojciak/phd_wwojciech_optimum_alloc.pdf>.

## See also

[`rdca()`](https://wwojciech.github.io/junco/reference/rdca.md),
[`dca()`](https://wwojciech.github.io/junco/reference/dca.md),
[`dca_nmax()`](https://wwojciech.github.io/junco/reference/dca.md),
[`opt()`](https://wwojciech.github.io/junco/reference/opt.md),
[`optcost()`](https://wwojciech.github.io/junco/reference/optcost.md).

## Examples

``` r

# Three domains with 2, 2, and 3 strata, respectively,
# that is, H = {(1,1), (1,2), (2,1), (2,2), (3,1), (3,2), (3,3)}.
H_counts <- c(2, 2, 3)
# (N_{1,1}, N_{1,2}, N_{2,1}, N_{2,2}, N_{3,1}, N_{3,2}, N_{3,3})
N <- c(140, 110, 135, 190, 200, 40, 70)
# (S_{1,1}, S_{1,2}, S_{2,1}, S_{2,2}, S_{3,1}, S_{3,2}, S_{3,3})
S <- c(180, 20, 5, 4, 35, 9, 40)
total <- c(2, 3, 5)
kappa <- c(0.5, 0.2, 0.3)
n <- 828

# Optimum allocation.
dopt(n, H_counts, N, S, total, kappa)
#> [1] 140.00000 108.06261 135.00000 154.02807 200.00000  20.90933  70.00000

# Example population with 9 domains and 278 strata
p <- pop9d278s
sum(p$N)
#> [1] 44060
n <- 5000
x <- dopt(n, p$H_counts, p$N, p$S, p$total, p$kappa, return_T = TRUE)
x
#> $xopt
#>   [1]   0.39071781   0.20838283   0.62514850   0.29955032   0.31257425
#>   [6]   0.31257425   1.30239271   0.18233498   0.13023927   2.86526395
#>  [11]   1.04191417   0.24745461   0.39071781   4.06346524   3.64281587
#>  [16]  13.11413715   8.65168770  80.00000000   1.82140794   2.91425270
#>  [21]   2.09461913   6.55706857   1.54819675   1.63926714   4.18923826
#>  [26]   1.63926714   4.91780143   3.00532310   4.00709746   1.36605595
#>  [31]   1.45712635   6.37492778   1.54819675   2.91425270   2.54997111
#>  [36]   2.73211191   1.82140794   6.92135016   2.00354873   1.82140794
#>  [41]   1.91247833   1.36605595   2.54997111   1.27498556  10.19988445
#>  [46]   3.82495667   4.37137905   2.36783032  45.89948002   0.72856317
#>  [51]   2.54861274   6.46955542   2.94070701   6.27350829  17.64424207
#>  [56]   7.44979109   9.41026244   1.96047134   4.31303695   2.74465988
#>  [61]   2.35256561   8.62607390   5.48931975   3.33280128  16.66400640
#>  [66]  18.03633633   2.74465988   7.44979109   3.52884841   3.52884841
#>  [71]   3.13675415   4.11698982   6.46955542  17.05610066   7.05769683
#>  [76]   1.56837707   2.94070701   0.70496746   1.71206383   0.80567710
#>  [81]   0.65461264   1.61135419   0.70496746   2.21561202   1.10780601
#>  [86]   0.85603192   3.62554694   1.91348311   4.22980476   0.80567710
#>  [91]   5.63973968   2.01419274   3.22270839  12.43764019   3.02128912
#>  [96]   0.45319337   1.96383792   1.66170901   2.76951502   2.21561202
#> [101]   9.81918962   0.80567710   9.06386735   3.02128912   0.60425782
#> [106]   0.70496746   0.55390300   1.51064456   0.95674155   0.50354819
#> [111]   2.01419274   1.25887046   0.30212891   6.79790051   1.81277347
#> [116]   1.15816083   1.51064456   1.48266059   4.44798178   2.81705512
#> [121]   2.37225695   0.88959636   1.55679362   2.96532118   2.96532118
#> [126]   1.55679362   4.74451389   1.48266059   1.77919271   3.55838542
#> [131]   1.77919271   1.70505968   3.70665148   1.77919271   1.92745877
#> [136]   0.88959636   9.78555991   2.44638998   2.89118815   1.77919271
#> [141]   1.55679362   2.96532118   4.00318360  14.08527562   2.22399089
#> [146]   5.33757813   6.82023872   0.81546333   5.11517904   5.33757813
#> [151]   1.03786241   4.00318360   0.31944915   8.10909381   0.44231421
#> [156]   0.63889830   0.88462842   0.49146023   1.27779660   0.98292046
#> [161]   0.27030313   0.39316818   2.40815513   0.54060625   1.59724575
#> [166]   0.27030313   0.51603324   0.88462842   2.55559320   1.37608865
#> [171]   0.93377444   2.35900911   3.09619945   0.41774120   1.40066166
#> [176]  11.20529326   0.39316818   1.69553780   0.39316818   0.58975228
#> [181]   2.50644718  12.71689299  12.35355319   4.36007760   2.54337860
#> [186]   6.29788986   4.36007760 250.00000000   6.90345620   4.60230413
#> [191]   5.81343680   3.14894493  43.60077598   1.45335920  81.38811516
#> [196]   6.17677660   9.68906133   3.14894493   5.32898373   4.60230413
#> [201]   1.69558573   1.69558573   3.63339800   2.05892553   4.11785106
#> [206]   2.66449187   2.66449187   2.30115207   0.35254681   0.17093179
#> [211]   4.48695938   0.21366473   1.28198839   0.09614913   0.51279536
#> [216]   0.12819884   0.19229826   0.17093179   0.68372714   0.76919304
#> [221]   0.23503121   0.12819884   3.41863572   0.94012482   0.16024855
#> [226]   0.38459652   0.13888208   0.83329246   0.30981386   0.23503121
#> [231]   0.27776415   0.53416183   0.27776415   0.23503121   0.94012482
#> [236]   1.08969014   1.11105661   0.11751560  46.31008018 101.88217639
#> [241]  76.41163229  32.41705612  41.67907216  69.46512026  87.98915233
#> [246] 257.02094497  55.57209621  34.73256013  41.67907216  55.57209621
#> [251] 130.00000000 120.40620846  25.47054410 145.87675255  43.99457617
#> [256]  46.31008018  39.36356815 157.45427260  30.10155211  97.25116837
#> [261]  34.73256013  90.30465634  87.98915233 129.66822449 260.00000000
#> [266]  37.04806414  48.62558418 350.00000000  16.20852806  64.83411225
#> [271] 125.03721647  74.09612828 129.66822449  46.31008018 120.00000000
#> [276]  74.09612828 208.39536079 160.00000000
#> 
#> $Topt
#> [1] 1112122
#> 
all(x$xopt <= p$N)
#> [1] TRUE
sum(x$xopt)
#> [1] 5000
```
