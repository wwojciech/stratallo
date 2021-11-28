
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stratallo

<!-- badges: start -->

<!-- badges: end -->

The goal of stratallo is to provide methods computing solution to
classical problem in survey methodology - an optimum sample allocation
problem in stratified sampling scheme with simple random sampling
without replacement design in each stratum. In this context, the optimal
allocation is in the classical Tschuprow-Neyman’s sense (Tschuprow 1923;
Neyman 1934), and it satisfies additional upper bounds restrictions
imposed on sample sizes in strata. There are four different algorithms
available to use, and one of them is Neyman optimal allocation applied
in a recursive way (see Sarndal, Swensson, and Wretman 1992, Remark
12.7.1, p. 466). All the algorithms are described in detail, including
proofs in Wojciak (2019).

## Installation

You can install the released version of stratallo from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("stratallo")
```

## Example

This is a basic example which shows you how to solve a common problem of
optimal sample allocation for an example population with 4 strata.

``` r
library(stratallo)

N <- c(3000, 4000, 5000, 2000) # strata sizes
S <- c(48, 79, 76, 17) # standard deviations of a study variable in strata
M <- c(100, 90, 70, 80) # upper bounds constraints imposed on the sample sizes in strata
all(M <= N) 
#> [1] TRUE
n <- 190 # total sample size
n <= sum(M)
#> [1] TRUE

nopt_ <- nopt(n = n, N = N, S = S, M = M)
nopt_
#> [1] 34.979757 76.761134 70.000000  8.259109
sum(nopt_)
#> [1] 190

# verbose output + variance for a given allocation
nopt(n = n, N = N, S = S, M = M, verbose = TRUE, variance = TRUE)
#> $nopt
#> [1] 34.979757 76.761134 70.000000  8.259109
#> 
#> $ksi
#> [1] 0.000242915
#> 
#> $J
#> [1] 3 2 1 4
#> 
#> $J_
#> [1] 2 1 4
#> 
#> $var
#> [1] 4035156476
```

# References

<div id="refs" class="references">

<div id="ref-Neyman1934">

Neyman, Jerzy. 1934. “On the Two Different Aspects of the Representative
Method: The Method of Stratified Sampling and the Method of Purposive
Selection.” *Journal of the Royal Statistical Society* 97 (4): 558–606.

</div>

<div id="ref-SarndalSwensson1993">

Sarndal, Carl-Erik, Bengt Swensson, and Jan Wretman. 1992. *Model
Assisted Survey Sampling*. Springer.

</div>

<div id="ref-Tschuprow1923">

Tschuprow, Alexander Alexandrovich. 1923. “On the Mathematical
Expectation of the Moments of Frequency Distributions in the Case of
Correlated Observations.” *Metron* 2: 461–93, 646–83.

</div>

<div id="ref-wojciak2019">

Wojciak, Wojciech. 2019. “Optimal Allocation in Stratified Sampling
Schemes.” *Master’s Diploma Thesis*. Warsaw University of Technology.
<http://home.elka.pw.edu.pl/~wwojciak/msc_optimal_allocation.pdf>.

</div>

</div>
