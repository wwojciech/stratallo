# Rounding of Numbers

**\[experimental\]**

## Usage

``` r
round_ran(x)

round_oric(x)
```

## Arguments

- x:

  (`numeric`)  
  numeric vector.

## Value

An integer vector.

## Functions

- `round_ran()`: Random rounding of numbers. A number \\x\\ is rounded
  to an integer \\y\\ according to the following rule:

  \$\$y = \left\lfloor x \right\rfloor + I\\\left(u \< x - \left\lfloor
  x \right\rfloor\right),\$\$ where the indicator function \\I :
  \\FALSE,\\ TRUE\\ \to \\0,\\ 1\\\\ is defined as \$\$ I(b) :=
  \begin{cases} 0, & b \text{ is } FALSE, \\ 1, & b \text{ is } TRUE,
  \end{cases} \$\$ and \\u\\ is a random number drawn from the
  \\\mathrm{Uniform}(0, 1)\\ distribution.

- `round_oric()`: Optimal rounding under integer constraints, as
  proposed by Cont and Heidari (2014) .

## References

Cont R, Heidari M (2014). “Optimal rounding under integer constraints.”
1501.00014, <https://arxiv.org/abs/1501.00014>.

## Examples

``` r
x <- c(4.5, 4.1, 4.9)

set.seed(5)
round_ran(x) # 5 4 4
#> [1] 5 4 4

set.seed(6)
round_ran(x) # 4 4 5
#> [1] 4 4 5

round_oric(x) # 4 4 5
#> [1] 4 4 5
```
