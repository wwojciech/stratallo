# Check Numeric Equality within a Tolerance

**\[stable\]**

Compares two numeric vectors element-wise using an adaptive tolerance
sequence ranging from `10^-19` to `10^tol_max`. The smallest tolerance
at which the values are considered equal is returned as the
corresponding name in the output.

## Usage

``` r
is_equal(x, y, tol_max = -1)
```

## Arguments

- x:

  (`numeric`)  
  first vector to compare.

- y:

  (`numeric`)  
  second vector to compare.

- tol_max:

  (`integerish(1)`)  
  the maximum tolerance exponent (as a power of 10).

## Value

A logical vector indicating whether each element pair is equal within
the detected tolerance. The names reflect the tolerance used.

## Examples

``` r
# internal functions (not exported) – examples skipped
if (FALSE) { # \dontrun{
is_equal(c(3, 4), c(3, 4))
is_equal(c(3, 4), c(3.01, 4.11))
is_equal(c(3, 4), c(3.01, 4.11), tol_max = 0)
} # }
```
