# Check for Mixed Signs in a Numeric Vector

**\[stable\]**

Determines whether a numeric vector contains both negative and positive
values. Zero (`0`) is treated as neutral and does not count as either
sign.

## Usage

``` r
has_mixed_signs(x)
```

## Arguments

- x:

  (`numeric`)  
  a vector to check.

## Value

`TRUE` if the vector contains both positive and negative values, `FALSE`
otherwise.

## Examples

``` r
# internal functions (not exported) – examples skipped
if (FALSE) { # \dontrun{
has_mixed_signs(1:5)
has_mixed_signs(-(1:5))
has_mixed_signs(c(-1, -2, 3))
has_mixed_signs(c(0, -1))
has_mixed_signs(c(0, 1))
has_mixed_signs(c(0, 1, -1))
} # }
```
