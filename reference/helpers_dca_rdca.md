# Internal Helper Functions for `dca0()`, `dca()`, and `rdca()`

**\[stable\]**

Internal utility functions used by
[`dca0()`](https://wwojciech.github.io/junco/reference/dca.md),
[`dca()`](https://wwojciech.github.io/junco/reference/dca.md), and
[`rdca()`](https://wwojciech.github.io/junco/reference/rdca.md) that
perform operations on sets of domain–strata indices and manage the
mapping between strata and domains.

## Usage

``` r
H_cnt2dind(H_counts)

H_cnt2glbidx(H_counts)

H_get_strata_indices(H_counts, d)
```

## Arguments

- H_counts:

  (`integerish`)  
  strata counts in each domain.

- d:

  (`integerish(1)`) domain index. Must satisfy
  `0 < d <= length(H_counts)`.

## Functions

- `H_cnt2dind()`: Creates a vector of domain indicators from a vector of
  strata counts per domain; each element of the vector is the index of
  the domain to which the corresponding stratum belongs.

- `H_cnt2glbidx()`: Creates unique indices for strata across multiple
  domains. Returns a `list` of integer vectors, where the \\d\\-th
  element contains the unique indices of the strata in domain \\d\\.

- `H_get_strata_indices()`: Get the globally unique indices of strata
  belonging to a specific domain.

## Note

These functions are internal and should typically not be called directly
by users.

## Examples

``` r
H_counts <- c(2, 2, 3) # three domains with 2, 2, and 3 strata respectively

# internal functions (not exported) – examples skipped
if (FALSE) { # \dontrun{
H_cnt2dind(H_counts) # 1 1 2 2 3 3 3
} # }

# internal functions (not exported) – examples skipped
if (FALSE) { # \dontrun{
H_cnt2glbidx(H_counts)
} # }

# internal functions (not exported) – examples skipped
if (FALSE) { # \dontrun{
H_get_strata_indices(H_counts, 3) # 5 6 7
} # }
```
