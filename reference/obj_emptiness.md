# Object Emptiness (`NULL` or zero-length)

**\[stable\]**

Utility functions for checking whether an object is empty, where
emptiness is defined as being `NULL` or having length 0.

## Usage

``` r
is_empty(x)

is_nonempty(x)
```

## Arguments

- x:

  object to test.

## Functions

- `is_empty()`: Returns `TRUE` if the object is `NULL` or has length 0,
  and `FALSE` otherwise.

- `is_nonempty()`: Logical negation of `is_empty()`. This function
  directly checks if `length(x) > 0L` for performance reasons, avoiding
  the extra negation step that would occur if using `!is_empty(x)`. It
  is optimized for repeated use in algorithms where `is_nonempty()` is
  called many times.

## Examples

``` r
# internal functions (not exported) – examples skipped

if (FALSE) { # \dontrun{
is_empty(NULL)
is_empty(character(0))
is_empty(1)
} # }

if (FALSE) { # \dontrun{
is_nonempty(NULL)
is_nonempty(character(0))
is_nonempty(1)
} # }
```
