# Range keys

These functions are helper functions for working with ranged data as
keys in guides. They all share the goal creating of a guide key, but
have different methods:

- `key_range_auto()` is a function factory whose functions make an
  attempt to infer ranges from the scale's labels.

- `key_range_manual()` uses user-provided vectors to set ranges.

- `key_range_map()` makes mappings from a `<data.frame>` to set ranges.

- `key_range_rle()` uses run-length encoding to determine the start and
  end of runs (blocks of repeated data values).

## Usage

``` r
key_range_auto(sep = "[^[:alnum:]]+", reverse = FALSE, ...)

key_range_manual(start, end, name = NULL, level = NULL, ..., call = NULL)

key_range_map(data, ..., call = NULL)

key_range_rle(x, ..., call = NULL)
```

## Arguments

- sep:

  A `<character[1]>` giving a [regular
  expression](https://rdrr.io/r/base/regex.html) to use for splitting
  labels provided by the scale using
  [`strsplit()`](https://rdrr.io/r/base/strsplit.html). Defaults to
  splitting on any non-alphanumeric character.

- reverse:

  A `<logical[1]>` which if `FALSE` (default) treats the first labels as
  the inner labels and the last labels as the outer labels. If `TRUE`,
  thee first labels are treated as the outer labels and the last labels
  are treated as the inner labels.

- ...:

  The `...` parameter has two purposes.

  1.  In `key_range_map()` it is
      [`<data-masking>`](https://rlang.r-lib.org/reference/topic-data-mask.html).
      A set of mappings similar to those provided to
      [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html), which
      will be evaluated in the `data` argument. These *must* contain
      `start` and `end` mappings.

  2.  In other keys, `...` can be used to transfer graphical properties
      to the individual ranges of a guide. For example, using
      `colour = "blue"` will draw parts of the guides associated with
      ranges in blue. There is a shallow hierarchy in that
      `text_colour`, `line_colour`, `rect_colour` and `point_colour` are
      the specific properties for elements, but all inherit from the
      main `colour` setting. Likewise, `size`, `linewidth`, `linetype`
      and `fill` have specific variants for elements.

- start, end:

  A vector that can be interpreted by the scale, giving the start and
  end positions of each range respectively.

- name:

  A `<character>` or list of expressions

- level:

  An `<integer>` giving the depth of each range to avoid overlaps
  between different ranges. When `level` is smaller than 1, no brackets
  are drawn.

- call:

  A [call](https://rlang.r-lib.org/reference/topic-error-call.html) to
  display in messages.

- data:

  A `<data.frame>` or similar object coerced by
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html) to
  a `<data.frame>`, in which the `mapping` argument is evaluated.

- x:

  A `<vector[n]>` for which to determine run-starts and run-ends.

## Value

For `key_range_auto()` a function. For `key_range_manual()` and
`key_range_map()` a `<data.frame>` with the `<key_range>` class.

## Details

The `level` variable is optional and when missing, the guides use an
algorithm similar to `IRanges::disjointBins()` to avoid overlaps.

The `key_range_auto()` does *not* work with expression labels.

## See also

Other keys:
[`key_group`](https://teunbrand.github.io/legendry/reference/key_group.md),
[`key_segments`](https://teunbrand.github.io/legendry/reference/key_segments.md),
[`key_specialty`](https://teunbrand.github.io/legendry/reference/key_specialty.md),
[`key_standard`](https://teunbrand.github.io/legendry/reference/key_standard.md)

## Examples

``` r
# Example scale
template <- scale_x_discrete(limits = c("A 1", "B 1", "C&1", "D&2", "E&2"))

# By default, splits on all non-alphanumeric characters
auto <- key_range_auto()
auto(template)
#>   start end .label .level
#> 1     1   1      A      0
#> 2     2   2      B      0
#> 3     3   3      C      0
#> 4     4   4      D      0
#> 5     5   5      E      0
#> 6     1   3      1      1
#> 7     4   5      2      1

# Only split on a specific character
auto <- key_range_auto(sep = "&")
auto(template)
#> Warning: Not all labels in `key_range_auto()` can be split into equal lengths.
#> ℹ Is "&" the correct `sep` argument?
#>   start end .label .level
#> 1     1   1    A 1      0
#> 2     2   2    B 1      0
#> 3     3   3      C      0
#> 4     4   4      D      0
#> 5     5   5      E      0
#> 6     3   3      1      1
#> 7     4   5      2      1

# Treating the letters as outer labels and numbers as inner labels
auto <- key_range_auto(reverse = TRUE)
auto(template)
#>    start end .label .level
#> 1      1   1      1      0
#> 2      2   2      1      0
#> 3      3   3      1      0
#> 4      4   4      2      0
#> 5      5   5      2      0
#> 6      1   1      A      1
#> 7      2   2      B      1
#> 8      3   3      C      1
#> 9      4   4      D      1
#> 10     5   5      E      1

# Providing custom values
key_range_manual(
  start = c(1, 5,  10),
  end   = c(4, 15, 11),
  level = c(0, 2, 1),
  name  = c("A", "B", "C")
)
#>   start end .label .level
#> 1     1   4      A      0
#> 2     5  15      B      2
#> 3    10  11      C      1

# Values from a <data.frame>
key_range_map(presidential, start = start, end = end, name = name)
#>         start        end     .label
#> 1  1953-01-20 1961-01-20 Eisenhower
#> 2  1961-01-20 1963-11-22    Kennedy
#> 3  1963-11-22 1969-01-20    Johnson
#> 4  1969-01-20 1974-08-09      Nixon
#> 5  1974-08-09 1977-01-20       Ford
#> 6  1977-01-20 1981-01-20     Carter
#> 7  1981-01-20 1989-01-20     Reagan
#> 8  1989-01-20 1993-01-20       Bush
#> 9  1993-01-20 2001-01-20    Clinton
#> 10 2001-01-20 2009-01-20       Bush
#> 11 2009-01-20 2017-01-20      Obama
#> 12 2017-01-20 2021-01-20      Trump

# Values from run length encoding
key_range_rle(c("AB", "AB", "C", "DEF", "DEF", "DEF"))
#>   start end .label .level
#> 1     1   2     AB      1
#> 2     3   3      C      1
#> 3     4   6    DEF      1
```
