# Standard keys

These functions are helper functions for working with tick marks as keys
in guides. They all share the goal of creating a guide key, but have
different outcomes:

- `key_auto()` is a function factory whose functions extract a typical
  key from major breaks in a scale.

- `key_manual()` uses user-provided vectors to make a key.

- `key_map()` makes mappings from a `<data.frame>` to make a key.

- `key_minor()` is a function factory whose functions also extract minor
  break positions for minor tick marks.

- `key_log()` is a function factory whose functions place ticks at
  intervals in log10 space.

- `key_none()` makes an empty key with no entries.

## Usage

``` r
key_auto(..., call = NULL)

key_manual(
  aesthetic,
  value = aesthetic,
  label = as.character(value),
  type = NULL,
  ...,
  call = NULL
)

key_map(data, ..., call = NULL)

key_minor(..., call = NULL)

key_log(
  prescale_base = NULL,
  negative_small = 0.1,
  expanded = TRUE,
  labeller = NULL,
  ...,
  call = NULL
)

key_none()
```

## Arguments

- ...:

  The `...` parameter has two purposes.

  1.  In `key_map()` it is
      [`<data-masking>`](https://rlang.r-lib.org/reference/topic-data-mask.html).
      A set of mappings similar to those provided to
      [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html), which
      will be evaluated in the `data` argument. These must contain
      `aesthetic` mapping.

  2.  In other keys, `...` can be used to transfer graphical properties
      to the individual breaks of a guide. For example, using
      `colour = "blue"` will draw parts of the guides associated with
      breaks in blue. There is a shallow hierarchy in that
      `text_colour`, `line_colour`, `rect_colour` and `point_colour` are
      the specific properties for elements, but all inherit from the
      main `colour` setting. Likewise, `size`, `linewidth`, `linetype`
      and `fill` have specific variants for elements.

- call:

  A [call](https://rlang.r-lib.org/reference/topic-error-call.html) to
  display in messages.

- aesthetic, value:

  A vector of values for the guide to represent equivalent to the
  `breaks` argument in scales. The `aesthetic` will be mapped, whereas
  `value` will not. For most intents and purposes, `aesthetic` and
  `value` should be identical.

- label:

  A `<character>` or list of expressions to use as labels.

- type:

  A `<character>` vector representing the one of the break types:
  `"major"`, `"minor"` or `"mini"`. If `NULL` (default), all breaks are
  treated as major breaks.

- data:

  A `<data.frame>` or similar object coerced by
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html) to
  a `<data.frame>`, in which the `mapping` argument is evaluated.

- prescale_base:

  A `<numeric[1]>` giving the base of logarithm to transform data
  manually. The default, `NULL`, will use the scale transformation to
  calculate positions. It is only advisable to set the `prescale_base`
  argument when the data have already been log-transformed. When using a
  log-transform in the scale or in
  [`coord_transform()`](https://ggplot2.tidyverse.org/reference/coord_transform.html),
  the default `NULL` is recommended.

- negative_small:

  A `<numeric[1]>` setting the smallest absolute value that is marked
  with a tick in case the scale limits include 0 or negative numbers.

- expanded:

  A `<logical[1]>` determining whether the ticks should cover the entire
  range after scale expansion (`TRUE`, default), or be restricted to the
  scale limits (`FALSE`).

- labeller:

  A `<function>` that receives major breaks and returns formatted
  labels. For `key_log()`, `NULL` will default to
  [`scales::label_log()`](https://scales.r-lib.org/reference/label_log.html)
  for strictly positive numbers and a custom labeller when negative
  numbers are included.

## Value

For `key_auto()`, `key_minor()` and `key_log()` a function. For
`key_manual()` and `key_map()` a `<data.frame>` with the
`<key_standard>` class.

## See also

Other keys:
[`key_group`](https://teunbrand.github.io/legendry/reference/key_group.md),
[`key_range`](https://teunbrand.github.io/legendry/reference/key_range.md),
[`key_segments`](https://teunbrand.github.io/legendry/reference/key_segments.md),
[`key_specialty`](https://teunbrand.github.io/legendry/reference/key_specialty.md)

## Examples

``` r
# An example scale
template <- scale_x_continuous(limits = c(0, 10))

# The auto, minor and log keys operate on scales
key_auto()(template)
#>      x .value .label
#> 1  0.0    0.0    0.0
#> 2  2.5    2.5    2.5
#> 3  5.0    5.0    5.0
#> 4  7.5    7.5    7.5
#> 5 10.0   10.0   10.0
key_minor()(template)
#>       x .value .label .type
#> 1  0.00   0.00    0.0 major
#> 2  2.50   2.50    2.5 major
#> 3  5.00   5.00    5.0 major
#> 4  7.50   7.50    7.5 major
#> 5 10.00  10.00   10.0 major
#> 6  1.25   1.25   <NA> minor
#> 7  3.75   3.75   <NA> minor
#> 8  6.25   6.25   <NA> minor
#> 9  8.75   8.75   <NA> minor

# So does the log key
template <- scale_x_continuous(transform = "log10", limits = c(0.1, 10))
key_log()(template)
#>              x      .value .label .type
#> 1  -1.00000000 -1.00000000  10^-1 major
#> 2   0.00000000  0.00000000   10^0 major
#> 3   1.00000000  1.00000000   10^1 major
#> 4  -0.30103000 -0.30103000   NULL minor
#> 5   0.69897000  0.69897000   NULL minor
#> 6  -0.69897000 -0.69897000   NULL  mini
#> 7  -0.52287875 -0.52287875   NULL  mini
#> 8  -0.39794001 -0.39794001   NULL  mini
#> 9  -0.22184875 -0.22184875   NULL  mini
#> 10 -0.15490196 -0.15490196   NULL  mini
#> 11 -0.09691001 -0.09691001   NULL  mini
#> 12 -0.04575749 -0.04575749   NULL  mini
#> 13  0.30103000  0.30103000   NULL  mini
#> 14  0.47712125  0.47712125   NULL  mini
#> 15  0.60205999  0.60205999   NULL  mini
#> 16  0.77815125  0.77815125   NULL  mini
#> 17  0.84509804  0.84509804   NULL  mini
#> 18  0.90308999  0.90308999   NULL  mini
#> 19  0.95424251  0.95424251   NULL  mini

# Providing custom values
key_manual(
  aesthetic = 1:5,
  label = c("one", "two", "three", "four", "five")
)
#>   aesthetic .value .label
#> 1         1      1    one
#> 2         2      2    two
#> 3         3      3  three
#> 4         4      4   four
#> 5         5      5   five

# Values from a `<data.frame>`
key_map(ToothGrowth, aesthetic = unique(supp))
#>   aesthetic .value .label
#> 1        VC     VC     VC
#> 2        OJ     OJ     OJ

# Empty key
key_none()
#> [1] aesthetic .value    .label   
#> <0 rows> (or 0-length row.names)
```
