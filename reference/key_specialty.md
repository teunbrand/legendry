# Speciality keys

These functions are helper functions for working with keys in guides.
The functions described here are not widely applicable and may only
apply to a small subset of guides. As such, it is fine to adjust the
arguments of a speciality key, but swapping types is ill-advised.

- `key_sequence()` is a function factory whose functions create a
  regularly spaced sequence between the limits of a scale. It is used in
  colour bar guides.

- `key_bins()` is a function factory whose function create a binned key
  given the breaks in the scale. It is used in colour steps guides.

- `key_upset()` is a function factory whose function creates an upset
  key from splitting the breaks in the scale. It is used in the upset
  guide.

- `key_symbols()` is a function factory whose function creates a key
  from the literal provided values. It is used in the symbols guide.

## Usage

``` r
key_sequence(n = 15)

key_bins(even.steps = FALSE, show.limits = NULL)

key_upset(sep = "[^[:alnum:]]+", order = NULL, empty_label = "Other")

key_symbols(aesthetic, level, symbol = NULL, ...)
```

## Arguments

- n:

  A positive `<integer[1]>` giving the number of colours to use for a
  gradient.

- even.steps:

  A `<logical[1]>` indicating whether the size of bins should be
  displayed as equal (`TRUE`) or proportional to their length in data
  space (`FALSE`).

- show.limits:

  A `<logical[1]>` stating whether the limits of the scale should be
  shown with labels and ticks (`TRUE`) or remain hidden (`FALSE`). Note
  that breaks coinciding with limits are shown regardless of this
  setting. The default, `NULL`, consults the scale's `show.limits`
  setting or defaults to `FALSE`.

- sep:

  A `<character[1]>` giving a [regular
  expression](https://rdrr.io/r/base/regex.html) to use for splitting
  labels provided by the scale using the
  [`strsplit()`](https://rdrr.io/r/base/strsplit.html) function. By
  defaults, labels are splitted on any non-alphanumeric character.

- order:

  Order to set the upset layers in. One of the following:

  - A `<character[n]>` giving pieces of split labels.

  - An `<integer[n]>` giving the numerical order in which pieces of
    split labels should appear.

- empty_label:

  A `<character[1]>` giving a level label to assign to the breaks that
  match no values to the pieces of split labels. Can be `NULL` to omit
  labels for empty levels.

- aesthetic:

  A vector of values for the guide to represent equivalent to the
  `breaks` argument in scales. These will be mapped by the scale to
  positions. Alternatively, a `<numeric[n]>` vector to set positions
  directly. Positions are used to place symbols.

- level:

  A `<factor[n]>` or `<character[n]>` parallel to the `aesthetic`
  argument setting the label level of the symbol.

- symbol:

  (Optional) An `<integer[n]>` indexing the guide's `override.aes`
  parameter.

- ...:

  Additional graphical properties to set for each symbol. Valid
  properties are `colour`, `shape`, `size`, `fill` and `stroke`. These
  graphical properties have priority over properties derived via
  `symbol` or the theme.

## Value

A function.

## See also

Other keys:
[`key_group`](https://teunbrand.github.io/legendry/reference/key_group.md),
[`key_range`](https://teunbrand.github.io/legendry/reference/key_range.md),
[`key_segments`](https://teunbrand.github.io/legendry/reference/key_segments.md),
[`key_standard`](https://teunbrand.github.io/legendry/reference/key_standard.md)

## Examples

``` r
# An example scale
template <- scale_fill_viridis_c(limits = c(0, 10), breaks = c(2, 4, 6, 8))

# Retrieving colourbar and colourstep keys
key_sequence()(template)
#>       fill     .value
#> 1  #440154  0.0000000
#> 2  #461F66  0.7142857
#> 3  #453478  1.4285714
#> 4  #414888  2.1428571
#> 5  #3C5B8A  2.8571429
#> 6  #336D8D  3.5714286
#> 7  #2B7F8D  4.2857143
#> 8  #2B9089  5.0000000
#> 9  #26A186  5.7142857
#> 10 #40B17B  6.4285714
#> 11 #5FBF69  7.1428571
#> 12 #76CE56  7.8571429
#> 13 #A4D848  8.5714286
#> 14 #D2E039  9.2857143
#> 15 #FDE725 10.0000000
key_bins()(template)
#>      fill min max .label .value
#> 1 #46286D   0   2   <NA>     NA
#> 2 #3B5E8B   2   4      2      2
#> 3 #2B9089   4   6      4      4
#> 4 #5ABC6D   6   8      6      6
#> 5 #C0DD40   8  10      8      8
#> 6    <NA>  NA  NA   <NA>     NA

# Upset key with example scale
template <- scale_x_discrete(limits = c("A", "A,B", ""))
key_upset()(template)
#>   x .col .row .value .symbol
#> 1 1    1    1      A    TRUE
#> 2 2    1    2      A    TRUE
#> 3 3    1    3      A   FALSE
#> 4 1    2    1      B   FALSE
#> 5 2    2    2      B    TRUE
#> 6 3    2    3      B   FALSE
#> 7 1    3    1  Other      NA
#> 8 2    3    2  Other      NA
#> 9 3    3    3  Other    TRUE
# Putting 'B' in 1st level
key_upset(order = c("B", "A"))(template)
#>   x .col .row .value .symbol
#> 1 1    1    1      B   FALSE
#> 2 2    1    2      B    TRUE
#> 3 3    1    3      B   FALSE
#> 4 1    2    1      A    TRUE
#> 5 2    2    2      A    TRUE
#> 6 3    2    3      A   FALSE
#> 7 1    3    1  Other      NA
#> 8 2    3    2  Other      NA
#> 9 3    3    3  Other    TRUE
# Omit level for the empty break
key_upset(empty_label = NULL)(template)
#>   x .col .row .value .symbol
#> 1 1    1    1      A    TRUE
#> 2 2    1    2      A    TRUE
#> 3 3    1    3      A   FALSE
#> 4 1    2    1      B   FALSE
#> 5 2    2    2      B    TRUE
#> 6 3    2    3      B   FALSE

# Symbol key with example scale
template <- scale_x_discrete(limits = LETTERS[1:5])
key_symbols(aesthetic = LETTERS[1:3], level = 3:1)(template)
#>   .value .col x .label .row
#> 1      3    3 1      A    1
#> 2      2    2 2      B    2
#> 3      1    1 3      C    3
# Aesthetic can also be numeric
key_symbols(1:3, 3:1)(template)
#>   aesthetic .value .col
#> 1         1      3    3
#> 2         2      2    2
#> 3         3      1    1
# Setting level order via factors
key_symbols(1:3, level = factor(c("X", "Y", "Z"), c("Y", "X", "Z")))(template)
#>   aesthetic .value .col
#> 1         1      X    2
#> 2         2      Y    1
#> 3         3      Z    3
# Setting groups for symbols, for `guide_axis_symbols(override.aes)`
key_symbols(1:3, 1:3, symbol = c(1, 1, 2))(template)
#>   aesthetic .value .col .symbol
#> 1         1      1    1       1
#> 2         2      2    2       1
#> 3         3      3    3       2
# Passing individual graphical parameters
key_symbols(1:3, 3:1, colour = c("red", "green", "blue"))(template)
#>   aesthetic .value .col .colour
#> 1         1      3    3     red
#> 2         2      2    2   green
#> 3         3      1    1    blue
```
