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

## Usage

``` r
key_sequence(n = 15)

key_bins(even.steps = FALSE, show.limits = NULL)
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

## Value

For `key_sequence()` a function.

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
```
