# Group keys

These functions are helper functions for working with grouped data as
keys in guides. They all share the goal of creating a guide key, but
have different methods.

- `key_group_split()` is a function factory whose functions make an
  attempt to infer groups from the scale's labels.

- `key_group_lut()` is a function factory whose functions use a look up
  table to sort out group membership.

## Usage

``` r
key_group_split(sep = "[^[:alnum:]]+", reverse = FALSE)

key_group_lut(members, group, ungrouped = "Other")
```

## Arguments

- sep:

  A `<character[1]>` giving a [regular
  expression](https://rdrr.io/r/base/regex.html) to use for splitting
  labels provided by the scale using the
  [`strsplit()`](https://rdrr.io/r/base/strsplit.html) function. By
  defaults, labels are splitted on any non-alphanumeric character.

- reverse:

  A `<logical[1]>` which if `FALSE` (default) treats the first part of
  the split string as groups and later parts as members. If `TRUE`,
  treats the last part as groups.

- members:

  A vector including the scale's `breaks` values.

- group:

  A vector parallel to `members` giving the group of each member.

- ungrouped:

  A `<character[1]>` giving a group label to assign to the scale's
  `breaks` that match no values in the `members` argument.

## Value

A function to use as the `key` argument in a guide.

## See also

Other keys:
[`key_range`](https://teunbrand.github.io/legendry/reference/key_range.md),
[`key_segments`](https://teunbrand.github.io/legendry/reference/key_segments.md),
[`key_specialty`](https://teunbrand.github.io/legendry/reference/key_specialty.md),
[`key_standard`](https://teunbrand.github.io/legendry/reference/key_standard.md)

## Examples

``` r
# Example scale
values <- c("group A:value 1", "group A:value 2", "group B:value 1")
template <- scale_colour_hue(limits = values)

# Treat the 'group X' part as groups
key <- key_group_split(sep = ":")
key(template)
#>    colour          .value  .label  .group
#> 1 #F8766D group A:value 1 value 1 group A
#> 2 #00BA38 group A:value 2 value 2 group A
#> 3 #619CFF group B:value 1 value 1 group B

# Treat the 'value X' part as groups
key <- key_group_split(sep = ":", reverse = TRUE)
key(template)
#>    colour          .value  .label  .group
#> 1 #F8766D group A:value 1 group A value 1
#> 2 #619CFF group B:value 1 group B value 1
#> 3 #00BA38 group A:value 2 group A value 2

# Example scale
template <- scale_colour_hue(limits = msleep$name[c(1, 7, 9, 23, 24)])

# A lookup table can have more entries than needed
key <- key_group_lut(msleep$name, msleep$order)
key(template)
#>    colour            .value            .label         .group
#> 1 #F8766D           Cheetah           Cheetah      Carnivora
#> 2 #A3A500 Northern fur seal Northern fur seal      Carnivora
#> 3 #00BF7D               Dog               Dog      Carnivora
#> 4 #00B0F6             Horse             Horse Perissodactyla
#> 5 #E76BF3            Donkey            Donkey Perissodactyla

# Or less entries than needed
key <- key_group_lut(
  msleep$name[23:24], msleep$order[23:24],
  ungrouped = "Other animals"
)
key(template)
#>    colour            .value            .label         .group
#> 1 #00B0F6             Horse             Horse Perissodactyla
#> 2 #E76BF3            Donkey            Donkey Perissodactyla
#> 3 #F8766D           Cheetah           Cheetah  Other animals
#> 4 #A3A500 Northern fur seal Northern fur seal  Other animals
#> 5 #00BF7D               Dog               Dog  Other animals
```
