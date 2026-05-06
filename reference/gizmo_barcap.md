# Guide gizmo: capped colour bar

This guide displays a colour bar with optional caps at either ends of
the bar.

## Usage

``` r
gizmo_barcap(
  key = "sequence",
  shape = "triangle",
  size = NULL,
  show = NA,
  alpha = NA,
  oob = "keep",
  theme = NULL,
  position = waiver(),
  direction = NULL
)
```

## Arguments

- key:

  A [sequence
  key](https://teunbrand.github.io/legendry/reference/key_specialty.md)
  specification. Defaults to
  [`key_sequence(n = 15)`](https://teunbrand.github.io/legendry/reference/key_specialty.md).
  Changing the argument to
  [`key_sequence()`](https://teunbrand.github.io/legendry/reference/key_specialty.md)
  is fine, but changing the key type is not advised.

- shape:

  A [cap](https://teunbrand.github.io/legendry/reference/cap_options.md)
  specification by providing one of the following:

  - A cap `<function>`, such as
    [`cap_triangle()`](https://teunbrand.github.io/legendry/reference/cap_options.md).

  - A `<character[1]>` naming a cap function without the
    '`cap_`'-prefix, e.g. `"round"`.

  - A two column `<matrix[n, 2]>` giving coordinates for a cap, like
    those created by cap functions such as
    [`cap_arch()`](https://teunbrand.github.io/legendry/reference/cap_options.md).

- size:

  A [`<unit>`](https://rdrr.io/r/grid/unit.html) setting the size of the
  cap. When `NULL` (default), cap size will be proportional to the
  `shape` coordinates and the `legend.key.size` theme setting.

- show:

  A `<logical>` to control how caps are displayed at the ends of the
  bar. When `TRUE`, caps are always displayed. When `FALSE`, caps are
  never displayed. When `NA` (default), caps are displayed when the data
  range exceed the limits. When given as `<logical[2]>`, `show[1]`
  controls the display at the lower end and `show[2]` at the upper end.

- alpha:

  A `<numeric[1]>` between 0 and 1 setting the colour transparency of
  the bar. Use `NA` to preserve the alpha encoded in the colour itself.

- oob:

  An out-of-bounds handling function that affects the cap colour. Can be
  one of the following:

  - A `<function>` like
    [`oob_squish`](https://scales.r-lib.org/reference/oob.html).

  - A `<character[1]>` naming such a function without the
    '`oob`'-prefix, such as `"keep"`.

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

- direction:

  A `<character[1]>` indicating the direction of the guide. Can be on of
  `"horizontal"` or `"vertical"`.

## Value

A `<GizmoBarcap>` object.

## See also

Other gizmos:
[`gizmo_density()`](https://teunbrand.github.io/legendry/reference/gizmo_density.md),
[`gizmo_grob()`](https://teunbrand.github.io/legendry/reference/gizmo_grob.md),
[`gizmo_histogram()`](https://teunbrand.github.io/legendry/reference/gizmo_histogram.md),
[`gizmo_stepcap()`](https://teunbrand.github.io/legendry/reference/gizmo_stepcap.md)

## Examples

``` r
# A standard plot
p <- ggplot(mpg, aes(displ, hwy, colour = cty)) +
  geom_point()

# Just a bar
p + scale_colour_viridis_c(guide = gizmo_barcap())


# Caps show up when there is data outside the limits
p + scale_colour_viridis_c(
  limits = c(10, 30),
  guide  = gizmo_barcap()
)


# The scale's out-of-bounds handler determines cap colour
p + scale_colour_viridis_c(
  limits = c(10, 30), oob = scales::oob_squish,
  guide = gizmo_barcap()
)


# Customising display of the guide
p +
  scale_colour_viridis_c(
    oob = scales::oob_squish,
    guide = gizmo_barcap(
      shape = "arch", show = c(FALSE, TRUE),
      size = unit(2, "cm"),
      theme = theme(legend.key.height = unit(4, "cm"))
    )
  ) +
  theme(
    legend.frame = element_rect(colour = "black"),
    legend.key.width = unit(0.5, "cm")
  )
```
