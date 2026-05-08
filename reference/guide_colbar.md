# Custom colour bar guide

Similar to
[`guide_colourbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html),
this guide displays continuous `colour` or `fill` aesthetics. It has
additional options to display caps at the end of the bar, depending on
out-of-bounds values.

## Usage

``` r
guide_colbar(
  title = waiver(),
  key = "auto",
  first_guide = "axis_base",
  second_guide = first_guide,
  shape = "triangle",
  size = NULL,
  show = NA,
  nbin = 15,
  alpha = NA,
  reverse = FALSE,
  suppress_labels = "second",
  oob = scales::oob_keep,
  theme = NULL,
  vanilla = TRUE,
  position = waiver(),
  available_aes = c("colour", "fill")
)
```

## Arguments

- title:

  One of the following to indicate the title of the guide:

  - A `<character[1]>` or `<expression[1]>` to set a custom title.

  - `NULL` to not display any title.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    (default) to take the name of the scale object or the name specified
    in [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) as
    the title.

- key:

  A [sequence
  key](https://teunbrand.github.io/legendry/reference/key_specialty.md)
  specification. Defaults to
  [`key_sequence(n = 15)`](https://teunbrand.github.io/legendry/reference/key_specialty.md).
  Changing the argument to
  [`key_sequence()`](https://teunbrand.github.io/legendry/reference/key_specialty.md)
  is fine, but changing the key type is not advised.

- first_guide, second_guide:

  Guides to flank the colour bar. Each guide can be specified using one
  of the following:

  - A `<Guide>` class object.

  - A `<function>` that returns a `<Guide>` class object.

  - A `<character>` naming such a function, without the `guide_` or
    `primitive_` prefix.

  The `first_guide` will be placed at the location specified by the
  `legend.text.position` theme setting. The `second_guide` will be
  placed opposite that position. When `second_guide` has a label
  suppression mechanism, no labels will be drawn for that guide.

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

- nbin:

  A positive `<integer[1]>` determining how many colours to use for the
  colour gradient.

- alpha:

  A `<numeric[1]>` between 0 and 1 setting the colour transparency of
  the bar. Use `NA` to preserve the alpha encoded in the colour itself.

- reverse:

  A `<logical[1]>` whether to reverse continuous guides. If `TRUE`,
  guides like colour bars are flipped. If `FALSE` (default), the
  original order is maintained.

- suppress_labels:

  A `<character>` vector giving any of `"first"` and `"second"` for the
  parallel guides. The guide(s) listed here will not draw labels if they
  support a label suppression mechanism.

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

- vanilla:

  A `<logical[1]>` whether to have the default style match the vanilla
  [`guide_colourbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html)
  (`TRUE`) or take the theme verbatim (`FALSE`).

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

- available_aes:

  A `<character>` vector listing the aesthetics for which this guide can
  be build.

## Value

A `<Guide>` object

## Details

As colours are always rendered as gradients, it is important to use a
graphics device that can render these. This can be checked by using
[`check_device("gradients")`](https://ggplot2.tidyverse.org/reference/check_device.html).

## See also

Other standalone guides:
[`guide_axis_base()`](https://teunbrand.github.io/legendry/reference/guide_axis_base.md),
[`guide_axis_dendro()`](https://teunbrand.github.io/legendry/reference/guide_axis_dendro.md),
[`guide_axis_nested()`](https://teunbrand.github.io/legendry/reference/guide_axis_nested.md),
[`guide_axis_plot()`](https://teunbrand.github.io/legendry/reference/guide_axis_plot.md),
[`guide_axis_symbols()`](https://teunbrand.github.io/legendry/reference/guide_axis_symbols.md),
[`guide_circles()`](https://teunbrand.github.io/legendry/reference/guide_circles.md),
[`guide_colring()`](https://teunbrand.github.io/legendry/reference/guide_colring.md),
[`guide_colsteps()`](https://teunbrand.github.io/legendry/reference/guide_colsteps.md),
[`guide_legend_base()`](https://teunbrand.github.io/legendry/reference/guide_legend_base.md),
[`guide_legend_cross()`](https://teunbrand.github.io/legendry/reference/guide_legend_cross.md),
[`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md),
[`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)

## Examples

``` r
# A standard plot
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = cty))

# The colourbar shows caps when values are out-of-bounds (oob)
p + scale_colour_viridis_c(
  limits = c(10, NA),
  guide = "colbar"
)


# It also shows how oob values are handled
p + scale_colour_viridis_c(
  limits = c(10, NA), oob = scales::oob_squish,
  guide = "colbar"
)


# Adjusting the type of cap
p + scale_colour_viridis_c(
  limits = c(10, 30), oob = scales::oob_squish,
  guide = guide_colbar(shape = "round")
)


# One-sided ticks
p + scale_colour_viridis_c(
  guide = guide_colbar(second_guide = "none")
)


# Colour bar with minor breaks
p + scale_colour_viridis_c(
  minor_breaks = scales::breaks_width(1),
  guide = guide_colbar(key = "minor")
)


# Using log ticks on a colourbar
ggplot(msleep, aes(sleep_total, sleep_rem)) +
  geom_point(aes(colour = bodywt), na.rm = TRUE) +
  scale_colour_viridis_c(
    transform = "log10",
    guide = guide_colbar(key = "log")
  )
```
