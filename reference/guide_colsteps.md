# Custom colour steps guide

Similar to
[`guide_coloursteps()`](https://ggplot2.tidyverse.org/reference/guide_coloursteps.html),
this guide displays continuous `colour` or `fill` aesthetics. It has
additional options to display caps at the end of the bar, depending on
out-of-bounds values.

## Usage

``` r
guide_colsteps(
  title = waiver(),
  key = "bins",
  first_guide = "axis_base",
  second_guide = "axis_base",
  shape = "triangle",
  size = NULL,
  show = NA,
  alpha = NA,
  reverse = FALSE,
  suppress_labels = "second",
  oob = scales::oob_keep,
  theme = NULL,
  position = waiver(),
  vanilla = TRUE,
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

  A [bins
  key](https://teunbrand.github.io/legendry/reference/key_specialty.md)
  specificiation. Defaults to
  `key_bins(even.steps = FALSE, show.limits = NULL)`. Changing the
  arguments to
  [`key_bins()`](https://teunbrand.github.io/legendry/reference/key_specialty.md)
  is fine, but changing the key type is not advised.

- first_guide, second_guide:

  Guides to flank the colour steps. Each guide can be specified using
  one of the following:

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

- alpha:

  A `<numeric[1]>` between 0 and 1 setting the colour transparency of
  the bar. Use `NA` to preserve the alpha encoded in the colour itself.

- reverse:

  A `<logical[1]>` whether to reverse continuous guides. If `TRUE`,
  guides like colour bars are flipped. If `FALSE` (default), the
  original order is maintained.

- suppress_labels:

  A `<character>` vector giving any of `"text"` and `"opposite"` for the
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

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

- vanilla:

  A `<logical[1]>` whether to have the default style match the vanilla
  [`guide_colourbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html)
  (`TRUE`) or take the theme verbatim (`FALSE`).

- available_aes:

  A `<character>` vector listing the aesthetics for which this guide can
  be build.

## Value

A `<Guide>` object

## Details

As steps are rendered as clipped rectangles, it is important to use a
graphics device that can render clipped paths. This can be checked by
using
[`check_device("clippingPaths")`](https://ggplot2.tidyverse.org/reference/check_device.html).

## See also

Other standalone guides:
[`guide_axis_base()`](https://teunbrand.github.io/legendry/reference/guide_axis_base.md),
[`guide_axis_dendro()`](https://teunbrand.github.io/legendry/reference/guide_axis_dendro.md),
[`guide_axis_nested()`](https://teunbrand.github.io/legendry/reference/guide_axis_nested.md),
[`guide_axis_plot()`](https://teunbrand.github.io/legendry/reference/guide_axis_plot.md),
[`guide_axis_symbols()`](https://teunbrand.github.io/legendry/reference/guide_axis_symbols.md),
[`guide_circles()`](https://teunbrand.github.io/legendry/reference/guide_circles.md),
[`guide_colbar()`](https://teunbrand.github.io/legendry/reference/guide_colbar.md),
[`guide_colring()`](https://teunbrand.github.io/legendry/reference/guide_colring.md),
[`guide_legend_base()`](https://teunbrand.github.io/legendry/reference/guide_legend_base.md),
[`guide_legend_cross()`](https://teunbrand.github.io/legendry/reference/guide_legend_cross.md),
[`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md),
[`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)

## Examples

``` r
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = cty))

# The colour steps show caps when values are out-of-bounds
p + scale_colour_viridis_b(
  limits = c(10, NA),
  guide = "colsteps"
)


# It also shows how oob values are handled
p + scale_colour_viridis_b(
  limits = c(10, 30), oob = scales::oob_censor,
  guide = "colsteps"
)


# Adjusting the type of cap
p + scale_colour_viridis_b(
  limits = c(10, 30),
  guide = guide_colsteps(shape = "round")
)


# The default is to use the breaks as-is
p + scale_colour_viridis_b(
  limits = c(10, 30), breaks = c(10, 20, 25),
  guide = "colsteps"
)


# But the display can be set to use evenly spaced steps
p + scale_colour_viridis_b(
  limits = c(10, 30), breaks = c(10, 20, 25),
  guide = guide_colsteps(key = key_bins(even.steps = TRUE))
)


# Using tick marks by swapping side guides
p + scale_colour_viridis_b(
  guide = guide_colsteps(
    first_guide  = "axis_base",
    second_guide = "axis_base"
  )
)
```
