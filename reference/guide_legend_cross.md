# Cross legend guide

This is a legend type similar to
[`guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html)
that displays crosses, or: interactions, between two variables.

## Usage

``` r
guide_legend_cross(
  key = NULL,
  title = waiver(),
  row_title = waiver(),
  col_title = waiver(),
  swap = FALSE,
  col_text = element_text(angle = 90, vjust = 0.5),
  subtitle_position = position_text(angle = c(0, -90, 0, 90), hjust = 0.5),
  override.aes = list(),
  reverse = FALSE,
  theme = NULL,
  position = NULL,
  direction = NULL,
  order = 0
)
```

## Arguments

- key:

  One of the following key specifications:

  - A [group
    split](https://teunbrand.github.io/legendry/reference/key_group.md)
    specification when using the legend to display a compound variable
    like `paste(var1, var2)`.

  - A [standard
    key](https://teunbrand.github.io/legendry/reference/key_standard.md)
    specification, like
    [`key_auto()`](https://teunbrand.github.io/legendry/reference/key_standard.md),
    when crossing two separate variables across two scales.

- title:

  One of the following to indicate the title of the guide:

  - A `<character[1]>` or `<expression[1]>` to set a custom title.

  - `NULL` to not display any title.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    (default) to take the name of the scale object or the name specified
    in [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) as
    the title.

- row_title, col_title:

  One of the following to indicate subtitles spanning the rows and
  columns of the guide:

  - A `<character[1]>` or `<expression[1]>` to set a custom title.

  - `NULL` to not display any title.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html) to
    propagate subtitles from merging guides (default).

- swap:

  A `<logical[1]>` which when `TRUE` exchanges the column and row
  variables in the displayed legend.

- col_text:

  An `<element_text>` object giving adjustments to text for the column
  labels. Can be `NULL` to display column labels in equal fashion to the
  row labels.

- subtitle_position:

  A named list of 4 [text
  elements](https://ggplot2.tidyverse.org/reference/element.html),
  having the names `"top"`, `"right"`, `"bottom"` and `"left`. These
  govern the display of subtitles when placed in any of these positions
  relative to the keys. See
  [`position_text()`](https://teunbrand.github.io/legendry/reference/position_text.md)
  for a convenient helper.

- override.aes:

  A named `<list>` specifying aesthetic parameters of the key glyphs.
  See details and examples in
  [`guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html).

- reverse:

  A `<logical[2]>` whether the order of the keys should be inverted,
  where the first value controls the row order and second value the
  column order. Input as `<logical[1]>` will be recycled.

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

- order:

  A positive `<integer[1]>` that specifies the order of this guide among
  multiple guides. This controls in which order guides are merged if
  there are multiple guides for the same position. If `0` (default), the
  order is determined by a hashing indicative settings of a guide.

## Value

A `<GuideLegend>` object.

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
[`guide_colsteps()`](https://teunbrand.github.io/legendry/reference/guide_colsteps.md),
[`guide_legend_base()`](https://teunbrand.github.io/legendry/reference/guide_legend_base.md),
[`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md),
[`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)

Other legend guides:
[`guide_legend_base()`](https://teunbrand.github.io/legendry/reference/guide_legend_base.md),
[`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md),
[`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)

## Examples

``` r
# Standard use for single aesthetic. The default is to split labels to
# disentangle aesthetics that are already crossed (by e.g. `paste()`)
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = paste(year, drv))) +
  guides(colour = "legend_cross")


# If legends should be merged between identical aesthetics, both need the
# same legend type.
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = paste(year, drv), shape = paste(year, drv))) +
  guides(colour = "legend_cross", shape = "legend_cross")


# Crossing two aesthetics requires a shared title and `key = "auto"`. The
# easy way to achieve this is to predefine a shared guide.
my_guide <- guide_legend_cross(key = "auto", title = "My title")

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = drv, shape = factor(year))) +
  guides(colour = my_guide, shape  = my_guide)


# You can cross more than 2 aesthetics but not more than 2 unique aesthetics.
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = drv, shape = factor(year), size = factor(drv))) +
  scale_size_ordinal() +
  guides(colour = my_guide, shape = my_guide, size = my_guide)


# You can merge an aesthetic that is already crossed with an aesthetic that
# contributes to only one side of the cross.
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = paste(year, drv), shape  = drv)) +
  guides(
    colour = guide_legend_cross(title = "My Title"),
    shape  = guide_legend_cross(title = "My Title", key = "auto")
  )
```
