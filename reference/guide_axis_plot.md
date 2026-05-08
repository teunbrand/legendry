# Side-plot axis

**\[experimental\]**

Displays an axis-sharing plot to the side of the panel.

## Usage

``` r
guide_axis_plot(
  plot,
  title = NULL,
  size = unit(2, "cm"),
  reposition = TRUE,
  theme = theme_sub_legend(position = "none"),
  position = waiver()
)
```

## Arguments

- plot:

  A `<ggplot>` object, subject to limitations listed in the 'Details'
  section. Alternatively, a `<function>` that takes the scale as
  argument and returns a `<ggplot>` object.

- title:

  One of the following to indicate the title of the guide:

  - A `<character[1]>` or `<expression[1]>` to set a custom title.

  - `NULL` (default) to not display any title.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html) to
    take the name of the scale object or the name specified in
    [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) as the
    title. Please note that `plot` will still display a title unless
    instructed otherwise. To avoid duplicated titles, the default is to
    have no title for the guide.

- size:

  An absolute [`<unit>`](https://rdrr.io/r/grid/unit.html) to set the
  size of the `plot` panel in the orthogonal direction.

- reposition:

  A `<logical[1]>`. If `TRUE` (default) the `position` argument of this
  guide will be propagated to the position scale bestowed upon the
  `plot` argument. If `FALSE`, that position scale will retain its
  original `position` field. Setting `reposition = TRUE` will generally
  tend to point axes outwards.

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The order in which themes are applies is as
  follows: (1) the main plot's theme (2) the `plot` argument's theme
  and (3) this `theme` argument. The default `theme` argument suppresses
  legends.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

## Value

A `<Guide>` object.

## Details

This guide is subject to the following limitations:

- The x- or y-scale of the main plot override the corresponding scale in
  the `plot` argument. This ensures that the scales line up. The `plot`
  argument should not have the relevant scale.

- The `plot` argument cannot have custom facets. It must use the default
  [`facet_null()`](https://ggplot2.tidyverse.org/reference/facet_null.html).

- This guide cannot be used in non-linear coordinate systems of the main
  plot and does not support non-linear coordinate systems in the `plot`
  argument.

- The `theme(panel.widths, panel.heights)` setting in the `plot`
  argument will be ignored in favour of the `size` argument.

- There is no mechanism to accommodate extra space needed by plot
  components outside the panel. This applies in the horizontal direction
  for x-axes and the vertical direction for y-axes. You may need to
  manually tweak the `theme(plot.margin)` setting of the main plot to
  accommodate these components.

## See also

Other standalone guides:
[`guide_axis_base()`](https://teunbrand.github.io/legendry/reference/guide_axis_base.md),
[`guide_axis_dendro()`](https://teunbrand.github.io/legendry/reference/guide_axis_dendro.md),
[`guide_axis_nested()`](https://teunbrand.github.io/legendry/reference/guide_axis_nested.md),
[`guide_axis_symbols()`](https://teunbrand.github.io/legendry/reference/guide_axis_symbols.md),
[`guide_circles()`](https://teunbrand.github.io/legendry/reference/guide_circles.md),
[`guide_colbar()`](https://teunbrand.github.io/legendry/reference/guide_colbar.md),
[`guide_colring()`](https://teunbrand.github.io/legendry/reference/guide_colring.md),
[`guide_colsteps()`](https://teunbrand.github.io/legendry/reference/guide_colsteps.md),
[`guide_legend_base()`](https://teunbrand.github.io/legendry/reference/guide_legend_base.md),
[`guide_legend_cross()`](https://teunbrand.github.io/legendry/reference/guide_legend_cross.md),
[`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md),
[`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)

## Examples

``` r
# A standard plot
main_plot <- ggplot(mpg, aes(displ, hwy, colour = drv)) +
  geom_point()

# Simple plot sharing the x-variable
x_plot <- ggplot(mpg, aes(displ, fill = drv)) +
  geom_density(alpha = 0.7)

# Simple plot sharing the y-variable
y_plot <- ggplot(mpg, aes(drv, hwy, colour = drv)) +
  geom_boxplot()

# Typical use
main_plot + guides(
  x = guide_axis_plot(x_plot),
  y = guide_axis_plot(y_plot)
)


main_plot + guides(
  # Include `fill` legend by overriding theme
  x = guide_axis_plot(x_plot, theme = NULL),
  # Change the size of the side-plot
  y = guide_axis_plot(y_plot, size = unit(4, "cm"))
)


# Components outside panels may need to be manually acommodated
main_plot +
  guides(y = guide_axis_plot(y_plot + labs(title = "Boxplot"))) +
  theme(plot.margin = margin(25, 5.5, 5.5, 5.5))


# Recursive use of this guide
main_plot + guides(x = guide_axis_plot(
  main_plot + guides(x = guide_axis_plot(x_plot))
))
```
