# Manual legend

**\[experimental\]**

This is a guide that displays user-defined keys independent of scales.
It should only be used as a last resort when struggling to format a
conventional legend.

## Usage

``` r
guide_legend_manual(
  labels,
  ...,
  layers = list(geom_point()),
  title = NULL,
  legend_args = list()
)
```

## Arguments

- labels:

  Labels to display next to the keys. Can be a `<character>` or
  `<expression>` vector to set labels, or `NULL` to draw no labels.

- ...:

  Arguments interpreted as aesthetics. For example: `colour = "red"`.
  The aesthetics must have the same size as the `labels` argument, or
  have size 1. These aesthetics may be overruled by fixed aesthetics set
  in the `layers` argument.

- layers:

  A `<list>` of layers (`<LayerInstance>` objects) created by the
  `geom_*()` or `stat_*()` family of functions. These layers are used
  for their
  [`key_glyph`](https://ggplot2.tidyverse.org/reference/draw_key.html)
  drawing functions, as well as to populate default aesthetics. Any
  fixed aesthetics provided to these layers overrule aesthetics passed
  to the `...` argument.

- title:

  One of the following to indicate the title of the guide:

  - A `<character[1]>` or `<expression[1]>` to set a custom title.

  - `NULL` to not display any title.

- legend_args:

  A `<list>` of arguments passed on to
  [`guide_legend_base()`](https://teunbrand.github.io/legendry/reference/guide_legend_base.md).

## Value

A `<GuideCustom>` object.

## Details

Because this guide is not tied to a scale, it can be given an arbitrary
name in
[`guides()`](https://ggplot2.tidyverse.org/reference/guides.html); as
long as it doesn't clash with other aesthetics.

## See also

Other standalone guides:
[`guide_axis_base()`](https://teunbrand.github.io/legendry/reference/guide_axis_base.md),
[`guide_axis_dendro()`](https://teunbrand.github.io/legendry/reference/guide_axis_dendro.md),
[`guide_axis_nested()`](https://teunbrand.github.io/legendry/reference/guide_axis_nested.md),
[`guide_axis_plot()`](https://teunbrand.github.io/legendry/reference/guide_axis_plot.md),
[`guide_circles()`](https://teunbrand.github.io/legendry/reference/guide_circles.md),
[`guide_colbar()`](https://teunbrand.github.io/legendry/reference/guide_colbar.md),
[`guide_colring()`](https://teunbrand.github.io/legendry/reference/guide_colring.md),
[`guide_colsteps()`](https://teunbrand.github.io/legendry/reference/guide_colsteps.md),
[`guide_legend_base()`](https://teunbrand.github.io/legendry/reference/guide_legend_base.md),
[`guide_legend_cross()`](https://teunbrand.github.io/legendry/reference/guide_legend_cross.md),
[`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md)

Other legend guides:
[`guide_legend_base()`](https://teunbrand.github.io/legendry/reference/guide_legend_base.md),
[`guide_legend_cross()`](https://teunbrand.github.io/legendry/reference/guide_legend_cross.md),
[`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md)

## Examples

``` r
# A standard plot
p <- ggplot(mtcars, aes(disp, mpg)) +
  geom_point()

# Typical usage: set `label` and some aesthetics
p + guides(
  some_name = guide_legend_manual(
    label  = c("foo", "bar"),
    colour = c(NA, "black"),
    fill   = c("grey40", NA),
    layers = geom_col()
  )
)


# Alternative: use `layers` to set aesthetics
p + guides(
  some_name = guide_legend_manual(
    label = c("foo", "bar"),
    layers = geom_col(
      # Must match length of `label`
      colour = c(NA, "black"),
      fill = c("grey40", NA)
    )
  )
)


# You can use >1 layer
p + guides(
  some_name = guide_legend_manual(
    label = c("foo", "bar"),
    colour = c("tomato", "dodgerblue"),
    fill = NA,
    layers = list(geom_col(), geom_point())
  )
)
```
