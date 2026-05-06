# Grouped legend

This legend resembles
[`ggplot2::guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html),
but has the ability to keep groups in blocks with their own titles.

## Usage

``` r
guide_legend_group(
  key = "group_split",
  title = waiver(),
  override.aes = list(),
  nrow = NULL,
  ncol = NULL,
  theme = NULL,
  position = NULL,
  direction = NULL,
  order = 0
)
```

## Arguments

- key:

  A [group
  key](https://teunbrand.github.io/legendry/reference/key_group.md)
  specification. Defaults to
  [`key_group_split()`](https://teunbrand.github.io/legendry/reference/key_group.md)
  to split labels to find groups.

- title:

  One of the following to indicate the title of the guide:

  - A `<character[1]>` or `<expression[1]>` to set a custom title.

  - `NULL` to not display any title.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    (default) to take the name of the scale object or the name specified
    in [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) as
    the title.

- override.aes:

  A named `<list>` specifying aesthetic parameters of the key glyphs.
  See details and examples in
  [`guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html).

- nrow, ncol:

  A positive `<integer[1]>` setting the desired dimensions of the legend
  layout. Either `nrow` or `ncol` can be set, but not both,

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
[`guide_circles()`](https://teunbrand.github.io/legendry/reference/guide_circles.md),
[`guide_colbar()`](https://teunbrand.github.io/legendry/reference/guide_colbar.md),
[`guide_colring()`](https://teunbrand.github.io/legendry/reference/guide_colring.md),
[`guide_colsteps()`](https://teunbrand.github.io/legendry/reference/guide_colsteps.md),
[`guide_legend_base()`](https://teunbrand.github.io/legendry/reference/guide_legend_base.md),
[`guide_legend_cross()`](https://teunbrand.github.io/legendry/reference/guide_legend_cross.md),
[`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)

Other legend guides:
[`guide_legend_base()`](https://teunbrand.github.io/legendry/reference/guide_legend_base.md),
[`guide_legend_cross()`](https://teunbrand.github.io/legendry/reference/guide_legend_cross.md),
[`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)

## Examples

``` r
# Standard plot for selection of `msleep`
df <- msleep[c(9, 28, 11, 5, 34, 54, 64, 24, 53), ]

p <- ggplot(df) +
  aes(bodywt, awake, colour = paste(order, name)) +
  geom_point()

# By default, groups are inferred from the name
p + guides(colour = "legend_group")


# You can also use a look-up table for groups
# The lookup table can be more expansive than just the data:
# We're using the full 'msleep' data here instead of the subset
lut <- key_group_lut(msleep$name, msleep$order)

p + aes(colour = name) +
  guides(colour = guide_legend_group(key = lut))


# `nrow` and `ncol` apply within groups
p + guides(colour = guide_legend_group(nrow = 1))


# Groups are arranged according to `direction`
p + guides(colour = guide_legend_group(ncol = 1, direction = "horizontal")) +
  theme(legend.title.position = "top")


# Customising the group titles
p + guides(colour = "legend_group") +
  theme(
    legendry.legend.subtitle.position = "left",
    legendry.legend.subtitle = element_text(
      hjust = 1, vjust = 1, size = rel(0.9),
      margin = margin(t = 5.5, r = 5.5)
    )
  )


# Changing the spacing between groups
p + guides(colour = "legend_group") +
  theme(legendry.group.spacing = unit(0, "cm"))
```
