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
  order = 0L
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

## Details

### Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide.

|  |  |  |
|----|----|----|
| **Theme setting** | **Type** | **Description** |
| `legendry.legend.subtitle` | [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html) | Title of groups in the legend. |
| `legendry.legend.subtitle.position` | `<character[1]>` | One of `"top"`, `"right"`, `"bottom"` or `"left"`. |
| `legendry.group.spacing` | [`unit()`](https://rdrr.io/r/grid/unit.html) | Spacing in between groups of keys. |
| `legend.background` | [`element_rect()`](https://ggplot2.tidyverse.org/reference/element.html) | Background of the legend. |
| `legend.margin` | [`margin()`](https://ggplot2.tidyverse.org/reference/element.html) | Padding around the legend. |
| `legend.text` | [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html) | Labels displayed next to keys. |
| `legend.text.position` | `<character[1]>` | One of `"top"`, `"right"`, `"bottom"` or `"left"`. |
| `legend.title` | [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html) | Title of the legend. |
| `legend.title.position` | `<character[1]>` | One of `"top"`, `"right"`, `"bottom"` or `"left"`. |
| `legend.key` | [`element_rect()`](https://ggplot2.tidyverse.org/reference/element.html) | Background of the key areas. |
| `legend.key.height` | [`unit()`](https://rdrr.io/r/grid/unit.html) | Height of keys. |
| `legend.key.width` | [`unit()`](https://rdrr.io/r/grid/unit.html) | Width of keys. |
| `legend.key.justification` | `<numeric[2]>` | Justification for placing legend keys in excess space. |
| `legend.key.spacing.x` | [`unit()`](https://rdrr.io/r/grid/unit.html) | Horizontal spacing between keys. |
| `legend.key.spacing.y` | [`unit()`](https://rdrr.io/r/grid/unit.html) | Vertical spacing between keys. Taken literally. |
| `legend.byrow` | `<logical[1]>` | Row-order key filling (`TRUE`) or column-order (`FALSE`) |

The context-agnostic alternative to using
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) is to
use
[`theme_guide()`](https://teunbrand.github.io/legendry/reference/theme_guide.md):

    guide_legend_group(theme = theme_guide(
      subtitle = element_text(),
      subtitle.position = "top",
      group.spacing = unit(5, "mm"),
      text = element_text(),
      text.position = "right",
      title = element_text(),
      title.position = "top",
      key = element_rect(),
      key.height = unit(5, "mm"),
      key.width = unit(5, "mm"),
      key.justification = c(0.5, 0.5),
      key.spacing.x = unit(5, "mm"),
      key.spacing.y = unit(5, "mm"),
      byrow = TRUE,
      margin = margin(5),
      background = element_rect(),
    ))

## See also

Other standalone guides:
[`guide_axis_annotation()`](https://teunbrand.github.io/legendry/reference/guide_axis_annotation.md),
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
