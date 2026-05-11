# Dendrogram guide

This axis is a speciality axis for discrete data that has been
hierarchically clustered. Please be aware that the guide cannot affect
the scale limits, which should be set appropriately. This guide will
give misleading results when this step is skipped!

## Usage

``` r
guide_axis_dendro(
  key = "dendro",
  title = waiver(),
  theme = NULL,
  labels = TRUE,
  space = rel(10),
  vanish = TRUE,
  n.dodge = 1L,
  angle = waiver(),
  check.overlap = FALSE,
  ticks = "none",
  axis_line = "none",
  order = 0L,
  position = waiver()
)
```

## Arguments

- key:

  A [segment
  key](https://teunbrand.github.io/legendry/reference/key_segments.md)
  specification. See more information in the linked topic.
  Alternatively, an object of class
  [`<hclust>`](https://rdrr.io/r/stats/hclust.html) that automatically
  invokes
  [`key_dendro()`](https://teunbrand.github.io/legendry/reference/key_segments.md).

- title:

  One of the following to indicate the title of the guide:

  - A `<character[1]>` or `<expression[1]>` to set a custom title.

  - `NULL` to not display any title.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    (default) to take the name of the scale object or the name specified
    in [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) as
    the title.

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- labels, ticks, axis_line:

  Guides to use as labels, ticks or axis lines. Can be specified as one
  of the following:

  - A `<logical[1]>` which when `FALSE` will set the guide to
    [`guide_none()`](https://ggplot2.tidyverse.org/reference/guide_none.html)
    and if `TRUE`, will set guide to appropriate primitive.

  - A `<Guide>` class object.

  - A `<function>` that returns a `<Guide>` class object.

  - A `<character[1]>` naming such a function, without the `guide_` or
    `primitive_` prefix.

- space:

  Either a [`<unit>`](https://rdrr.io/r/grid/unit.html) or
  [`<rel>`](https://ggplot2.tidyverse.org/reference/element.html) object
  of length 1 determining the space allocated in the orthogonal
  direction. When the `space` argument is of class `<rel>` (default) the
  base size is taken from the tick length theme setting.

- vanish:

  Only relevant when the guide is used in the secondary theta position:
  a `<logical[1]>` on whether the continue to draw the segments until
  they meed in the center (`TRUE`) or strictly observe the `space`
  setting (`FALSE`).

- n.dodge:

  An positive `<integer[1]>` setting the number of layers text labels
  can occupy to avoid overlapping labels.

- angle:

  A specification for the text angle. Compared to setting the `angle`
  argument in
  [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html),
  this argument uses some heuristics to automatically pick the `hjust`
  and `vjust` that you probably want. Can be one of the following:

  - `NULL` to take angles and justification settings directly from the
    theme.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html) to
    allow reasonable defaults in special cases.

  - A `<numeric[1]>` between -360 and 360 for the text angle in degrees.

- check.overlap:

  A `<logical[1]>` indicating whether to check for and omit overlapping
  text. If `TRUE`, first, last and middle labels are recursively
  prioritised in that order. If `FALSE`, all labels are drawn.

- order:

  A positive `<integer[1]>` that specifies the order of this guide among
  multiple guides. This controls in which order guides are merged if
  there are multiple guides for the same position. If `0` (default), the
  order is determined by a hashing indicative settings of a guide.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

## Value

A `<Guide>` object.

## Details

### Styling options

Because this guide is pure composite guide, the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
govern the styling are determined by its constituents. They are linked
below so you can find their 'Styling options' sections.

|  |  |
|----|----|
| **Primitive** | **Description** |
| [`compose_stack`](https://teunbrand.github.io/legendry/reference/compose_stack.md) | Stacks the lines, tick marks and labels and dendrogram. |
| [`primitive_segments()`](https://teunbrand.github.io/legendry/reference/primitive_segments.md) | The dendrogram. |
| [`primitive_line()`](https://teunbrand.github.io/legendry/reference/primitive_line.md) | Makes up the axis line. |
| [`primitive_ticks()`](https://teunbrand.github.io/legendry/reference/primitive_ticks.md) | Makes up the tick marks. |
| [`primitive_labels()`](https://teunbrand.github.io/legendry/reference/primitive_labels.md) | Makes up the labels. |

The context-agnostic alternative to using
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) is to
use
[`theme_guide()`](https://teunbrand.github.io/legendry/reference/theme_guide.md):

    guide_axis_dendro(theme = theme_guide(
      line = element_line(),
      text = element_text(),
      ticks = element_line(),
      ticks.length = unit(5, "mm"),
    ))

## See also

Other standalone guides:
[`guide_axis_base()`](https://teunbrand.github.io/legendry/reference/guide_axis_base.md),
[`guide_axis_nested()`](https://teunbrand.github.io/legendry/reference/guide_axis_nested.md),
[`guide_axis_plot()`](https://teunbrand.github.io/legendry/reference/guide_axis_plot.md),
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
# Hierarchically cluster data
clust <- hclust(dist(scale(mtcars)), "ave")

# Using the guide along with appropriate limits
p <- ggplot(mtcars, aes(disp, rownames(mtcars))) +
  geom_col() +
  scale_y_discrete(limits = clust$labels[clust$order])

# Standard usage
p + guides(y = guide_axis_dendro(clust))


# Adding ticks and axis line
p +
  guides(y = guide_axis_dendro(clust, ticks = "ticks", axis_line = "line")) +
  theme(axis.line = element_line())


# Controlling space allocated to dendrogram
p + guides(y = guide_axis_dendro(clust, space = unit(4, "cm"))) +
  theme(axis.ticks.y.left = element_line("red"))


# If want just the dendrogram, use `labels = FALSE`
p + guides(y = guide_axis_dendro(clust, labels = FALSE), y.sec = "axis")
```
