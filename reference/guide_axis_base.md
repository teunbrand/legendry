# Custom axis guide

This axis guide is a visual representation of position scales and can
represent the `x`, `y`, `theta` and `r` aesthetics. It differs from
[`guide_axis()`](https://ggplot2.tidyverse.org/reference/guide_axis.html)
in that it can accept custom keys and is can act as an axis for
[`coord_radial()`](https://ggplot2.tidyverse.org/reference/coord_radial.html)
like
[`guide_axis_theta()`](https://ggplot2.tidyverse.org/reference/guide_axis_theta.html).

## Usage

``` r
guide_axis_base(
  key = NULL,
  title = waiver(),
  subtitle = NULL,
  theme = NULL,
  n.dodge = 1L,
  check.overlap = FALSE,
  angle = waiver(),
  cap = "none",
  bidi = FALSE,
  order = 0L,
  position = waiver()
)
```

## Arguments

- key:

  A [standard
  key](https://teunbrand.github.io/legendry/reference/key_standard.md)
  specification. Defaults to
  [`key_auto()`](https://teunbrand.github.io/legendry/reference/key_standard.md).
  See more information in the linked topic and the 'Details' section.

- title:

  One of the following to indicate the title of the guide:

  - A `<character[1]>` or `<expression[1]>` to set a custom title.

  - `NULL` to not display any title.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    (default) to take the name of the scale object or the name specified
    in [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) as
    the title.

- subtitle:

  Passed on to
  [`primitive_title(title)`](https://teunbrand.github.io/legendry/reference/primitive_title.md).
  Follow the linked topic for more details.

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- n.dodge:

  An positive `<integer[1]>` setting the number of layers text labels
  can occupy to avoid overlapping labels.

- check.overlap:

  A `<logical[1]>` indicating whether to check for and omit overlapping
  text. If `TRUE`, first, last and middle labels are recursively
  prioritised in that order. If `FALSE`, all labels are drawn.

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

- cap:

  A method to cap the axes. One of the following:

  - A `<character[1]>` with one of the following:

    - `"none"` to perform no capping.

    - `"both"` to cap the line at both ends at the most extreme breaks.

    - `"upper"` to cap the line at the upper extreme break.

    - `"lower"` to cap the line at the lower extreme break.

  - A `<logical>[1]`, where `TRUE` is equivalent to `"both"` and `FALSE`
    is equivalent to `"none"` in the options above.

  - A sorted `<numeric>[2n]` with an even number of members. The lines
    will be drawn between every odd-even pair.

  - A `<function>` that takes the scale's breaks as the first argument,
    the scale's limits as the second argument and returns a
    `<numeric>[2n]` as described above.

- bidi:

  A `<logical[1]>`: whether ticks should be drawn bidirectionally
  (`TRUE`) or in a single direction (`FALSE`, default).

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

Under the hood, this guide is a [stack
composition](https://teunbrand.github.io/legendry/reference/compose_stack.md)
of a
[line](https://teunbrand.github.io/legendry/reference/primitive_line.md),
[ticks](https://teunbrand.github.io/legendry/reference/primitive_ticks.md)
and
[labels](https://teunbrand.github.io/legendry/reference/primitive_labels.md)
primitives.

To set minor ticks, use `key = "minor"`, or use the `type` argument in
[`key_manual()`](https://teunbrand.github.io/legendry/reference/key_standard.md)
or
[`key_map()`](https://teunbrand.github.io/legendry/reference/key_standard.md).

To use this as a logarithmic axis, set `key = "log"`.

### Styling options

Because this guide is pure composite guide, the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
govern the styling are determined by its constituents. They are linked
below so you can find their 'Styling options' sections.

|  |  |
|----|----|
| **Primitive** | **Description** |
| [`compose_stack`](https://teunbrand.github.io/legendry/reference/compose_stack.md) | Stacks the lines, tick marks and labels. |
| [`primitive_line()`](https://teunbrand.github.io/legendry/reference/primitive_line.md) | Makes up the axis line. |
| [`primitive_ticks()`](https://teunbrand.github.io/legendry/reference/primitive_ticks.md) | Makes up the tick marks. |
| [`primitive_labels()`](https://teunbrand.github.io/legendry/reference/primitive_labels.md) | Makes up the labels. |

Styling options *per break* can be set in the [standard
key](https://teunbrand.github.io/legendry/reference/key_standard.md).
These override theme settings.

The context-agnostic alternative to using
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) is to
use
[`theme_guide()`](https://teunbrand.github.io/legendry/reference/theme_guide.md):

    guide_axis_base(theme = theme_guide(
      # Common options
      line = element_line(),
      text = element_text(),
      ticks = element_line(),
      ticks.length = unit(5, "mm"),

      # Niche options below
      minor.ticks = element_line(),
      minor.ticks.length = unit(5, "mm"),
      mini.ticks = element_line(),
      mini.ticks.length = unit(5, "mm"),
    ))

## See also

Other standalone guides:
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
[`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md),
[`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)

## Examples

``` r
# A standard plot with custom keys
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(
    guide = guide_axis_base(key = key_minor())
  ) +
  scale_y_continuous(
    guide = guide_axis_base(key = key_manual(c(20, 25, 30, 40)))
  )
p


# Is translated to theta axis without fuss
p + coord_radial()


# To use as logarithmic axis:
ggplot(msleep, aes(bodywt, brainwt)) +
  geom_point(na.rm = TRUE) +
  scale_x_continuous(
    transform = "log10",
    guide = guide_axis_base("log")
  )
```
