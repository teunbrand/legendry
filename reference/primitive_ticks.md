# Guide primitive: line

This function constructs a ticks [guide
primitive](https://teunbrand.github.io/legendry/reference/guide-primitives.md).

## Usage

``` r
primitive_ticks(key = NULL, bidi = FALSE, theme = NULL, position = waiver())
```

## Arguments

- key:

  A [standard
  key](https://teunbrand.github.io/legendry/reference/key_standard.md)
  specification. See more information in the linked topic.

- bidi:

  A `<logical[1]>`: whether ticks should be drawn bidirectionally
  (`TRUE`) or in a single direction (`FALSE`, default).

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

## Value

A `PrimitiveTicks` primitive guide that can be used inside other guides.

## Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide, which may differ depending on
whether the guide is used in an axis or in a legend context.

Common to both types is the following:

### As an axis guide

- `axis.ticks.{x/y}.{position}` an
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  for major tick lines.

- `axis.minor.ticks.{x/y}.{position}` an
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  for minor tick lines.

- `legendry.axis.mini.ticks` an
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  internally inheriting from the minor ticks for the smallest ticks in
  e.g. log axes.

- `axis.ticks.length.{x/y}.{position}` a
  [`<unit>`](https://rdrr.io/r/grid/unit.html) for the major ticks
  length.

- `axis.minor.ticks.length.{x/y}.{position}` a
  [`<unit>`](https://rdrr.io/r/grid/unit.html) for the minor ticks
  length.

- `legendry.axis.mini.ticks.length` a
  [`<unit>`](https://rdrr.io/r/grid/unit.html) internally inheriting
  from the minor tick length for the smallest ticks in e.g. log axes.

### As a legend guide

- `legend.ticks` an
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  for major tick lines.

- `legendry.legend.minor.ticks` an
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  for minor tick lines.

- `legendry.legend.mini.ticks` an
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  for the smallest ticks in e.g. log axes.

- `legend.ticks.length` a [`<unit>`](https://rdrr.io/r/grid/unit.html)
  for the major ticks length.

- `legendry.legend.minor.ticks.length` a
  [`<unit>`](https://rdrr.io/r/grid/unit.html) for the minor ticks
  length.

- `legendry.legend.mini.ticks.length` a
  [`<unit>`](https://rdrr.io/r/grid/unit.html) for the smallest ticks in
  e.g. log axes.

## See also

Other primitives:
[`primitive_box()`](https://teunbrand.github.io/legendry/reference/primitive_box.md),
[`primitive_bracket()`](https://teunbrand.github.io/legendry/reference/primitive_bracket.md),
[`primitive_fence()`](https://teunbrand.github.io/legendry/reference/primitive_fence.md),
[`primitive_labels()`](https://teunbrand.github.io/legendry/reference/primitive_labels.md),
[`primitive_line()`](https://teunbrand.github.io/legendry/reference/primitive_line.md),
[`primitive_segments()`](https://teunbrand.github.io/legendry/reference/primitive_segments.md),
[`primitive_spacer()`](https://teunbrand.github.io/legendry/reference/primitive_spacer.md),
[`primitive_title()`](https://teunbrand.github.io/legendry/reference/primitive_title.md)

## Examples

``` r
# A standard plot
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point()

# Adding as secondary guides
p + guides(x.sec = primitive_ticks(), y.sec = primitive_ticks())
```
