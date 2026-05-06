# Guide primitives: segments

This function constructs a [guide
primitive](https://teunbrand.github.io/legendry/reference/guide-primitives.md).

## Usage

``` r
primitive_segments(
  key = NULL,
  space = rel(10),
  vanish = FALSE,
  theme = NULL,
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

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

## Value

A `<PrimitiveSegments>` primitive guide that can be used inside other
guides.

## Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the style of this guide, which may differ depending on whether
the guide is used in an axis or in a legend context.

### As an axis guide

- `axis.ticks.{x/y}.{position}` an
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  for display of the segments.

- `axis.ticks.length.{x/y}.{position}` a
  [`<unit>`](https://rdrr.io/r/grid/unit.html) for the base size of the
  segments in the orthogonal direction.

### As a legend guide

- `legend.ticks` an
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  for display of the segments.

- `legend.ticks.length` a [`<unit>`](https://rdrr.io/r/grid/unit.html)
  for the base size of the segments in the orthogonal direction.

## See also

Other primitives:
[`primitive_box()`](https://teunbrand.github.io/legendry/reference/primitive_box.md),
[`primitive_bracket()`](https://teunbrand.github.io/legendry/reference/primitive_bracket.md),
[`primitive_fence()`](https://teunbrand.github.io/legendry/reference/primitive_fence.md),
[`primitive_labels()`](https://teunbrand.github.io/legendry/reference/primitive_labels.md),
[`primitive_line()`](https://teunbrand.github.io/legendry/reference/primitive_line.md),
[`primitive_spacer()`](https://teunbrand.github.io/legendry/reference/primitive_spacer.md),
[`primitive_ticks()`](https://teunbrand.github.io/legendry/reference/primitive_ticks.md),
[`primitive_title()`](https://teunbrand.github.io/legendry/reference/primitive_title.md)

## Examples

``` r
# Building a key
key <- key_segment_manual(
  value     = c(1.6, 1.6, 3.4, 5.2),
  value_end = c(7.0, 7.0, 3.4, 5.2),
  oppo      = c(1.0, 2.0, 0.0, 0.0),
  oppo_end  = c(1.0, 2.0, 3.0, 3.0)
)

# Using the primitive in a plot
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_x_continuous(
    guide = primitive_segments(key = key)
  )
```
