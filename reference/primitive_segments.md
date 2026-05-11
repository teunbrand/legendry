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

## Details

### Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide, which may differ depending on
whether the guide is used in an axis or in a legend context.

The possible `{position}` suffixes mentioned below are `x`, `x.top`,
`x.bottom`, `y`, `y.left`, `y.right`. The `theta` and `r` position
suffixes in ggplot2 are *not* obeyed in legendry.

|  |  |  |  |
|----|----|----|----|
| **Theme setting** | **Context** | **Type** | **Description** |
| `axis.ticks.{position}` | Axis | [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html) | The line segments. |
| `axis.ticks.length.{position}` | Axis | [`unit()`](https://rdrr.io/r/grid/unit.html) | Basis for the `space` argument |
| `legend.ticks` | Legend | [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html) | The line segments |
| `legend.ticks.length` | Legend | [`unit()`](https://rdrr.io/r/grid/unit.html) | Basis for the `space` argument |

Styling options *per segment* can be set in the [segment
key](https://teunbrand.github.io/legendry/reference/key_segments.md).
The `line` prefixed properties are prioritised for segments. These
override theme settings.

The context-agnostic alternative to using
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) is to
use
[`theme_guide()`](https://teunbrand.github.io/legendry/reference/theme_guide.md):

    primitive_segments(theme = theme_guide(
      ticks = element_line(),
      ticks.length = unit(5, "mm")
    ))

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
