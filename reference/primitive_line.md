# Guide primitive: line

This function constructs a line [guide
primitive](https://teunbrand.github.io/legendry/reference/guide-primitives.md).

## Usage

``` r
primitive_line(key = NULL, cap = "none", theme = NULL, position = waiver())
```

## Arguments

- key:

  A [standard
  key](https://teunbrand.github.io/legendry/reference/key_standard.md)
  specification. See more information in the linked topic.

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

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

## Value

A `PrimitiveLine` primitive guide that can be used inside other guides.

## Details

### Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide, which may differ depending on
whether the guide is used in an axis or in a legend context.

|  |  |  |  |
|----|----|----|----|
| **Theme setting** | **Context** | **Type** | **Description** |
| `axis.line.{x/y}.{position}` | Axis | [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html) | The axis line. |
| `legend.axis.line` | Legend | [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html) | The axis line. |

There are no other styling options. The context-agnostic alternative to
using [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) is
to use
[`theme_guide()`](https://teunbrand.github.io/legendry/reference/theme_guide.md):

    primitive_line(theme = theme_guide(
      line = element_line()
    ))

## See also

Other primitives:
[`primitive_box()`](https://teunbrand.github.io/legendry/reference/primitive_box.md),
[`primitive_bracket()`](https://teunbrand.github.io/legendry/reference/primitive_bracket.md),
[`primitive_fence()`](https://teunbrand.github.io/legendry/reference/primitive_fence.md),
[`primitive_labels()`](https://teunbrand.github.io/legendry/reference/primitive_labels.md),
[`primitive_segments()`](https://teunbrand.github.io/legendry/reference/primitive_segments.md),
[`primitive_spacer()`](https://teunbrand.github.io/legendry/reference/primitive_spacer.md),
[`primitive_ticks()`](https://teunbrand.github.io/legendry/reference/primitive_ticks.md),
[`primitive_title()`](https://teunbrand.github.io/legendry/reference/primitive_title.md)

## Examples

``` r
# A standard plot
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  theme(axis.line = element_line())

# Adding as secondary guides
p + guides(
  x.sec = primitive_line(),
  y.sec = primitive_line(cap = "both")
)
```
