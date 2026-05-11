# Guide primitive: title

This function constructs a title [guide
primitive](https://teunbrand.github.io/legendry/reference/guide-primitives.md).

## Usage

``` r
primitive_title(
  title = waiver(),
  angle = waiver(),
  theme = NULL,
  position = waiver()
)
```

## Arguments

- title:

  One of the following to indicate the title of the guide:

  - A `<character[1]>` or `<expression[1]>` to set a custom title.

  - `NULL` to not display any title.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    (default) to take the name of the scale object or the name specified
    in [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) as
    the title.

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

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

## Value

A `<PrimitiveTitle>` primitive guide that can be used inside other
guides.

## Details

### Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide, which may differ depending on
whether the guide is used in an axis or in a legend context.

|  |  |  |  |
|----|----|----|----|
| **Theme setting** | **Context** | **Type** | **Description** |
| `axis.title.{x/y}.{position}` | Axis | [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html) | The title itself. |
| `legend.title` | Legend | [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html) | The title itself. |

There are no further styling options.

The context-agnostic alternative to using
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) is to
use
[`theme_guide()`](https://teunbrand.github.io/legendry/reference/theme_guide.md):

    primitive_title(theme = theme_guide(
      title = element_text(),
    ))

## See also

Other primitives:
[`primitive_box()`](https://teunbrand.github.io/legendry/reference/primitive_box.md),
[`primitive_bracket()`](https://teunbrand.github.io/legendry/reference/primitive_bracket.md),
[`primitive_fence()`](https://teunbrand.github.io/legendry/reference/primitive_fence.md),
[`primitive_labels()`](https://teunbrand.github.io/legendry/reference/primitive_labels.md),
[`primitive_line()`](https://teunbrand.github.io/legendry/reference/primitive_line.md),
[`primitive_segments()`](https://teunbrand.github.io/legendry/reference/primitive_segments.md),
[`primitive_spacer()`](https://teunbrand.github.io/legendry/reference/primitive_spacer.md),
[`primitive_ticks()`](https://teunbrand.github.io/legendry/reference/primitive_ticks.md)

## Examples

``` r
# A standard plot
p <- ggplot(mpg, aes(displ, hwy)) +
 geom_point()

# Adding as secondary guides
p + guides(
  x.sec = primitive_title("Horizontal Title"),
  y.sec = primitive_title(c("along vertical", "Multiple tiles"))
)


# 'Real' titles occur once per plot.
# Primitive titles repeat over facets and hide the 'real' title.
p + facet_wrap(~ drv) +
  guides(x = primitive_title("I am a repeated subtitle")) +
  labs(x = "I am the hidden real title")
```
