# Guide primitive: labels

This function constructs a labels [guide
primitive](https://teunbrand.github.io/legendry/reference/guide-primitives.md).

## Usage

``` r
primitive_labels(
  key = NULL,
  angle = waiver(),
  n.dodge = 1,
  check.overlap = FALSE,
  theme = NULL,
  position = waiver()
)
```

## Arguments

- key:

  A [standard
  key](https://teunbrand.github.io/legendry/reference/key_standard.md)
  specification. See more information in the linked topic.

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

- n.dodge:

  An positive `<integer[1]>` setting the number of layers text labels
  can occupy to avoid overlapping labels.

- check.overlap:

  A `<logical[1]>` indicating whether to check for and omit overlapping
  text. If `TRUE`, first, last and middle labels are recursively
  prioritised in that order. If `FALSE`, all labels are drawn.

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

## Value

A `<PrimitiveLabels>` primitive guide that can be used inside other
guides.

## Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide, which may differ depending on
whether the guide is used in an axis or in a legend context.

### As an axis guide

- `axis.text.{x/y}.{position}` an
  [`<element_text>`](https://ggplot2.tidyverse.org/reference/element.html)
  for the display of the labels.

### As a legend guide.

- `legend.text` an
  [`<element_text>`](https://ggplot2.tidyverse.org/reference/element.html)
  for the display of the labels.

## See also

Other primitives:
[`primitive_box()`](https://teunbrand.github.io/legendry/reference/primitive_box.md),
[`primitive_bracket()`](https://teunbrand.github.io/legendry/reference/primitive_bracket.md),
[`primitive_fence()`](https://teunbrand.github.io/legendry/reference/primitive_fence.md),
[`primitive_line()`](https://teunbrand.github.io/legendry/reference/primitive_line.md),
[`primitive_segments()`](https://teunbrand.github.io/legendry/reference/primitive_segments.md),
[`primitive_spacer()`](https://teunbrand.github.io/legendry/reference/primitive_spacer.md),
[`primitive_ticks()`](https://teunbrand.github.io/legendry/reference/primitive_ticks.md),
[`primitive_title()`](https://teunbrand.github.io/legendry/reference/primitive_title.md)

## Examples

``` r
# A standard plot
p <- ggplot(mpg, aes(displ, hwy)) +
 geom_point()

# Adding as secondary guides
p + guides(
  x.sec = primitive_labels(),
  y.sec = primitive_labels(n.dodge = 2)
)
```
