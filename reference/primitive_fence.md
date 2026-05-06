# Guide primitive: fence

This function constructs a fence [guide
primitive](https://teunbrand.github.io/legendry/reference/guide-primitives.md).
The customisation options are easier to understand if we view fence
'post' as the vertical pieces of a real world fence, and the 'rail' as
the horizontal pieces.

## Usage

``` r
primitive_fence(
  key = "range_auto",
  rail = "none",
  angle = waiver(),
  oob = "squish",
  drop_zero = TRUE,
  pad_discrete = 0.5,
  levels_text = NULL,
  levels_post = NULL,
  levels_rail = NULL,
  theme = NULL,
  position = waiver()
)
```

## Arguments

- key:

  A [range
  key](https://teunbrand.github.io/legendry/reference/key_range.md)
  specification. See more information in the linked topic.

- rail:

  A `<character[1]>` giving an option for how to display fence railing.
  Can be either `"none"` (default) to display no railings, `"inner"` to
  draw one rail closer to the plot panel, `"outer"` to display one rail
  farther from the plot panel, or `"both"` to sandwich the labels
  between rails.

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

- oob:

  A method for dealing with out-of-bounds (oob) ranges. Can be one of
  `"squish"`, `"censor"` or `"none"`.

- drop_zero:

  A `<logical[1]>` whether to drop near-zero width ranges (`TRUE`,
  default) or preserve them (`FALSE`).

- pad_discrete:

  A `<numeric[1]>` giving the amount ranges should be extended when
  given as a discrete variable. This is applied after the `drop_zero`
  setting.

- levels_text:

  A list of `<element_text>` objects to customise how text appears at
  every level.

- levels_post, levels_rail:

  A list of `<element_line>` objects to customise how fence posts and
  rails are displayed at every level.

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

## Value

A `<PrimitiveFence>` primitive guie that can be used inside other
guides.

## Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide, which may differ depending on
whether the guide is used in an axis or legend context.

Common to both types is the following:

- `legendry.fence.post` an
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  for the line used to draw the pieces orthogonal to the direction of
  the scale.

- `legendry.fence.rail` an
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  for the line used to draw the pieces parallel to the direction of the
  scale.

### As an axis guide

- `axis.text.{x/y}.{position}` an
  [`<element_text>`](https://ggplot2.tidyverse.org/reference/element.html)
  for the text displayed.

### As a legend guide

- `legend.text` an
  [`<element_text>`](https://ggplot2.tidyverse.org/reference/element.html)
  for the text displayed.

## See also

Other primitives:
[`primitive_box()`](https://teunbrand.github.io/legendry/reference/primitive_box.md),
[`primitive_bracket()`](https://teunbrand.github.io/legendry/reference/primitive_bracket.md),
[`primitive_labels()`](https://teunbrand.github.io/legendry/reference/primitive_labels.md),
[`primitive_line()`](https://teunbrand.github.io/legendry/reference/primitive_line.md),
[`primitive_segments()`](https://teunbrand.github.io/legendry/reference/primitive_segments.md),
[`primitive_spacer()`](https://teunbrand.github.io/legendry/reference/primitive_spacer.md),
[`primitive_ticks()`](https://teunbrand.github.io/legendry/reference/primitive_ticks.md),
[`primitive_title()`](https://teunbrand.github.io/legendry/reference/primitive_title.md)

## Examples

``` r
# A standard plot
p <- ggplot(mpg, aes(interaction(drv, year), displ)) +
  geom_point()

key <- key_range_manual(c(2, 4), c(5, 6), c("A", "B"))

# Adding as secondary guides
p + guides(
  x.sec = primitive_fence(rail = "inner"),
  y.sec = primitive_fence(key = key, rail = "outer")
)
```
