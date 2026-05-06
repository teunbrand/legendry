# Guide primitive: brackets

This function constructs a brackets [guide
primitive](https://teunbrand.github.io/legendry/reference/guide-primitives.md).

## Usage

``` r
primitive_bracket(
  key = "range_auto",
  bracket = "line",
  angle = waiver(),
  oob = "squish",
  drop_zero = TRUE,
  pad_discrete = 0.4,
  levels_brackets = NULL,
  levels_text = NULL,
  theme = NULL,
  position = waiver()
)
```

## Arguments

- key:

  A [range
  key](https://teunbrand.github.io/legendry/reference/key_range.md)
  specification. See more information in the linked topic.

- bracket:

  A
  [bracket](https://teunbrand.github.io/legendry/reference/bracket_options.md)
  by providing one of the following:

  - A bracket `<function>`, such as `bracket_square`.

  - A `<character[1]>` naming a bracket function without the
    '`bracket_`'-prefix, e.g. `"square"`.

  - A two-column `<matrix[n, 2]>` giving line coordinates for a bracket,
    like those created by bracket functions, such as
    [`bracket_round()`](https://teunbrand.github.io/legendry/reference/bracket_options.md).

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

- levels_brackets:

  A list of `<element_line>` objects to customise how brackets appear at
  every level.

- levels_text:

  A list of `<element_text>` objects to customise how text appears at
  every level.

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

## Value

A `<PrimitiveBracket>` primitive guide that can be used inside other
guides.

## Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide, which may differ depending on
whether the guide is used in an axis or a legend context.

Common to both types is the following:

- `legendry.bracket` an
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  for the line used to draw the brackets.

- `legendry.backet.size` a [`<unit>`](https://rdrr.io/r/grid/unit.html)
  setting the space afforded to a bracket.

### As an axis guide

- `axis.text.{x/y}.{position}` an
  [`<element_text>`](https://ggplot2.tidyverse.org/reference/element.html)
  for the text displayed over the brackets.

### As a legend guide

- `legend.text` an
  [`<element_text>`](https://ggplot2.tidyverse.org/reference/element.html)
  for the text displayed over the brackets.

## See also

Other primitives:
[`primitive_box()`](https://teunbrand.github.io/legendry/reference/primitive_box.md),
[`primitive_fence()`](https://teunbrand.github.io/legendry/reference/primitive_fence.md),
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
  x.sec = primitive_bracket(),
  y.sec = primitive_bracket(key = key)
)
```
