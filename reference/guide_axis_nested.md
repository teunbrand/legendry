# Nested axis guide

This axis guide gives extra range annotations to position scales. It can
be used to infer nesting structure from labels or annotate ranges.

## Usage

``` r
guide_axis_nested(
  key = "range_auto",
  regular_key = "auto",
  type = "bracket",
  title = waiver(),
  subtitle = NULL,
  theme = NULL,
  angle = waiver(),
  cap = "none",
  bidi = FALSE,
  oob = "squish",
  drop_zero = TRUE,
  pad_discrete = NULL,
  levels_text = NULL,
  ...,
  order = 0,
  position = waiver()
)
```

## Arguments

- key:

  One of the following:

  - A [range
    key](https://teunbrand.github.io/legendry/reference/key_range.md)
    specification. If not `key = "range_auto"`, additional labels will
    be inserted to represent point values.

  - A `<character[1]>` passed to the
    [`key_range_auto(sep)`](https://teunbrand.github.io/legendry/reference/key_range.md)
    argument. An exception is made when the string is a valid key
    specification.

- regular_key:

  A [standard
  key](https://teunbrand.github.io/legendry/reference/key_standard.md)
  specification for the appearance of regular tick marks.

- type:

  Appearance of ranges, either `"box"` to put text in boxes or
  `"bracket"` (default) to text brackets.

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

- ...:

  Arguments passed on to
  [`primitive_bracket()`](https://teunbrand.github.io/legendry/reference/primitive_bracket.md),
  [`primitive_box()`](https://teunbrand.github.io/legendry/reference/primitive_box.md)
  or
  [`primitive_fence()`](https://teunbrand.github.io/legendry/reference/primitive_fence.md)
  (depending on the `type` argument).

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
[ticks](https://teunbrand.github.io/legendry/reference/primitive_ticks.md),
optionally
[labels](https://teunbrand.github.io/legendry/reference/primitive_labels.md)
and either
[bracket](https://teunbrand.github.io/legendry/reference/primitive_bracket.md),
[box](https://teunbrand.github.io/legendry/reference/primitive_box.md)
or
[fence](https://teunbrand.github.io/legendry/reference/primitive_fence.md)
primitives.

By default, the
[`key = "range_auto"`](https://teunbrand.github.io/legendry/reference/key_range.md)
will incorporate the 0th level labels inferred from the scale's labels.
These labels will look like regular labels.

To offer other keys the opportunity to display ranges alongside
regular-looking labels, the `regular_key` argument can be used to setup
a separate key for display in between the ticks and ranges.

## See also

Other standalone guides:
[`guide_axis_base()`](https://teunbrand.github.io/legendry/reference/guide_axis_base.md),
[`guide_axis_dendro()`](https://teunbrand.github.io/legendry/reference/guide_axis_dendro.md),
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
# A plot with nested categories on the x-axis
p <- ggplot(mpg, aes(interaction(drv, cyl), hwy)) +
  geom_boxplot()

p + guides(x = "axis_nested")


# Apply styling to brackets
p + guides(x = "axis_nested") +
  theme_guide(bracket = element_line("red", linewidth = 1))


# Don't drop nesting indicators that have 0-width
p + guides(x = guide_axis_nested(drop_zero = FALSE))


# Change additional padding for discrete categories
p + guides(x = guide_axis_nested(pad_discrete = 0))


# Change bracket type
p + guides(x = guide_axis_nested(bracket = "curvy"))


# Use boxes instead of brackets + styling of boxes
p + guides(x = guide_axis_nested(type = "box")) +
  theme_guide(box = element_rect("limegreen", "forestgreen"))


# Using fences instead of brackets + styling of fences
p + guides(x = guide_axis_nested(type = "fence", rail = "inner")) +
  theme_guide(
    fence.post = element_line("tomato"),
    fence.rail = element_line("dodgerblue")
  )


# Use as annotation of a typical axis
# `regular_key` controls display of typical axis
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  guides(x = guide_axis_nested(
    key = key_range_manual(start = 2:3, end = 5:6, name = c("First", "Second")),
    regular_key = key_manual(c(2, 2.5, 3, 5, 7))
  ))
```
