# Annotation axis guide

**\[experimental\]**

This axis guide acts as annotation: it draws labels at specified places.
It also wraps an inner guide, making the behaviour look like one is
'adding' the annotation on top of the regular guide.

## Usage

``` r
guide_axis_annotation(
  aesthetic,
  label = as.character(aesthetic),
  ...,
  key = NULL,
  arrow = NULL,
  inner = waiver(),
  title = waiver(),
  theme = NULL,
  order = 0L,
  position = waiver(),
  call = NULL
)

annotate_top(..., position = "top")

annotate_right(..., position = "right")

annotate_bottom(..., position = "bottom")

annotate_left(..., position = "left")
```

## Arguments

- aesthetic:

  A vector of values for the guide to represent.

- label:

  A `<character[n]>` or list of expressions to use as labels.

- ...:

  Additional graphical properties parallel to `aesthetic`. Can be
  `text_colour`, `size`, `face`, `hjust`, `vjust`, `angle` or
  `lineheight` for the labels. Can be `line_colour`, `linewidth` and
  `linetype` for ticks. Setting `colour` will also set `text_colour` and
  `line_colour`. For `annotate_top()`, `annotate_right()`,
  `annotate_bottom()` and `annotate_left()`, arguments are passed on to
  `guide_axis_annotation()`.

- key:

  A [standard
  key](https://teunbrand.github.io/legendry/reference/key_standard.md)
  overriding the `aesthetic`, `label` and `...` arguments.

- arrow:

  A [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html) specification.
  Can be `NULL` (default) to draw no arrow.

- inner:

  A guide that supports the aesthetic to draw the annotation across.
  When [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
  (default), populates a
  [`guide_axis_base()`](https://teunbrand.github.io/legendry/reference/guide_axis_base.md)
  except in the `"top"`, `"right"` and `"theta.sec"` positions.

- title:

  One of the following to indicate the title of the guide:

  - A `<character[1]>` or `<expression[1]>` to set a custom title.

  - `NULL` to not display any title.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    (default) to take the name of the scale object or the name specified
    in [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) as
    the title.

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the *annotation* part of this guide differently from
  the plot's theme settings. The `theme` argument in this guide
  overrides and is combined with the plot's theme.

- order:

  A positive `<integer[1]>` that specifies the order of this guide among
  multiple guides. This controls in which order guides are merged if
  there are multiple guides for the same position. If `0` (default), the
  order is determined by a hashing indicative settings of a guide.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

- call:

  A [call](https://rlang.r-lib.org/reference/topic-error-call.html) to
  display in messages.

## Value

A `<Guide>` object

## Details

Under the hood, this guide is a hybrid composition guide. The
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) options
that govern the styling are partially determined by its consituents.
They are linked below so you can find their 'Styling options' sections.

\| **Constituent** \| **Description** \| \|
[`compose_ontop()`](https://teunbrand.github.io/legendry/reference/compose_ontop.md)
\| Composes the annotation on top of the `inner` guide \| \|
[`guide_axis_base()`](https://teunbrand.github.io/legendry/reference/guide_axis_base.md)
\| The default `inner` guide \| \|
[`primitive_ticks()`](https://teunbrand.github.io/legendry/reference/primitive_ticks.md)
\| Display of the annotation ticks \| \|
[`primitive_labels()`](https://teunbrand.github.io/legendry/reference/primitive_labels.md)
\| Display of the annotation labels \|

Styling options *per annotation* can be set via `...` as described
above.

The context-agnostic alternative to using
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) is to
use
[`theme_guide()`](https://teunbrand.github.io/legendry/reference/theme_guide.md):

    # Note that `theme` is used *only* for the annotation
    guide_axis_annotation(theme = theme_guide(
      text = element_text(),
      ticks = element_line(),
      ticks.length = unit(5, "mm")
    ))

## See also

Other standalone guides:
[`guide_axis_base()`](https://teunbrand.github.io/legendry/reference/guide_axis_base.md),
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
# Basic plot
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point()

# Typical use
p +
  annotate_top(5, face = "bold") +
  annotate_bottom(c(2.5, 4.5), c("Bottom annotation", "Second label"))


# If you want to combine them with secondary axes, you must
# set the `inner` argument manually.
p +
  scale_y_continuous(
    sec.axis = dup_axis(breaks = c(15, 25, 35))
  ) +
  annotate_right(30, inner = "axis")


# Use in theta axis
p + coord_radial() +
  guides(theta = guide_axis_annotation(4.5, "Theta annotation"))


# Specialised use as part of other guides
# Note that `guide_colbar` imposes white inward ticks
# We can overrule these impositions with a replacement theme
p + aes(colour = cty) +
  guides(colour = guide_colbar(
    second_guide = guide_axis_annotation(22, "This", theme = theme_gray())
  ))
```
