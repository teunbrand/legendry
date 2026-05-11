# Circle size guide

This guide displays the sizes of points as a series of circles. It is
typically paired with
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
with
[`draw_key_point()`](https://ggplot2.tidyverse.org/reference/draw_key.html)
glyphs.

## Usage

``` r
guide_circles(
  key = NULL,
  title = waiver(),
  theme = NULL,
  hjust = 0.5,
  vjust = 0,
  text_position = NULL,
  clip_text = FALSE,
  override.aes = list(shape = 1L),
  position = waiver(),
  direction = NULL
)
```

## Arguments

- key:

  A [standard
  key](https://teunbrand.github.io/legendry/reference/key_standard.md)
  specification. Defaults to
  [`key_auto()`](https://teunbrand.github.io/legendry/reference/key_standard.md).
  See more information in the linked topic.

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
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- hjust, vjust:

  A `<numeric[1]>` between 0 and 1 giving the horizontal and vertical
  justification, respectively, of the central shapes. It is recommended
  `hjust = 0.5` when text is placed on the left or right and
  `vjust = 0.5` is recommended when text is placed on top or in the
  bottom.

- text_position:

  A string, one of `"ontop"`, `"top"`, `"right"`, `"bottom"`, or
  `"left"` do describe the placement of labels. The default (`NULL`),
  will take the `legend.text.position` theme setting.

- clip_text:

  A `<logical[1]>` whether to give text in the `"ontop"` position a
  small rectangle of background colour.

- override.aes:

  A named `<list>` specifying aesthetic parameters of the key glyphs.
  See details and examples in
  [`guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html).

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

- direction:

  A `<character[1]>` indicating the direction of the guide. Can be on of
  `"horizontal"` or `"vertical"`.

## Value

A `<GuideCircles>` object.

## Details

Please note that the default size scales scale to area, not radius, so
equidistant breaks will appear at irregularly spaced positions due to
labelling the diameter of a circle.

This graph was designed with standard round
[shapes](https://ggplot2.tidyverse.org/reference/scale_shape.html) in
mind, i.e. shapes 1, 16, 19 and 21. For that reason, `shape = 1` is the
default `override.aes` argument. Other shapes will probably be drawn but
the quality of their alignment and label placement may be
unsatisfactory.

### Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide.

|  |  |  |
|----|----|----|
| **Theme setting** | **Type** | **Description** |
| `legend.background` | [`element_rect()`](https://ggplot2.tidyverse.org/reference/element.html) | Background of the legend. |
| `legend.margin` | [`margin()`](https://ggplot2.tidyverse.org/reference/element.html) | Padding around the legend. |
| `legend.key` | [`element_rect()`](https://ggplot2.tidyverse.org/reference/element.html) | Background of the key area underneath circles. |
| `legend.text` | [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html) | Labels displayed next to tick marks or on top of circles. |
| `legendry.legend.key.margin` | [`margin()`](https://ggplot2.tidyverse.org/reference/element.html) | Padding between circles and edge of key area. |
| `legend.ticks` | [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html) | Tick marks connecting circle to label. |
| `legend.title` | [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html) | Title of the legend. |
| `legend.title.position` | `<character[1]>` | One of `"top"`, `"right"`, `"bottom"` or `"left"`. |

Styling options *per break* can be set in the [standard
key](https://teunbrand.github.io/legendry/reference/key_standard.md).
These override theme settings.

The context-agnostic alternative to using
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) is to
use
[`theme_guide()`](https://teunbrand.github.io/legendry/reference/theme_guide.md):

    guide_circles(theme = theme_guide(
      text = element_text(),
      title = element_text(),
      title.position = "top",
      margin = margin(5),
      key = element_rect(),
      key.margin = margin(5),
      background = element_rect(),
      ticks = element_line(),
    ))

## See also

Other standalone guides:
[`guide_axis_annotation()`](https://teunbrand.github.io/legendry/reference/guide_axis_annotation.md),
[`guide_axis_base()`](https://teunbrand.github.io/legendry/reference/guide_axis_base.md),
[`guide_axis_dendro()`](https://teunbrand.github.io/legendry/reference/guide_axis_dendro.md),
[`guide_axis_nested()`](https://teunbrand.github.io/legendry/reference/guide_axis_nested.md),
[`guide_axis_plot()`](https://teunbrand.github.io/legendry/reference/guide_axis_plot.md),
[`guide_axis_symbols()`](https://teunbrand.github.io/legendry/reference/guide_axis_symbols.md),
[`guide_colbar()`](https://teunbrand.github.io/legendry/reference/guide_colbar.md),
[`guide_colring()`](https://teunbrand.github.io/legendry/reference/guide_colring.md),
[`guide_colsteps()`](https://teunbrand.github.io/legendry/reference/guide_colsteps.md),
[`guide_legend_base()`](https://teunbrand.github.io/legendry/reference/guide_legend_base.md),
[`guide_legend_cross()`](https://teunbrand.github.io/legendry/reference/guide_legend_cross.md),
[`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md),
[`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)

## Examples

``` r
# A standard plot
p <- ggplot(mtcars, aes(disp, mpg)) +
  geom_point(aes(size = hp), alpha = 0.3)

# By default, the sizes aren't large enough to make this guide clear
p + scale_size_area(guide = "circles")


# Update with a more approrpriate scale
p <- p +
  scale_size_area(
    max_size = 30,
    limits = c(0, NA),
    breaks = c(0, 25, 100, 250)
  )
p + guides(size = "circles")


# Horizontal orientation
p + guides(size = guide_circles(
  vjust = 0.5, hjust = 0, text_position = "bottom"
))


# Alternative text placement
p + guides(size = guide_circles(
  text_position = "ontop",
  clip_text = TRUE
))


# More styling options
p + guides(size = guide_circles(override.aes = aes(colour = "red")))+
  theme(
    # Key background
    legend.key = element_rect(colour = "black", fill = 'white'),
    # Padding around central shapes
    legendry.legend.key.margin = margin(1, 1, 1, 1, "cm"),
    legend.ticks = element_line(colour = "blue"),
    legend.text.position = "left"
  )
```
