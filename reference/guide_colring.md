# Colour rings and arcs

Similar to
[`guide_colourbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html),
this guide displays continuous `colour` or `fill` aesthetics. Instead of
a bar, the gradient in shown in a ring or arc, which can be convenient
for cyclical palettes such as some provided in the scico package.

## Usage

``` r
guide_colring(
  title = waiver(),
  key = "auto",
  start = 0,
  end = NULL,
  outer_guide = "axis_base",
  inner_guide = "axis_base",
  nbin = 300L,
  reverse = FALSE,
  show_labels = "outer",
  theme = NULL,
  vanilla = TRUE,
  position = waiver(),
  available_aes = c("colour", "fill"),
  ...
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

- key:

  A [standard
  key](https://teunbrand.github.io/legendry/reference/key_standard.md)
  specification. Defaults to
  [`key_auto()`](https://teunbrand.github.io/legendry/reference/key_standard.md).

- start, end:

  A `<numeric[1]>` in radians specifying the offset of the starting and
  end points from 12 o'clock. The `NULL` default for `end`, internally
  defaults to `start + 2 * pi`.

- outer_guide, inner_guide:

  Guides to display on the outside and inside of the colour ring. Each
  guide can be specified using one of the following:

  - A `<Guide>` class object.

  - A `<function>` that returns a `<Guide>` class object.

  - A `<character>` naming such function, without the `guide_` or
    `primitive_` prefix.

- nbin:

  A positive `<integer[1]>` determining how many colours to display.

- reverse:

  A `<logical[1]>` whether to reverse continuous guides. If `TRUE`,
  guides like colour bars are flipped. If `FALSE` (default), the
  original order is maintained.

- show_labels:

  A `<character[1]>` indicating for which guide labels should be shown.
  Can be one of `"outer"` (default), `"inner"`, `"both"` or `"none"`.
  Note that labels can only be omitted if the related guide has a label
  suppression mechanism.

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- vanilla:

  A `<logical[1]>` whether to have the default style match the vanilla
  [`guide_colourbar()`](https://ggplot2.tidyverse.org/reference/guide_colourbar.html)
  (`TRUE`) or take the theme verbatim (`FALSE`).

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

- available_aes:

  A `<character>` vector listing the aesthetics for which this guide can
  be build.

- ...:

  Arguments forwarded to the `outer_guide` and `inner_guide` if provided
  as functions or strings.

## Value

A `<Guide>` object.

## Details

### Styling options

This guide is a hybrid composition guide, where the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) settings
apply both this guide itself and the constituents it manages. The
constituents are linked below so you can find their 'Styling options'
sections. Note that
[`guide_axis_base()`](https://teunbrand.github.io/legendry/reference/guide_axis_base.md)
is just a default that can be swapped out.

|  |  |
|----|----|
| **Constituent** | **Description** |
| [`guide_axis_base()`](https://teunbrand.github.io/legendry/reference/guide_axis_base.md) | Makes up the tick marks and labels at inner and outer rings. |

In addition to the constituent's theme setting, it also has the
following settings:

|  |  |  |
|----|----|----|
| **Theme setting** | **Type** | **Description** |
| `legend.background` | [`element_rect()`](https://ggplot2.tidyverse.org/reference/element.html) | Background of the legend. |
| `legend.margin` | [`margin()`](https://ggplot2.tidyverse.org/reference/element.html) | Padding around the legend. |
| `legend.key.width` | [`unit()`](https://rdrr.io/r/grid/unit.html) | Radial thickness of the donut ring. |
| `legend.key.size` | [`unit()`](https://rdrr.io/r/grid/unit.html) | Size of the legend. Multiplied by 5 to roughly match dimensions of colour bar guides. |
| `legend.frame` | [`element_rect()`](https://ggplot2.tidyverse.org/reference/element.html) | Outline of the donut. The `fill` setting is ignored. |
| `legend.title` | [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html) | Title of the legend. |
| `legend.title.position` | `<character[1]>` | One of `"top"`, `"right"`, `"bottom"` or `"left"`. |

Styling options *per break* can be set in the [standard
key](https://teunbrand.github.io/legendry/reference/key_standard.md).
These override theme settings.

The context-agnostic alternative to using
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) is to
use
[`theme_guide()`](https://teunbrand.github.io/legendry/reference/theme_guide.md):

    guide_colring(theme = theme_guide(
      # Ring settings
      title = element_text(),
      title.position = "top",
      margin = margin(5),
      background = element_rect(),
      frame = element_rect(),
      key.size = unit(1, "cm"),
      key.width = unit(5, "mm"),

      # Common options for `guide_axis_base()`
      line = element_line(),
      text = element_text(),
      ticks = element_line(),
      ticks.length = unit(5, "mm"),
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
[`guide_colsteps()`](https://teunbrand.github.io/legendry/reference/guide_colsteps.md),
[`guide_legend_base()`](https://teunbrand.github.io/legendry/reference/guide_legend_base.md),
[`guide_legend_cross()`](https://teunbrand.github.io/legendry/reference/guide_legend_cross.md),
[`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md),
[`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)

## Examples

``` r
# Rings works best with a cyclical palette
my_pal <- c("black", "tomato", "white", "dodgerblue", "black")

p <- ggplot(mpg, aes(displ, hwy, colour = cty)) +
  geom_point() +
  scale_colour_gradientn(colours = my_pal)

# Standard colour ring
p + guides(colour = "colring")


# As an arc
p + guides(colour = guide_colring(
  start = 1.25 * pi, end = 2.75 * pi
))


# Removing the inner tick marks
p + guides(colour = guide_colring(inner_guide = "none"))


# Include labels on the inner axis
p + guides(colour = guide_colring(show_labels = "both"))


# Passing an argument to inner/outer guides
p + guides(colour = guide_colring(angle = 0))
```
