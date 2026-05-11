# Theme wrapper for guides

This function does three things different than
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Usage

``` r
theme_guide(
  text = NULL,
  line = NULL,
  title = NULL,
  subtitle = NULL,
  text.position = NULL,
  title.position = NULL,
  subtitle.position = NULL,
  ticks = NULL,
  minor.ticks = NULL,
  mini.ticks = NULL,
  ticks.length = NULL,
  minor.ticks.length = NULL,
  mini.ticks.length = NULL,
  spacing = NULL,
  group.spacing = NULL,
  table.spacing = NULL,
  key = NULL,
  key.size = NULL,
  key.width = NULL,
  key.height = NULL,
  key.spacing = NULL,
  key.spacing.x = NULL,
  key.spacing.y = NULL,
  key.margin = NULL,
  key.justification = NULL,
  frame = NULL,
  byrow = NULL,
  background = NULL,
  margin = NULL,
  bracket = NULL,
  bracket.size = NULL,
  box = NULL,
  fence = NULL,
  fence.post = NULL,
  fence.rail = NULL,
  zebra.light = NULL,
  zebra.dark = NULL,
  point = NULL,
  connector = NULL
)
```

## Arguments

- text:

  An
  [`<element_text>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting both `legend.text` and `axis.text` elements.

- line:

  An
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting both `legend.axis.line` and `axis.line` elements.

- title:

  An
  [`<element_text>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting both `legend.title` and `axis.title` elements.

- subtitle:

  An
  [`<element_text>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting both `legendry.legend.subtitle` and `legendry.axis.subtitle`
  elements.

- text.position, title.position, subtitle.position:

  One of `"top"`, `"right"`, `"bottom"` or `"right"` setting the
  following elements:

  - `text.position`: sets only `legend.text.position`.

  - `title.position`: sets only `legend.title.position`.

  - `subtitle.position` sets both `legendry.legend.subtitle.position`
    and `legendry.axis.subtitle.position`

- ticks:

  An
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting both `axis.ticks` and `legend.ticks` elements.

- minor.ticks:

  An
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting `legendry.legend.minor.ticks` and all 6 of the
  `axis.ticks.minor.{r/theta/x.top/x.bottom/y.left/y.right}` elements.

- mini.ticks:

  An
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting both `legendry.legend.mini.ticks` and
  `legendry.axis.mini.ticks` elements.

- ticks.length, minor.ticks.length, mini.ticks.length:

  A \[`<unit[1]>`\]\[grid::unit()\] setting the following elements:

  - `ticks.length`: sets both `legend.ticks.length` and
    `axis.ticks.length`.

  - `minor.ticks.length` sets both `axis.minor.ticks.length` and
    `legendry.legend.minor.ticks.length`.

  - `mini.ticks.length` sets both `legendry.axis.mini.ticks.length` and
    `legendry.legend.mini.ticks.length`.

- spacing, group.spacing, table.spacing:

  A \[`<unit[1]>`\]\[grid::unit()\] settingthe `legendry.guide.spacing`,
  `legendry.group.spacing` and `legendry.table.spacing` theme elements
  respectively.

- key:

  An
  [`<element_rect>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting the `legend.key` element.

- key.size, key.width, key.height:

  A [`<unit>`](https://rdrr.io/r/grid/unit.html) setting the
  `legend.key.size`, `legend.key.width` and `legend.key.height` elements
  respectively.

- key.spacing, key.spacing.x, key.spacing.y:

  A \[`<unit[1]>`\]\[grid::unit()\] setting the `legend.key.spacing`,
  `legend.key.spacing.x` and `legend.key.spacing.y` elements
  respectively.

- key.margin:

  A [`<margin>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting the margin around legend glyphs.

- key.justification:

  A \[`<numeric[2]>`\] passed to the `legend.key.justification` setting.

- frame:

  An
  [`<element_rect>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting the `legend.frame` element.

- byrow:

  A `<logical[1]>` setting the `legend.byrow` element.

- background:

  An
  [`<element_rect>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting the `legend.background` element.

- margin:

  A [`<margin>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting the `legend.margin` element.

- bracket:

  An
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting the `legendry.bracket` element.

- bracket.size:

  A \[`<unit[1]>`\]\[grid::unit()\] setting the `legendry.bracket.size`
  element.

- box:

  An
  [`<element_rect>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting the `legendry.box` element.

- fence, fence.post, fence.rail:

  An
  [`<element_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting the `legendry.fence`, `legendry.fence.post` and
  `legendry.fence.rail` respectively.

- zebra.light, zebra.dark:

  An
  [`<element_rect>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting the `legendry.zebra.light` and `legendry.zebra.dark` settings
  respectively.

- point:

  An
  [`<element_point>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting the `legendry.point` element.

- connector:

  An
  [`<elemenet_line>`](https://ggplot2.tidyverse.org/reference/element.html)
  setting the `legendry.connector` element.

## Value

A `<theme>` object that can be provided to a guide.

## Details

1.  It has shorthand names for various guide settings, similar to
    [`theme_sub_legend()`](https://ggplot2.tidyverse.org/reference/subtheme.html)
    and
    [`theme_sub_axis()`](https://ggplot2.tidyverse.org/reference/subtheme.html)

2.  It simultaneously sets theme elements for axes and legends. This
    makes it contextually agnostic. For example, setting
    `theme_guide(text)` will populate `legend.text` and also
    `axis.text`.

3.  It includes legendry specific settings.

The first two things make it very good for setting the `guide_*(theme)`
arguments. The second thing makes it very bad for setting plot-wide or
global themes.

## Examples

``` r
red_ticks <- theme_guide(
  ticks = element_line(colour = "red", linewidth = 0.5)
)

# Both axis and colourbar gain red ticks
ggplot(mpg, aes(displ, hwy, colour = cty)) +
  geom_point() +
  guides(
    colour = guide_colourbar(theme = red_ticks),
    x = guide_axis(theme = red_ticks)
  )
```
