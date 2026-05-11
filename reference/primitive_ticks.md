# Guide primitive: line

This function constructs a ticks [guide
primitive](https://teunbrand.github.io/legendry/reference/guide-primitives.md).

## Usage

``` r
primitive_ticks(key = NULL, bidi = FALSE, theme = NULL, position = waiver())
```

## Arguments

- key:

  A [standard
  key](https://teunbrand.github.io/legendry/reference/key_standard.md)
  specification. See more information in the linked topic.

- bidi:

  A `<logical[1]>`: whether ticks should be drawn bidirectionally
  (`TRUE`) or in a single direction (`FALSE`, default).

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

## Value

A `PrimitiveTicks` primitive guide that can be used inside other guides.

## Details

### Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide, which may differ depending on
whether the guide is used in an axis or in a legend context.

The ticks can come in three variants: major, minor and minimal. Which
variants are drawn depends on the keys:
[`key_minor()`](https://teunbrand.github.io/legendry/reference/key_standard.md)
draws major and minor ticks, whereas
[`key_log()`](https://teunbrand.github.io/legendry/reference/key_standard.md)
also has minimal ticks. Each variant has a corresponding length setting.

The possible `{position}` suffixes mentioned below are `x`, `x.top`,
`x.bottom`, `y`, `y.left`, `y.right`. The `theta` and `r` position
suffixes in ggplot2 are *not* obeyed in legendry.

|  |  |  |  |
|----|----|----|----|
| **Theme setting** | **Context** | **Type** | **Description** |
| `axis.ticks.{position}` | Axis | [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html) | Major tick lines |
| `axis.ticks.length.{position}` | Axis | [`unit()`](https://rdrr.io/r/grid/unit.html) | Major tick length |
| `axis.minor.ticks.{position}` | Axis | [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html) | Minor tick lines |
| `axis.minor.ticks.length.{position}` | Axis | [`unit()`](https://rdrr.io/r/grid/unit.html) | Minor tick length |
| `legendry.axis.mini.ticks` | Axis | [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html) | Minimal tick lines |
| `legendry.axis.mini.ticks.length` | Axis | [`unit()`](https://rdrr.io/r/grid/unit.html) | Minimal tick length |
| `legend.ticks` | Legend | [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html) | Major tick lines |
| `legend.ticks.length` | Legend | [`unit()`](https://rdrr.io/r/grid/unit.html) | Major ticks length |
| `legendry.legend.minor.ticks` | Legend | [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html) | Minor tick lines |
| `legendry.legend.minor.ticks.length` | Legend | [`unit()`](https://rdrr.io/r/grid/unit.html) | Minor ticks length |
| `legendry.legend.mini.ticks` | Legend | [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html) | Minimal tick lines |
| `legendry.legend.mini.ticks.length` | Legend | [`unit()`](https://rdrr.io/r/grid/unit.html) | Minimal tick length |

Styling options *per break* can be set in the
[key](https://teunbrand.github.io/legendry/reference/key_standard.md).
The `line` and prefixed properties are prioritised for the tick lines.
These override theme settings.

The context-agnostic alternative to using
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) is to
use
[`theme_guide()`](https://teunbrand.github.io/legendry/reference/theme_guide.md):

    primitive_ticks(theme = theme_guide(
      ticks = element_line(),
      ticks.length = unit(5, "mm"),
      minor.ticks = element_line(),
      minor.ticks.length = unit(4, "mm"),
      mini.ticks = element_line(),
      mini.ticks.length = unit(3, "mm")
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
[`primitive_title()`](https://teunbrand.github.io/legendry/reference/primitive_title.md)

## Examples

``` r
# A standard plot
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point()

# Adding as secondary guides
p + guides(x.sec = primitive_ticks(), y.sec = primitive_ticks())
```
