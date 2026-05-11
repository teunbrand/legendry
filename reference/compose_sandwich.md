# Compose guides as a sandwich

**\[experimental\]**

This guide composition has a middle guide flanked by two parallel
guides.

## Usage

``` r
compose_sandwich(
  key = key_auto(),
  middle = gizmo_barcap(),
  text = "none",
  opposite = "none",
  args = list(),
  suppress_labels = "opposite",
  complete = TRUE,
  theme = NULL,
  theme_defaults = list(),
  reverse = FALSE,
  order = 0L,
  title = waiver(),
  position = waiver(),
  available_aes = NULL
)
```

## Arguments

- key:

  A [standard
  key](https://teunbrand.github.io/legendry/reference/key_standard.md)
  specification. The key is shared among all guides that have `NULL`
  keys themselves. See more information in the linked topic.

- middle:

  Guide to use as the middle guide. Each guide can be specified as one
  of the following:

  - A `<Guide>` class object.

  - A `<function>` that returns a `<Guide>` class object.

  - A `<character>` naming such a function, without the `guide_` or
    `primitive_` prefix.

- text, opposite:

  Guides to use at the `legend.text.position` location and on the
  opposite side of the `middle` guide respectively. Guide specification
  is the same as in the `middle` argument.

- args:

  A `<list>` of arguments to pass to guides that are given either as a
  function or as a string.

- suppress_labels:

  A `<character>` vector giving any of `"text"` and `"opposite"` for the
  parallel guides. The guide(s) listed here will not draw labels if they
  support a label suppression mechanism.

- complete:

  A `<logical[1]>` whether to treat the composition as a complete guide.
  If `TRUE`, a title and margin are added to the result. If `FALSE`
  (default), no titles and margins are added.

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually of differently from the plot's
  theme settings. The `theme` arguments in the guide overrides, and is
  combined with, the plot's theme.

- theme_defaults:

  A `<list>` of theme elements to override undeclared theme arguments.

- reverse:

  A `<logical[1]>` whether to reverse continuous guides. If `TRUE`,
  guides like colour bars are flipped. If `FALSE` (default), the
  original order is maintained.

- order:

  A positive `<integer[1]>` that specifies the order of this guide among
  multiple guides. This controls in which order guides are merged if
  there are multiple guides for the same position. If `0` (default), the
  order is determined by a hashing indicative settings of a guide.

- title:

  One of the following to indicate the title of the guide:

  - A `<character[1]>` or `<expression[1]>` to set a custom title.

  - `NULL` to not display any title.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    (default) to take the name of the scale object or the name specified
    in [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) as
    the title.

- position:

  Where this guide should be drawn: one of `"top"`, `"bottom"`,
  `"left"`, or `"right"`.

- available_aes:

  A `<character>` vector listing the aesthetics for which this guide can
  be build.

## Value

A `<ComposeSandwich>` guide object.

## Details

The sandwich composition is effectively the same as a [crux
composition](https://teunbrand.github.io/legendry/reference/compose_crux.md)
lacking two opposing arms.

### Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide.

|  |  |  |
|----|----|----|
| **Theme setting** | **Type** | **Description** |
| `legend.title` | [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html) | The title of the legend. |
| `legend.title.position` | `<character[1]>` | One of `"top"`, `"right"`, `"bottom"` or `"left"`. |
| `legend.text.position` | `<character[1]>` | One of `"top"`, `"right"`, `"bottom"` or `"left"`. |
| `legend.margin` | [`margin()`](https://ggplot2.tidyverse.org/reference/element.html) | Padding around the legend. |
| `legend.background` | [`element_rect()`](https://ggplot2.tidyverse.org/reference/element.html) | Background of the legend. |

There are no further styling options.

The context-agnostic alternative to using
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) is to
use
[`theme_guide()`](https://teunbrand.github.io/legendry/reference/theme_guide.md):

    compose_sandwich(theme = theme_guide(
      title = element_text(),
      title.position = "top",
      text.position = "right",
      margin = margin(5),
      background = element_rect()
    ))

## See also

Other composition:
[`compose_crux()`](https://teunbrand.github.io/legendry/reference/compose_crux.md),
[`compose_ontop()`](https://teunbrand.github.io/legendry/reference/compose_ontop.md),
[`compose_stack()`](https://teunbrand.github.io/legendry/reference/compose_stack.md),
[`guide-composition`](https://teunbrand.github.io/legendry/reference/guide-composition.md)

## Examples

``` r
# A standard plot with a sandwich guide
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = cty)) +
  guides(colour = compose_sandwich(
    middle = "colourbar",
    text = "axis_base",
    opposite = primitive_bracket(key = key_range_manual(
      start = c(10, 20), end = c(25, 30), name = c("A", "B")
    ))
  ))
```
