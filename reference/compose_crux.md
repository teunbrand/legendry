# Compose guides in a cross

**\[experimental\]**

This guide composition has a central guide optionally surrounded by
other guides on all four sides.

## Usage

``` r
compose_crux(
  key = NULL,
  centre = "none",
  left = "none",
  right = "none",
  top = "none",
  bottom = "none",
  args = list(),
  complete = FALSE,
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

- centre, left, right, top, bottom:

  Guides to use in
  [composition](https://teunbrand.github.io/legendry/reference/guide-composition.md)
  per position. Each guide can be specified as one of the following:

  - A `<Guide>` class object.

  - A `<function>` that returns a `<Guide>` class object.

  - A `<character>` naming such a function, without the `guide_` or
    `primitive_` prefix.

- args:

  A `<list>` of arguments to pass to guides that are given either as a
  function or as a string.

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

A `<ComposeCrux>` guide object.

## See also

Other composition:
[`compose_ontop()`](https://teunbrand.github.io/legendry/reference/compose_ontop.md),
[`compose_sandwich()`](https://teunbrand.github.io/legendry/reference/compose_sandwich.md),
[`compose_stack()`](https://teunbrand.github.io/legendry/reference/compose_stack.md),
[`guide-composition`](https://teunbrand.github.io/legendry/reference/guide-composition.md)

## Examples

``` r
# Roughly recreating a colour bar with extra text on top and bottom
crux <- compose_crux(
  centre = gizmo_barcap(), left = "axis_base",
  right = "axis_base",
  top = primitive_title("A lot"),
  bottom = primitive_title("A little")
)

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = cty)) +
  guides(colour = crux)
```
