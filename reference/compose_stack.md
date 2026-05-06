# Compose guides as stack

**\[experimental\]**

This guide can stack other guides.

## Usage

``` r
compose_stack(
  ...,
  args = list(),
  key = NULL,
  title = waiver(),
  side.titles = waiver(),
  angle = waiver(),
  theme = NULL,
  order = 0,
  drop = NULL,
  position = waiver(),
  available_aes = NULL
)
```

## Arguments

- ...:

  Guides to stack in
  [composition](https://teunbrand.github.io/legendry/reference/guide-composition.md).
  Each guide can be specified as one of the following:

  - A `<Guide>` class object.

  - A `<function>` that returns a `<Guide>` class object.

  - A `<character[1]>` naming such a function, without the `guide_` or
    `primitive_` prefix.

- args:

  A `<list>` of arguments to pass to guides that are given either as a
  function or as a string.

- key:

  A [standard
  key](https://teunbrand.github.io/legendry/reference/key_standard.md)
  specification. The key is shared among all guides that have `NULL`
  keys themselves. See more information in the linked topic.

- title:

  One of the following to indicate the title of the guide:

  - A `<character[1]>` or `<expression[1]>` to set a custom title.

  - `NULL` to not display any title.

  - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    (default) to take the name of the scale object or the name specified
    in [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) as
    the title.

- side.titles:

  A `<character>` giving labels for titles displayed on the side of the
  stack. Set to `NULL` to display no side titles. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html), an
  attempt is made to extract the titles from the guides and use these as
  side titles.

  The `side.titles` are styled using the `legendry.axis.subtitle` theme
  setting. Their placement is controlled via the
  `legendry.axis.subtitle.position` setting.

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

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- order:

  A positive `<integer[1]>` that specifies the order of this guide among
  multiple guides. This controls in which order guides are merged if
  there are multiple guides for the same position. If `0` (default), the
  order is determined by a hashing indicative settings of a guide.

- drop:

  An `<integer>` giving the indices of guides that should be dropped
  when a facet requests no labels to be drawn at axes in between panels.
  The default, `NULL`, will drop every guide except the first.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

- available_aes:

  A `<character>` giving aesthetics that must match the the guides.

## Value

A `<ComposeStack>` guide object.

## See also

Other composition:
[`compose_crux()`](https://teunbrand.github.io/legendry/reference/compose_crux.md),
[`compose_ontop()`](https://teunbrand.github.io/legendry/reference/compose_ontop.md),
[`compose_sandwich()`](https://teunbrand.github.io/legendry/reference/compose_sandwich.md),
[`guide-composition`](https://teunbrand.github.io/legendry/reference/guide-composition.md)

## Examples

``` r
ggplot() +
  geom_function(fun = dnorm, xlim = c(-3, 3)) +
  guides(x = compose_stack(
    "axis", "axis",
    side.titles = c("first", "second")
  )) +
  # Add margin to make room for side titles
  theme(plot.margin = margin(5.5, 5.5, 5.5, 11))
```
