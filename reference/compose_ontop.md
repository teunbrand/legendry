# Compose guides on top of one another

**\[experimental\]**

This guide can place place other guides on top of one another.

## Usage

``` r
compose_ontop(
  ...,
  args = list(),
  key = NULL,
  title = waiver(),
  angle = waiver(),
  theme = NULL,
  order = 0L,
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

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

- available_aes:

  A `<character>` giving aesthetics that must match the the guides.

## Value

A `<ComposeOntop>` composite guide object.

## See also

Other composition:
[`compose_crux()`](https://teunbrand.github.io/legendry/reference/compose_crux.md),
[`compose_sandwich()`](https://teunbrand.github.io/legendry/reference/compose_sandwich.md),
[`compose_stack()`](https://teunbrand.github.io/legendry/reference/compose_stack.md),
[`guide-composition`](https://teunbrand.github.io/legendry/reference/guide-composition.md)

## Examples

``` r
# Using the ontop composition to get two types of ticks with different
# lengths
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  guides(x = compose_ontop(
    guide_axis_base(
      key_manual(c(2, 4, 6)),
      theme = theme(
        axis.ticks = element_line(colour = "limegreen"),
        axis.ticks.length = unit(11, "pt")
      )
    ),
    guide_axis_base(
      key_manual(c(3, 5, 7)),
      theme = theme(
        axis.ticks = element_line(colour = "tomato"),
        axis.ticks.length = unit(5.5, "pt")
      )
    )
  ))
```
