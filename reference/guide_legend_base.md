# Custom legend guide

This legend closely mirrors
[`ggplot2::guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html),
but has two adjustments. First, `guide_legend_base()` supports a
`design` argument for a more flexible layout. Secondly, the
`legend.spacing.y` theme element is observed verbatim instead of
overruled.

## Usage

``` r
guide_legend_base(
  key = NULL,
  title = waiver(),
  theme = NULL,
  design = NULL,
  nrow = NULL,
  ncol = NULL,
  reverse = FALSE,
  override.aes = list(),
  position = NULL,
  direction = NULL,
  order = 0
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

- design:

  Specification of the legend layout. One of the following:

  - `NULL` (default) to use the layout algorithm of
    [`guide_legend()`](https://ggplot2.tidyverse.org/reference/guide_legend.html).

  - A `<character[1]>` string representing a cell layout wherein `#`
    defines an empty cell. See examples.

  - A `<matrix[n, m]>` representing a cell layout wherein `NA` defines
    an empty cell. See examples. Non-string atomic vectors will be
    treated with [`as.matrix()`](https://rdrr.io/r/base/matrix.html).

- nrow, ncol:

  A positive `<integer[1]>` setting the desired dimensions of the legend
  layout. When `NULL` (default), the dimensions will be derived from the
  `design` argument or fit to match the number of keys.

- reverse:

  A `<logical[1]>` whether the order of keys should be inverted.

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

- order:

  A positive `<integer[1]>` that specifies the order of this guide among
  multiple guides. This controls in which order guides are merged if
  there are multiple guides for the same position. If `0` (default), the
  order is determined by a hashing indicative settings of a guide.

## Value

A `<GuideLegend>` object.

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
[`guide_legend_cross()`](https://teunbrand.github.io/legendry/reference/guide_legend_cross.md),
[`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md),
[`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)

Other legend guides:
[`guide_legend_cross()`](https://teunbrand.github.io/legendry/reference/guide_legend_cross.md),
[`guide_legend_group()`](https://teunbrand.github.io/legendry/reference/guide_legend_group.md),
[`guide_legend_manual()`](https://teunbrand.github.io/legendry/reference/guide_legend_manual.md)

## Examples

``` r
# A dummy plot
p <- ggplot(data.frame(x = 1:3, type = c("tic", "tac", "toe"))) +
  aes(x, x, shape = type) +
  geom_point(na.rm = TRUE) +
  scale_shape_manual(values = c(1, 4, NA))

# A design string, each character giving a cell value.
# Newlines separate rows, white space is ignored.
design <- "
  123
  213
  321
"

# Alternatively, the same can be specified using a matrix directly
# design <- matrix(c(1, 2, 3, 2, 1, 3, 3, 2, 1), 3, 3, byrow = TRUE)

p + guides(shape = guide_legend_base(design = design))


# Empty cells can be created using `#`
design <- "
  #2#
  1#3
"

# Alternatively:
# design <- matrix(c(NA, 1, 2, NA, NA, 3), nrow = 2)

p + guides(shape = guide_legend_base(design = design))
```
