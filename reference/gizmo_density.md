# Guide gizmo: kernel density estimate

This guide displays a kernel density estimate (KDE) of the aesthetic. If
the aesthetic is `colour` or `fill`, the shape will reflect this.

## Usage

``` r
gizmo_density(
  key = waiver(),
  density = NULL,
  density.args = list(),
  density.fun = stats::density,
  just = 0.5,
  oob = "keep",
  alpha = NA,
  theme = NULL,
  position = waiver(),
  direction = NULL
)
```

## Arguments

- key:

  A [sequence
  key](https://teunbrand.github.io/legendry/reference/key_specialty.md)
  or [binned
  key](https://teunbrand.github.io/legendry/reference/key_specialty.md)
  specification. Internally defaults to a sequence key when the scale is
  continuous and a binned key when the scale is binned.

- density:

  One of the following:

  - `NULL` for using kernel density estimation on the data values
    (default).

  - a `<numeric>` vector to feed to the `density.fun` function.

  - A named `<list>` with `x` and `y` numeric elements of equal length,
    such as one returned by using the
    [`density()`](https://rdrr.io/r/stats/density.html) function. Please
    note that `<list>` input is expected in scale-transformed space, not
    original data space.

- density.args:

  A `<list>` with additional arguments to the `density.fun` argument.
  Only applies when `density` is not provided as a `<list>`. already.

- density.fun:

  A `<function>` to use for kernel density estimation when the `density`
  argument is not provided as a list already.

- just:

  A `<numeric[1]>` between 0 and 1. Use 0 for bottom- or left-aligned
  densities, use 1 for top- or right-aligned densities and 0.5 for
  violin shapes.

- oob:

  An out-of-bounds handling function that affects the cap colour. Can be
  one of the following:

  - A `<function>` like
    [`oob_squish`](https://scales.r-lib.org/reference/oob.html).

  - A `<character[1]>` naming such a function without the
    '`oob`'-prefix, such as `"keep"`.

- alpha:

  A `<numeric[1]>` between 0 and 1 setting the colour transparency of
  the bar. Use `NA` to preserve the alpha encoded in the colour itself.

- theme:

  A [`<theme>`](https://ggplot2.tidyverse.org/reference/theme.html)
  object to style the guide individually or differently from the plot's
  theme settings. The `theme` argument in the guide overrides and is
  combined with the plot's theme.

- position:

  A `<character[1]>` giving the location of the guide. Can be one of
  `"top"`, `"bottom"`, `"left"` or `"right"`.

- direction:

  A `<character[1]>` indicating the direction of the guide. Can be on of
  `"horizontal"` or `"vertical"`.

## Value

A `<GizmoDensity>` object.

## Details

Non-finite values such as `NA` and `NaN` are ignored while infinite
values such as `-Inf` and `Inf` are
[squished](https://scales.r-lib.org/reference/oob.html) to the limits.

### Styling options

Below are the
[theme](https://ggplot2.tidyverse.org/reference/theme.html) options that
determine the styling of this guide.

|  |  |  |
|----|----|----|
| **Theme setting** | **Type** | **Description** |
| `legend.frame` | [`element_rect()`](https://ggplot2.tidyverse.org/reference/element.html) | Outline drawn around the density itself. The `fill` setting is ignored. |
| `legend.key` | [`element_rect()`](https://ggplot2.tidyverse.org/reference/element.html) | Background underneath the density area. |
| `legend.key.width` | [`unit()`](https://rdrr.io/r/grid/unit.html) | Width of the density area. |
| `legend.key.height` | [`unit()`](https://rdrr.io/r/grid/unit.html) | Height of the density area. |

Please note that depending on the `direction` argument, the
`legend.key.width`/`legend.key.height` setting are expanded 5-fold if
originating from the global theme. To set these directly, you can use
the local `theme` argument in the guide. These settings have shorthands
in
[`theme_guide()`](https://teunbrand.github.io/legendry/reference/theme_guide.md):

    gizmo_density(theme = theme_guide(
      frame = element_rect(),
      key = element_rect(),
      key.width = unit(5, "mm")
      key.height = unit(5, "cm")
    ))

## See also

Other gizmos:
[`gizmo_barcap()`](https://teunbrand.github.io/legendry/reference/gizmo_barcap.md),
[`gizmo_grob()`](https://teunbrand.github.io/legendry/reference/gizmo_grob.md),
[`gizmo_histogram()`](https://teunbrand.github.io/legendry/reference/gizmo_histogram.md),
[`gizmo_stepcap()`](https://teunbrand.github.io/legendry/reference/gizmo_stepcap.md)

## Examples

``` r
# A standard plot
p <- ggplot(mpg, aes(displ, hwy, colour = cty)) +
  geom_point() +
  scale_colour_viridis_c()

# Density from plot data
p + guides(colour = gizmo_density())


# Using bins instead of gradient
p + guides(colour = gizmo_density("bins"))


# Providing custom values to compute density of
p + guides(colour = gizmo_density(density = runif(1000, min = 5, max = 35)))


# Providing a precomputed density
p + guides(colour = gizmo_density(density = density(mpg$cty, adjust = 0.5)))


# Alternatively, parameters may be passed through density.args
p + guides(colour = gizmo_density(density.args = list(adjust = 0.5)))
```
