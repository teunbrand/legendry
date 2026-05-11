# Guide gizmo: custom grob

This guide displays a user-provided grob.

## Usage

``` r
gizmo_grob(
  grob,
  width = grobWidth(grob),
  height = grobHeight(grob),
  hjust = 0.5,
  vjust = 0.5,
  position = waiver()
)
```

## Arguments

- grob:

  A [`<grob>`](https://rdrr.io/r/grid/grid.grob.html) to display.

- width, height:

  A \[`<unit[1]>`\]\[grid::unit\] setting the allocated width and height
  of the the grob respectively.

- hjust, vjust:

  A `<numeric[1]>` between 0 and 1 setting the horizontal and vertical
  justification of the grob when used as a guide for the `x` and `y`
  aesthetics.

- position:

  Where this guide should be drawn: one of `"top"`, `"bottom"`,
  `"left"`, or `"right"`.

## Value

A `<GizmoGrob>` object.

## Details

### Styling options

There are no
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) styling
options for `gizmo_grob()`.

## See also

Other gizmos:
[`gizmo_barcap()`](https://teunbrand.github.io/legendry/reference/gizmo_barcap.md),
[`gizmo_density()`](https://teunbrand.github.io/legendry/reference/gizmo_density.md),
[`gizmo_histogram()`](https://teunbrand.github.io/legendry/reference/gizmo_histogram.md),
[`gizmo_stepcap()`](https://teunbrand.github.io/legendry/reference/gizmo_stepcap.md)

## Examples

``` r
circle <- grid::circleGrob()

# A standard plot with grob gizmos
ggplot(mpg, aes(displ, hwy, colour = cty)) +
  geom_point() +
  guides(
    x.sec = gizmo_grob(
      circle, hjust = 0.75,
      width = unit(2, "cm"), height = unit(2, "cm")
    ),
    colour = gizmo_grob(
      circle, width = unit(1, "cm"), height = unit(1, "cm")
    )
  )
```
