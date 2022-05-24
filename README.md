
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gguidance

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/gguidance)](https://CRAN.R-project.org/package=gguidance)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/teunbrand/gguidance/workflows/R-CMD-check/badge.svg)](https://github.com/teunbrand/gguidance/actions)
[![Codecov test
coverage](https://codecov.io/gh/teunbrand/gguidance/branch/master/graph/badge.svg)](https://app.codecov.io/gh/teunbrand/gguidance?branch=master)
<!-- badges: end -->

The goal of gguidance is to provide additional guides to the ggplot2
ecosystem. Guides are re-implemented within the ggproto system to make
use of the flexible extension framework.

Please note that this repo is still being worked on and, while probably
usable, isn’t finished.

## Installation

You can install the development version of gguidance from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("teunbrand/gguidance")
```

## Examples

Here is a simple example of how to use an axis guide and a colour bar
guide.

``` r
library(gguidance)
#> Loading required package: ggplot2

ggplot(msleep, aes(bodywt, sleep_total)) +
  geom_point(aes(colour = sleep_rem)) +
  scale_colour_viridis_c(guide = "colourbar_cap") +
  scale_x_log10(guide = "axis_log")
```

<img src="man/figures/README-example-1.png" width="80%" />

Demonstrating a cross legend and axis subtitles.

``` r
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = paste(cyl, year))) +
  guides(colour = "legend_cross",
         x = guide_axis_ext(subtitle = "litres"),
         y = guide_axis_ext(subtitle = "miles per gallon"))
```

<img src="man/figures/README-example2-1.png" width="80%" />

## Progress

So far, the following has been implemented:

-   **Axis guides**
    -   `guide_axis_vanilla()`: a re-implementation of `guide_axis()`.
    -   `guide_axis_ext()`: Extended options for axes.
    -   `guide_axis_minor()`: Axes with minor ticks.
    -   `guide_axis_trunc()`: Axes with truncated axis lines.
    -   `guide_axis_log()`: Axes with log10-based tickmarks.
-   **Legend guides**
    -   `guide_legend_vanilla()`: a re-implementation of
        `guide_legend()`.
    -   `guide_legend_cross()`: Combining two sets of labels in one
        legend.
-   **Colour bar guides**
    -   `guide_colourbar_vanilla()`: a re-implementation of
        `guide_colourbar()`.
    -   `guide_colourbar_cap()`: With cap decorations at the ends.

## Notes

A medium-term goal is to move and refactor guides from the {ggh4x}
package to {gguidance}. A longer-term goal is to make this package a hub
for additional axes, legends and other guides.
