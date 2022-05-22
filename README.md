
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gguidance

<!-- badges: start -->
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

## Example

As of now, the only thing of note is `guide_colourbar_cap()`.

``` r
library(gguidance)
#> Loading required package: ggplot2

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = cty)) +
  scale_colour_viridis_c(guide = "colourbar_cap")
```

<img src="man/figures/README-example-1.png" width="80%" />

## Progress

So far, the following has been implemented:

-   **Axis guides**
    -   `guide_axis_vanilla()`: a re-implementation of `guide_axis()`.
-   **Legend guides**
    -   `guide_legend_vanilla()`: a re-implementation of
        `guide_legend()`.
-   **Colour bar guides**
    -   `guide_colourbar_vanilla()`: a re-implementation of
        `guide_colourbar()`.
    -   `guide_colourbar_cap()`: With cap decorations at the ends.

## Notes

A medium-term goal is to move and refactor guides from the {ggh4x}
package to {gguidance}. A longer-term goal is to make this package a hub
for additional axes, legends and other guides.
