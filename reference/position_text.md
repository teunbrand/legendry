# Helper to position text

This is a helper function for use in
[`guide_legend_cross()`](https://teunbrand.github.io/legendry/reference/guide_legend_cross.md).
It creates a list of text elements, each corresponding the top, right,
bottom or left positions. Input is given for that order as well.

## Usage

``` r
position_text(angle = NA, hjust = NA, vjust = NA, ..., element = element_text)
```

## Arguments

- angle:

  A `<numeric[1-4]>` value setting the angle of the text.

- hjust, vjust:

  A `<numeric[1-4]>` between 0 and 1 to set the horizontal justification
  (`hjust`) or vertical justification (`vjust`) for the text.

- ...:

  Other arguments passed to the `element` function.

- element:

  An `<function>` creating an object that inherits from the
  [`<element_text>`](https://ggplot2.tidyverse.org/reference/element.html)
  class.

## Value

A named list of `element` objects of length 4. The list has the names
`"top"`, `"right"`, `"bottom"` and `"left"`.

## Details

Input other than the `element` argument will be recycled to length 4.
`NA` and 0-length input will be dropped.

## Examples

``` r
# Red text turning along with position
position_text(angle = c(0, -90, 180, 90), colour = "red")
#> $top
#> <ggplot2::element_text>
#>  @ family       : NULL
#>  @ face         : NULL
#>  @ italic       : chr NA
#>  @ fontweight   : num NA
#>  @ fontwidth    : num NA
#>  @ colour       : chr "red"
#>  @ size         : NULL
#>  @ hjust        : NULL
#>  @ vjust        : NULL
#>  @ angle        : num 0
#>  @ lineheight   : NULL
#>  @ margin       : NULL
#>  @ debug        : NULL
#>  @ inherit.blank: logi FALSE
#> 
#> $right
#> <ggplot2::element_text>
#>  @ family       : NULL
#>  @ face         : NULL
#>  @ italic       : chr NA
#>  @ fontweight   : num NA
#>  @ fontwidth    : num NA
#>  @ colour       : chr "red"
#>  @ size         : NULL
#>  @ hjust        : NULL
#>  @ vjust        : NULL
#>  @ angle        : num -90
#>  @ lineheight   : NULL
#>  @ margin       : NULL
#>  @ debug        : NULL
#>  @ inherit.blank: logi FALSE
#> 
#> $bottom
#> <ggplot2::element_text>
#>  @ family       : NULL
#>  @ face         : NULL
#>  @ italic       : chr NA
#>  @ fontweight   : num NA
#>  @ fontwidth    : num NA
#>  @ colour       : chr "red"
#>  @ size         : NULL
#>  @ hjust        : NULL
#>  @ vjust        : NULL
#>  @ angle        : num 180
#>  @ lineheight   : NULL
#>  @ margin       : NULL
#>  @ debug        : NULL
#>  @ inherit.blank: logi FALSE
#> 
#> $left
#> <ggplot2::element_text>
#>  @ family       : NULL
#>  @ face         : NULL
#>  @ italic       : chr NA
#>  @ fontweight   : num NA
#>  @ fontwidth    : num NA
#>  @ colour       : chr "red"
#>  @ size         : NULL
#>  @ hjust        : NULL
#>  @ vjust        : NULL
#>  @ angle        : num 90
#>  @ lineheight   : NULL
#>  @ margin       : NULL
#>  @ debug        : NULL
#>  @ inherit.blank: logi FALSE
#> 
```
