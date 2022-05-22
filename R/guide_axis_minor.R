# Public constructor ------------------------------------------------------

#' Axis guide with ticks for minor breaks
#'
#' This is very similar to a regular axis guide, with the addition that it
#' also places tick marks at minor break locations. The `minor_breaks` argument
#' to [`scale_{x/y}_continuous()`][ggplot2::scale_x_continuous()] controls
#' where the breaks are placed.
#'
#' @inheritParams guide_axis_ext
#' @inheritDotParams guide_axis_ext
#'
#' @inherit guide_axis_vanilla return
#' @export
#' @family axis variants
#'
#' @examples
#' # A basic plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Show minor ticks
#' p + guides(x = "axis_minor")
#'
#' # Controlling tick size relative to normal ticks
#' p + guides(x = guide_axis_minor(minor_size = 2))
guide_axis_minor <- function(
  minor_size = 0.75,
  ...
) {
  guide_axis_ext(
    minor_size = minor_size,
    ...
  )
}
