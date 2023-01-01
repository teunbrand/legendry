# Public constructor ------------------------------------------------------

#' Axis guide with ticks for minor breaks
#'
#' This is very similar to a regular axis guide, with the addition that it
#' also places tick marks at minor break locations. The `minor_breaks` argument
#' to [`scale_{x/y}_continuous()`][ggplot2::scale_x_continuous()] controls
#' where the breaks are placed.
#'
#' @inheritParams guide_axis_extend
#' @inheritDotParams guide_axis_extend -minor_size -major_size
#'
#' @inherit guide_axis_extend return
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
#'
#' # Only show minor ticks
#' p + guides(x = guide_axis_minor(minor_size = 1, major_size = 0))
guide_axis_minor <- function(
    minor_size = 0.75,
    major_size = 1,
    ...
) {
  guide_axis_extend(
    minor_size = minor_size,
    major_size = major_size,
    ...
  )
}
