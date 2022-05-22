# Public constructor ------------------------------------------------------

#' Axis guide with truncated axis line
#'
#' This is very similar to a regular axis guide, with the addition that the
#' axis line can be truncated. By default, the axis line is truncated at the
#' most extreme breaks, but other options are available.
#'
#' @inheritParams guide_axis_ext
#' @inheritDotParams guide_axis_ext
#'
#' @inherit guide_axis_vanilla return
#' @export
#' @family axis variants
#'
#' @examples
#' # A basic plot with axis lines
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme(axis.line = element_line())
#'
#' # By default, axis lines are truncated to extreme breaks
#' p + guides(x = "axis_trunc")
#'
#' # Turn off truncation at only one end
#' p + guides(x = guide_axis_trunc(trunc_upper = NULL))
#'
#' # Truncated at specific data values
#' p + guides(x = guide_axis_trunc(trunc_lower = 3, trunc_upper = 6))
#'
#' # Truncated at some distance
#' p + guides(x = guide_axis_trunc(
#'   trunc_lower = unit(3, "cm"),
#'   trunc_upper = unit(1, "npc") - unit(3, "cm")
#' ))
#'
#' # Truncation with functions take existing breaks as input
#' p + guides(x = guide_axis_trunc(
#'   trunc_lower = function(x) x - 0.3,
#'   trunc_upper = ~ .x + 0.3 # rlang lambda syntax function
#' ))
guide_axis_trunc <- function(
  trunc_lower = min,
  trunc_upper = max,
  ...
) {
  guide_axis_ext(
    trunc_lower = trunc_lower,
    trunc_upper = trunc_upper,
    ...
  )
}
