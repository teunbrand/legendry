# Constructor -------------------------------------------------------------

#' Axis guide with capped axis line
#'
#' This is very similar to a regular axis guide, with the addition that the
#' axis line can be capped By default, the axis line is capped at the
#' most extreme breaks, but other options are available.
#'
#' @inheritParams guide_axis_extend
#' @inheritDotParams guide_axis_extend -cap_lower -cap_upper
#'
#' @inherit guide_axis_extend return
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
#' p + guides(x = "axis_cap")
#'
#' # Turn off truncation at only one end
#' p + guides(x = guide_axis_cap(cap_upper = NULL))
#'
#' # Truncated at specific data values
#' p + guides(x = guide_axis_cap(cap_lower = 3, cap_upper = 6))
#'
#' # Truncated at some distance
#' p + guides(x = guide_axis_cap(
#'   cap_lower = unit(3, "cm"),
#'   cap_upper = unit(1, "npc") - unit(3, "cm")
#' ))
#'
#' # Truncation with functions take existing breaks as input
#' p + guides(x = guide_axis_cap(
#'   cap_lower = function(x) x - 0.3,
#'   cap_upper = ~ .x + 0.3 # rlang lambda syntax function
#' ))
guide_axis_cap <- function(
    cap_lower = min,
    cap_upper = max,
    ...
) {
  guide_axis_extend(
    cap_lower = cap_lower,
    cap_upper = cap_upper,
    ...
  )
}
