# Constructor -------------------------------------------------------------

#' Custom axis guide
#'
#' This axis guide is a visual representation of position scales and can
#' represent the `x`, `y`, `theta` and `r` aesthetics. It differs from
#' [`guide_axis()`][ggplot2::guide_axis] in that it can accept custom keys
#' and is can act as an axis for [`coord_radial()`][ggplot2::coord_radial] like
#' [`guide_axis_theta()`][ggplot2::guide_axis_theta].
#'
#' @param key A [standard key][key_standard] specification. Defaults to
#'   [`key_auto()`]. See more information in the linked topic and the 'Details'
#'   section.
#' @inheritParams primitive_labels
#' @inheritParams primitive_line
#' @inheritParams compose_stack
#'
#' @details
#' Under the hood, this guide is a [stack composition][compose_stack] of a
#' [line][primitive_line], [ticks][primitive_ticks] and
#' [labels][primitive_labels] primitives.
#'
#' To set minor ticks, use `key = "minor"`, or use the `type` argument in
#' `key_manual()` or `key_map()`.
#'
#' To use this as a logarithmic axis, set `key = "log"`.
#'
#' @return A `<Guide>` object.
#' @export
#' @family standalone guides
#'
#' @examples
#' # A standard plot with custom keys
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   scale_x_continuous(
#'     guide = guide_axis_custom(key = key_minor())
#'   ) +
#'   scale_y_continuous(
#'     guide = guide_axis_custom(key = key_manual(c(20, 25, 30, 40)))
#'   )
#' p
#'
#' # Is translated to theta axis without fuss
#' p + coord_radial()
#'
#' # To use as logarithmic axis:
#' ggplot(msleep, aes(bodywt, brainwt)) +
#'   geom_point(na.rm = TRUE) +
#'   scale_x_continuous(
#'     transform = "log10",
#'     guide = guide_axis_custom("log")
#'   )
guide_axis_custom <- function(
  key = "auto", title = waiver(),
  theme = NULL, n.dodge = 1, check.overlap = FALSE, angle = waiver(),
  cap = "none", order = 0, position = waiver()
) {
  theme <- theme %||% theme()
  theme$gguidance.guide.spacing <-
    theme$gguidance.guide.spacing %||% unit(0, "cm")

  compose_stack(
    primitive_line(cap = cap, position = position),
    primitive_ticks(position = position),
    primitive_labels(
      angle = angle, n.dodge = n.dodge, check.overlap = check.overlap
    ),
    key = key, side.titles = NULL, drop = 3L,
    title = title, theme = theme, order = order,
    available_aes = c("x", "y", "r", "theta"),
    position = position
  )
}
