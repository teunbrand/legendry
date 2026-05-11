# Constructor -------------------------------------------------------------

#' Custom axis guide
#'
#' This axis guide is a visual representation of position scales and can
#' represent the `x`, `y`, `theta` and `r` aesthetics. It differs from
#' [`guide_axis()`][ggplot2::guide_axis] in that it can accept custom keys
#' and is can act as an axis for [`coord_radial()`][ggplot2::coord_radial] like
#' [`guide_axis_theta()`][ggplot2::guide_axis_theta].
#'
#' @param key
#' A [standard key][key_standard] specification. Defaults to
#' [`key_auto()`]. See more information in the linked topic and the 'Details'
#' section.
#' @param subtitle Passed on to [`primitive_title(title)`][primitive_title].
#'   Follow the linked topic for more details.
#' @inheritParams primitive_labels
#' @inheritParams primitive_line
#' @inheritParams primitive_ticks
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
#' ## Styling options
#'
#' Because this guide is pure composite guide, the [theme][ggplot2::theme]
#' options that govern the styling are determined by its constituents. They are
#' linked below so you can find their 'Styling options' sections.
#'
#' | **Primitive** | **Description** |
#' | ------------- | --------------- |
#' | [`compose_stack`] | Stacks the lines, tick marks and labels. |
#' | [`primitive_line()`] | Makes up the axis line. |
#' | [`primitive_ticks()`] | Makes up the tick marks. |
#' | [`primitive_labels()`] | Makes up the labels. |
#'
#' Styling options *per break* can be set in the [standard key][key_standard].
#' These override theme settings.
#'
#' The context-agnostic alternative to using `theme()` is to use
#' [`theme_guide()`]:
#'
#' ```r
#' guide_axis_base(theme = theme_guide(
#'   # Common options
#'   line = element_line(),
#'   text = element_text(),
#'   ticks = element_line(),
#'   ticks.length = unit(5, "mm"),
#'
#'   # Niche options below
#'   minor.ticks = element_line(),
#'   minor.ticks.length = unit(5, "mm"),
#'   mini.ticks = element_line(),
#'   mini.ticks.length = unit(5, "mm"),
#' ))
#' ```
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
#'     guide = guide_axis_base(key = key_minor())
#'   ) +
#'   scale_y_continuous(
#'     guide = guide_axis_base(key = key_manual(c(20, 25, 30, 40)))
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
#'     guide = guide_axis_base("log")
#'   )
guide_axis_base <- function(
  key = NULL, title = waiver(), subtitle = NULL, theme = NULL,
  n.dodge = 1L, check.overlap = FALSE, angle = waiver(),
  cap = "none", bidi = FALSE, order = 0L, position = waiver()
) {
  theme <- theme %||% theme()
  theme$legendry.guide.spacing <-
    theme$legendry.guide.spacing %||% unit(0.0, "cm")

  guides <- list(
    primitive_line(cap = cap, position = position),
    primitive_ticks(bidi = bidi, position = position),
    primitive_labels(
      angle = angle, n.dodge = n.dodge, check.overlap = check.overlap
    )
  )

  if (!is.null(subtitle)) {
    guides <- c(guides, list(primitive_title(subtitle)))
  }

  compose_stack(
    !!!guides,
    key = key, side.titles = NULL, drop = 3L,
    title = title, theme = theme, order = order,
    available_aes = c("any", "x", "y", "r", "theta"),
    position = position
  )
}
