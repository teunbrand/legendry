# Constructor -------------------------------------------------------------

#' Nested axis guide
#'
#' This axis guide gives extra range annotations to position scales. It can
#' be used to infer nesting structure from labels or annotate ranges.
#'
#' @param key A [range key][key_range] specification. If not
#'   `key = "range_auto"`, additional labels will be inserted to represent
#'   point values.
#' @param regular_key A [standard key][key_standard] specification for the
#'   appearance of regular tick marks.
#' @param type Appearance of ranges, either `"box"` to put text in boxes or
#'   `"bracket"` (default) to text brackets.
#' @inheritParams common_parameters
#' @inheritParams primitive_line
#' @inheritParams primitive_ticks
#' @inheritParams primitive_bracket
#' @param ... Arguments passed on to [`primitive_bracket()`] or
#'   [`primitive_box()`].
#'
#' @details
#' Under the hood, this guide is a [stack composition][compose_stack] of a
#' [line][primitive_line], [ticks][primitive_ticks], optionally
#' [labels][primitive_labels] and either [bracket][primitive_bracket] or
#' [box][primitive_box] primitives.
#'
#' By default, the [`key = "range_auto"`][key_range] will incorporate the 0th
#' level labels inferred from the scale's labels. These labels will look like
#' regular labels.
#'
#' To offer other keys the opportunity to display ranges alongside
#' regular-looking labels, the `regular_key` argument can be used to setup a
#' separate key for display in between the ticks and ranges.
#'
#' @return A `<Guide>` object.
#' @export
#' @family standalone guides
#'
#' @examples
#' # A plot with nested categories on the x-axis
#' p <- ggplot(mpg, aes(interaction(drv, cyl), hwy)) +
#'   geom_boxplot()
#'
#' p + guides(x = "axis_nested")
#'
#' # Apply styling to brackets
#' p + guides(x = "axis_nested") +
#'   theme_guide(bracket = element_line("red", linewidth = 1))
#'
#' # Don't drop nesting indicators that have 0-width
#' p + guides(x = guide_axis_nested(drop_zero = FALSE))
#'
#' # Change additional padding for discrete categories
#' p + guides(x = guide_axis_nested(pad_discrete = 0))
#'
#' # Change bracket type
#' p + guides(x = guide_axis_nested(bracket = "curvy"))
#'
#' # Use boxes instead of brackets + styling of boxes
#' p + guides(x = guide_axis_nested(type = "box")) +
#'   theme_guide(box = element_rect("limegreen", "forestgreen"))
#'
#' # Use as annotation of a typical axis
#' # `regular_key` controls display of typical axis
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   guides(x = guide_axis_nested(
#'     key = key_range_manual(start = 2:3, end = 5:6, name = c("First", "Second")),
#'     regular_key = key_manual(c(2, 2.5, 3, 5, 7))
#'   ))
guide_axis_nested <- function(
  key   = "range_auto",
  regular_key = "auto",
  type  = "bracket",
  title = waiver(),
  theme = NULL,
  angle = waiver(),
  cap   = "none",
  bidi  = FALSE,
  oob   = "squish",
  drop_zero = TRUE,
  pad_discrete = 0.4,
  levels_text = NULL,
  ...,
  order = 0,
  position = waiver()
) {

  theme <- theme %||% theme()
  theme$gguidance.guide.spacing <-
    theme$gguidance.guide.spacing %||% unit(0, "cm")

  nesting <- switch(
    arg_match0(type, c("bracket", "box")),
    bracket = primitive_bracket,
    box = primitive_box
  )

  if (identical(key, "range_auto")) {
    labels <- new_guide(
      available_aes = c("any", "x", "y", "r", "theta"),
      super = guide_none()
    )
  } else {
    labels <- primitive_labels(angle = angle)
  }

  compose_stack(
    primitive_line(cap = cap, position = position),
    primitive_ticks(bidi = bidi, position = position),
    labels,
    nesting(
      key = key %||% "range_auto", angle = angle,
      oob = oob, drop_zero = drop_zero, pad_discrete = pad_discrete,
      levels_text = levels_text, ...
    ),
    key = regular_key %||% "auto",
    side.titles = NULL, drop = 3:4, title = title, theme =theme,
    order = order, available_aes = c("any", "x", "y", "r", "theta"),
    position = position
  )
}
