# Constructor -------------------------------------------------------------

#' Nested axis guide
#'
#' This axis guide gives extra range annotations to position scales. It can
#' be used to infer nesting structure from labels or annotate ranges.
#'
#' @param key One of the following:
#'   * A [range key][key_range] specification. If not `key = "range_auto"`,
#'     additional labels will be inserted to represent point values.
#'   * A `<character[1]>` passed to the [`key_range_auto(sep)`][key_range_auto]
#'     argument. An exception is made when the string is a valid key
#'     specification.
#' @param regular_key A [standard key][key_standard] specification for the
#'   appearance of regular tick marks.
#' @param type
#' Appearance of ranges. One of the following:
#' * `"box"` to put text in boxes.
#' * `"bracket"` (default) to text over brackets.
#' * `"fence"` to put text near fences.
#' @param subtitle Passed on to [`primitive_title(title)`][primitive_title].
#'   Follow the linked topic for more details.
#' @inheritParams common_parameters
#' @inheritParams primitive_line
#' @inheritParams primitive_ticks
#' @inheritParams primitive_bracket
#' @param ... Arguments passed on to [`primitive_bracket()`],
#'   [`primitive_box()`] or [`primitive_fence()`] (depending on the `type`
#'   argument).
#'
#' @details
#' To offer other keys the opportunity to display ranges alongside
#' regular-looking labels, the `regular_key` argument can be used to setup a
#' separate key for display in between the ticks and ranges.
#'
#' By default, the [`key = "range_auto"`][key_range] will incorporate the 0th
#' level labels inferred from the scale's labels. These labels will look like
#' regular labels.
#'
#' ## Styling options
#'
#' Because this guide is pure composite guide, the [theme][ggplot2::theme]
#' options that govern the styling are determined by its constituents. They are
#' linked below so you can find their 'Styling options' sections.
#'
#' | **Primitive** | **Context** | **Description** |
#' | ------------- | ----------- | --------------- |
#' | [`compose_stack`] | Always | Stacks the other primitives. |
#' | [`primitive_line()`] | Always | Makes up the axis line. |
#' | [`primitive_ticks()`] | Always | Makes up the tick marks. |
#' | [`primitive_bracket()`] | `type = "bracket"` | Range display as brackets. |
#' | [`primitive_box()`] | `type = "box"` | Range display as boxes. |
#' | [`primitive_fence()`] | `type = "fence"` | Range display as fences. |
#' | [`primitive_title()`] | `subtitle = <...>` | Used for displaying subtitles. |
#' | [`primitive_labels()`] | `key != "range_auto"` | Used for displaying range-less, 0th level labels |
#'
#' Styling options *per range* can be set in the [range key][key_range].
#' These override theme settings.
#'
#' The context-agnostic alternative to using `theme()` is to use
#' [`theme_guide()`]:
#'
#' ```r
#' guide_axis_nested(theme = theme_guide(
#'   # Common options
#'   line = element_line(),
#'   text = element_text(),
#'   ticks = element_line(),
#'   ticks.length = unit(5, "mm"),
#'
#'   # For brackets
#'   bracket = element_line(),
#'   bracket.size = unit(5, "mm"),
#'
#'   # For boxes
#'   box = element_rect(),
#'
#'   # For fences
#'   fence = element_line(),
#'   fence.post = element_line(),
#'   fence.rail = element_line(),
#'
#'   # For subtitle (not main title)
#'   title = element_text()
#' ))
#' ```
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
#' # Using fences instead of brackets + styling of fences
#' p + guides(x = guide_axis_nested(type = "fence", rail = "inner")) +
#'   theme_guide(
#'     fence.post = element_line("tomato"),
#'     fence.rail = element_line("dodgerblue")
#'   )
#'
#' # Use as annotation of a typical axis
#' # `regular_key` controls display of typical axis
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   guides(x = guide_axis_nested(
#'     key = key_range_manual(
#'       start = 2:3,
#'       end = 5:6,
#'       name = c("First", "Second")
#'     ),
#'     regular_key = key_manual(c(2, 2.5, 3, 5, 7))
#'   ))
guide_axis_nested <- function(
  key   = "range_auto",
  regular_key = "auto",
  type  = "bracket",
  title = waiver(),
  subtitle = NULL,
  theme = NULL,
  angle = waiver(),
  cap   = "none",
  bidi  = FALSE,
  oob   = "squish",
  drop_zero = TRUE,
  pad_discrete = NULL,
  levels_text = NULL,
  ...,
  order = 0L,
  position = waiver()
) {

  theme <- theme %||% theme()
  theme$legendry.guide.spacing <-
    theme$legendry.guide.spacing %||% unit(0.0, "cm")

  pad_discrete <- pad_discrete %||% switch(type, fence = 0.5, 0.4)

  guides <- list(
    primitive_line(cap = cap, position = position),
    primitive_ticks(bidi = bidi, position = position)
  )

  nesting <- switch(
    arg_match0(type, c("bracket", "box", "fence")),
    bracket = primitive_bracket,
    box     = primitive_box,
    fence   = primitive_fence
  )

  if (is_string(key) && !is_key_string(key)) {
    key <- key_range_auto(sep = key)
  }
  # Some of the `nesting` guides may already absorb label-duty
  automatic_labels <-
    identical(key, "range_auto") ||
    inherits(key, "key_range_auto_function") ||
    is.null(key)
  if (!automatic_labels) {
    # If not absorbed, roll out a formal primitive for labels
    guides <- c(guides, list(primitive_labels(angle = angle)))
  }

  # Populate nesting guide
  nesting <- nesting(
    key = key %||% "range_auto", angle = angle,
    oob = oob, drop_zero = drop_zero, pad_discrete = pad_discrete,
    levels_text = levels_text, ...
  )
  guides <- c(guides, list(nesting))

  # Add optional subtitle
  if (!is.null(subtitle)) {
    guides <- c(guides, list(primitive_title(subtitle)))
  }

  compose_stack(
    !!!guides,
    key = regular_key %||% "auto",
    side.titles = NULL, drop = 3L:4L, title = title, theme = theme,
    order = order, available_aes = c("any", "x", "y", "r", "theta"),
    position = position
  )
}
