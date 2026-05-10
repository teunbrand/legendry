# Constructor -------------------------------------------------------------

#' Custom colour steps guide
#'
#' Similar to [`guide_coloursteps()`][ggplot2::guide_coloursteps], this guide
#' displays continuous `colour` or `fill` aesthetics. It has additional options
#' to display caps at the end of the bar, depending on out-of-bounds values.
#'
#' @param first_guide,second_guide Guides to flank the colour steps. Each guide
#'   can be specified using one of the following:
#'   * A `<Guide>` class object.
#'   * A `<function>` that returns a `<Guide>` class object.
#'   * A `<character>` naming such a function, without the `guide_` or
#'   `primitive_` prefix.
#'
#' The `first_guide` will be placed at the location specified by the
#' `legend.text.position` theme setting. The `second_guide` will be placed
#' opposite that position. When `second_guide` has a label suppression
#' mechanism, no labels will be drawn for that guide.
#'
#' @inheritParams gizmo_stepcap
#' @inheritParams compose_sandwich
#' @param vanilla A `<logical[1]>` whether to have the default style match
#'   the vanilla `guide_colourbar()` (`TRUE`) or take the theme
#'   verbatim (`FALSE`).
#'
#' @details
#' As steps are rendered as clipped rectangles, it is important to use a
#' graphics device that can render clipped paths. This can be checked by using
#' [`check_device("clippingPaths")`][ggplot2::check_device].
#'
#' ## Styling options
#'
#' Because this guide is pure composite guide, the [theme][ggplot2::theme]
#' options that govern the styling are determined by its constituents. They are
#' linked below so you can find their 'Styling options' sections. Note that
#' `guide_axis_base()` is just a default that can be swapped out.
#'
#' | **Constituent** | **Description** |
#' | ------------- | --------------- |
#' | [`compose_sandwich`] | Combines the bar with two side-guides. |
#' | [`gizmo_stepcap()`] | Makes up the colour bar. |
#' | [`guide_axis_base()`] | Makes up the tick marks and labels. |
#'
#' Styling options *per break* can be set in the [standard key][key_standard].
#' These override theme settings.
#'
#' The context-agnostic alternative to using `theme()` is to use
#' [`theme_guide()`]:
#'
#' ```r
#' guide_colsteps(theme = theme_guide(
#'   # Composition settings
#'   title = element_text(),
#'   title.position = "top",
#'   text.position = "right",
#'   margin = margin(5),
#'   background = element_rect(),
#'
#'   # Steps settings
#'   frame = element_rect(),
#'   key.width = unit(5, "mm")
#'   key.height = unit(5, "cm")
#'
#'   # Common options for `guide_axis_base()`
#'   line = element_line(),
#'   text = element_text(),
#'   ticks = element_line(),
#'   ticks.length = unit(5, "mm"),
#' ))
#' ```
#'
#' @return A `<Guide>` object
#' @export
#' @family standalone guides
#'
#' @examples
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = cty))
#'
#' # The colour steps show caps when values are out-of-bounds
#' p + scale_colour_viridis_b(
#'   limits = c(10, NA),
#'   guide = "colsteps"
#' )
#'
#' # It also shows how oob values are handled
#' p + scale_colour_viridis_b(
#'   limits = c(10, 30), oob = scales::oob_censor,
#'   guide = "colsteps"
#' )
#'
#' # Adjusting the type of cap
#' p + scale_colour_viridis_b(
#'   limits = c(10, 30),
#'   guide = guide_colsteps(shape = "round")
#' )
#'
#' # The default is to use the breaks as-is
#' p + scale_colour_viridis_b(
#'   limits = c(10, 30), breaks = c(10, 20, 25),
#'   guide = "colsteps"
#' )
#'
#' # But the display can be set to use evenly spaced steps
#' p + scale_colour_viridis_b(
#'   limits = c(10, 30), breaks = c(10, 20, 25),
#'   guide = guide_colsteps(key = key_bins(even.steps = TRUE))
#' )
#'
#' # Using tick marks by swapping side guides
#' p + scale_colour_viridis_b(
#'   guide = guide_colsteps(
#'     first_guide  = "axis_base",
#'     second_guide = "axis_base"
#'   )
#' )
guide_colsteps <- function(
  title = waiver(),
  key = "bins",
  first_guide = "axis_base",
  second_guide = "axis_base",
  shape = "triangle",
  size = NULL,
  show = NA,
  alpha = NA,
  reverse = FALSE,
  suppress_labels = "second",
  oob = scales::oob_keep,
  theme = NULL,
  position = waiver(),
  vanilla = TRUE,
  available_aes = c("colour", "fill")
) {

  steps <- gizmo_stepcap(
    key = NULL, shape = shape, size = size, show = show, alpha = alpha,
    oob = oob
  )

  defaults <- if (isTRUE(vanilla)) vanilla_coloursteps_theme() else NULL

  suppress_labels <- recode(
    suppress_labels,
    old = c("first", "second"),
    new = c("text", "opposite")
  )

  compose_sandwich(
    key = key,
    middle = steps,
    text = first_guide,
    opposite = second_guide,
    suppress_labels = suppress_labels,
    complete = TRUE,
    title = title,
    theme = theme,
    theme_defaults = defaults,
    position = position,
    available_aes = available_aes
  )
}

vanilla_coloursteps_theme <- function(...) {
  theme(
    legend.axis.line = element_blank(),
    legend.ticks = element_blank(),
    legend.ticks.length = rel(-1.0),
    legend.frame = element_blank(),
    ...
  )
}
