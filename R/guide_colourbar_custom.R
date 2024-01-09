# Constructor -------------------------------------------------------------

#' Custom colour bar guide
#'
#' Similar [`guide_colourbar()`][ggplot2::guide_colourbar], this guide displays
#' continuous `colour` or `fill` aesthetics. It has additional options to
#' display caps at the end of the bar, depending on out-of-bounds values.
#'
#' @param first_guide,second_guide Guides to flank the colour bar. Each guide
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
#' @inheritParams gizmo_barcap
#' @inheritParams compose_sandwich
#'
#' @details
#' As colours are always rendered as gradients, it is important to use a
#' graphics device that can render these.
#'
#' @return A `<Guide>` object
#' @export
#' @family standalone guides
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = cty))
#'
#' # The colourbar shows caps when values are out-of-bounds (oob)
#' p + scale_colour_viridis_c(
#'   limits = c(10, NA),
#'   guide = "colourbar_custom"
#' )
#'
#' # It also shows how oob values are handled
#' p + scale_colour_viridis_c(
#'   limits = c(10, NA), oob = oob_squish,
#'   guide = "colourbar_custom"
#' )
#'
#' # Adjusting the type of cap
#' p + scale_colour_viridis_c(
#'   limits = c(10, 30), oob = oob_squish,
#'   guide = guide_colourbar_custom(shape = "round")
#' )
#'
#' # One-sided ticks
#' p + scale_colour_viridis_c(
#'   guide = guide_colourbar_custom(second_guide = "none")
#' )
#'
#' # Colour bar with minor breaks
#' p + scale_colour_viridis_c(
#'   minor_breaks = breaks_width(1),
#'   guide = guide_colourbar_custom(key = "minor")
#' )
#'
#' # Using log ticks on a colourbar
#' ggplot(msleep, aes(sleep_total, sleep_rem)) +
#'   geom_point(aes(colour = bodywt), na.rm = TRUE) +
#'   scale_colour_viridis_c(
#'     transform = "log10",
#'     guide = guide_colourbar_custom(key = "log")
#'   )
guide_colourbar_custom <- function(
  title = waiver(),
  key = "auto",
  first_guide  = "axis_custom",
  second_guide = first_guide,
  shape = "triangle",
  size = NULL,
  show = NA,
  nbin = 15,
  alpha = NA,
  reverse = FALSE,
  oob = oob_keep,
  theme = NULL,
  position = waiver(),
  available_aes = c("colour", "fill")
) {

  bar <- gizmo_barcap(
    key = key_sequence(nbin),
    shape = shape, size = size, show = show, alpha = alpha,
    oob = oob
  )

  compose_sandwich(
    key = key,
    middle = bar,
    text = first_guide,
    opposite = second_guide,
    reverse = reverse,
    complete = TRUE,
    title = title,
    theme = theme,
    theme_defaults = .theme_defaults_colourbar,
    position = position,
    available_aes = available_aes
  )
}

.theme_defaults_colourbar <- theme(
  legend.axis.line = element_blank(),
  legend.ticks = element_line(colour = "white", linewidth = 0.5 / .pt),
  legend.ticks.length = unit(-0.2 * 1.2, "lines")
)
