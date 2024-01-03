# Constructor -------------------------------------------------------------

#' Guide primitive: spacer
#'
#' This function constructs a spacer [guide primitive][guide-primitives].
#'
#' @param space A [`<unit[1]>`][grid::unit()]
#' @inheritParams ggplot2::guide_axis
#'
#' @return A `<PrimitiveSpacer>` primitive guide that can be used inside
#'   other guides.
#' @export
#' @family primitives
#'
#' @examples
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   guides(
#'     x = guide_axis_stack("axis", primitive_spacer(unit(1, "cm")), "axis")
#'   )
primitive_spacer <- function(space = NULL, title = waiver(),
                             theme = NULL, position = waiver()) {
  check_object(space, is.unit, "a {.cls unit}", allow_null = TRUE)
  new_guide(
    space = space, title = title, theme = theme, position = position,
    available_aes = c("any", "x", "y", "r", "theta"),
    super = PrimitiveSpacer
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
PrimitiveSpacer <- ggproto(
  "Spacer", Guide,

  params = new_params(space = NULL),

  train = function(self, params = self$params, scale, aesthetic = NULL, ...) {
    params$aesthetic <- aesthetic %||% scale$aesthetics[1]
    params$position  <- params$position %|W|% NULL
    params
  },

  transform = function(self, params, coord, panel_params) {
    params
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    space <- params$space %||%
      calc_element("gguidance.guide.spacing", theme + params$theme)
    primitive_grob(
      zeroGrob(), sum(space), params$position %||% position, "spacer"
    )
  }
)
