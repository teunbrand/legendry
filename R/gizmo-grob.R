# Constructor -------------------------------------------------------------

#' Guide gizmo: custom grob
#'
#' This guide displays a user-provided grob.
#'
#' @param grob A [`<grob>`][grid::grob] to display.
#' @param width,height A [`<unit[1]>`][grid::unit] setting the allocated
#'   width and height of the the grob respectively.
#' @param hjust,vjust A `<numeric[1]>` between 0 and 1 setting the horizontal
#'   and vertical justification of the grob when used as a guide for the `x`
#'   and `y` aesthetics.
#' @param position Where this guide should be drawn: one of `"top"`, `"bottom"`,
#'   `"left"`, or `"right"`.
#'
#' @return A `<GizmoGrob>` object.
#' @export
#' @family gizmos
#'
#' @examples
#' circle <- grid::circleGrob()
#'
#' # A standard plot with grob gizmos
#' ggplot(mpg, aes(displ, hwy, colour = cty)) +
#'   geom_point() +
#'   guides(
#'     x.sec = gizmo_grob(
#'       circle, hjust = 0.75,
#'       width = unit(2, "cm"), height = unit(2, "cm")
#'     ),
#'     colour = gizmo_grob(
#'       circle, width = unit(1, "cm"), height = unit(1, "cm")
#'     )
#'   )
gizmo_grob <- function(
  grob, width = grobWidth(grob), height = grobHeight(grob),
  hjust = 0.5, vjust = 0.5,
  position = waiver()
) {

  check_object(grob, is.grob, "a {.cls grob} object")
  check_unit(width)
  check_unit(height)
  check_length(width, exact = 1)
  check_length(height, exact = 1)
  check_number_decimal(hjust, min = 0, max = 1, allow_infinite = FALSE)
  check_number_decimal(vjust, min = 0, max = 1, allow_infinite = FALSE)

  new_guide(
    grob   = grob,
    width  = width,
    height = height,
    hjust = hjust,
    vjust = vjust,
    hash = hash(grob),
    position = position,
    available_aes = c("any", "x", "y", "r"),
    super = GizmoGrob
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GizmoGrob <- ggproto(
  "GizmoGrob", Guide,

  params = new_params(
    grob = zeroGrob(), width = unit(0, "cm"), height = unit(0, "cm"),
    hjust = 0.5, vjust = 0.5
  ),

  elements = list(),

  train = function(params, scale, aesthetic = NULL, ...) {
    params$aesthetic <- aesthetic %||% scale$aesthetics[1]
    params$position  <- params$position %|W|% NULL
    params
  },

  transform = function(params, ...) {
    params
  },

  process_layers = function(params, ...) {
    params
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    width  <- unit(width_cm(params$width),   "cm")
    height <- unit(height_cm(params$height), "cm")

    gt <- gtable(widths = width, heights = height)
    gt <- gtable_add_grob(gt, params$grob, t = 1, l = 1, clip = "off", name = "gizmo_grob")

    if (params$aesthetic %in% c("x", "y")) {
      padding <- margin(
        t = 1 - params$vjust,
        b = params$vjust,
        r = 1 - params$hjust,
        l = params$hjust,
        unit = "null"
      )
      gt <- gtable_add_padding(gt, padding)
    }
    gt
  }
)
