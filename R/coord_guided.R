# Public constructor ------------------------------------------------------

#' Cartesian coordinates with grid guides.
#'
#' This coordinate system offers the additional flexibility of being able to set
#' a guide for the panel grid.
#'
#' @param guide A `<GuideGrid>` object giving a guide to draw the grid with. Can
#'   be constructed using functions listed in the 'See Also' section.
#' @param ratio Either `NULL` (default) or `numeric(1)`. If `numeric(1)`, locks
#'   the aspect ratio of panels, expressed as `y / x`.
#' @inheritParams ggplot2::coord_cartesian
#'
#' @return A `<Coord>` ggproto object that can be added to a plot.
#' @seealso Constructors: [`guide_grid_vanilla()`], [`guide_grid_zebra()`].
#' @export
#'
#' @examples
#' # Either choose a guide by name...
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   coord_guided(guide = "grid_zebra")
#'
#' # ... or using a constructor
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   coord_guided(guide = guide_grid_zebra())
coord_guided <- function(
  guide = guide_grid_vanilla(),
  xlim    = NULL, ylim    = NULL,
  ratio   = NULL, expand  = TRUE,
  clip    = "on", default = FALSE
) {
  guide <- validate_guide(guide)
  guide <- arg_class(guide, "GuideGrid")
  ggproto(
    NULL, CoordGuided,
    guide   = guide,
    limits  = list(x = xlim, y = ylim),
    expand  = expand,
    default = default,
    clip    = clip,
    ratio   = ratio
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
CoordGuided <- ggproto(
  "CoordGuided", CoordCartesian,

  expand  = FALSE,
  default = FALSE,
  ratio   = NULL,
  clip    = "on",
  limits  = list(x = c(NA, NA), y = c(NA, NA)),

  is_free = function(self) is.null(self$ratio),

  aspect  = function(self, ranges) {
    if (is.null(self$ratio)) {
      return(NULL)
    }
    diff(ranges$y.range) / diff(ranges$x.range) * self$ratio
  },

  render_bg = function(self, panel_params, theme) {
    self$guide$draw_guide(panel_params, theme)
  }
)


