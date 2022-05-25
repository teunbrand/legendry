# Public constructor ------------------------------------------------------

#' Vanilla grid guide
#'
#' This guide draws the background of a plotting panel. These include the
#' panel background itself and the major/minor grid lines.
#'
#' @return A `<Guide>` ggproto object that can be given to `coord_guided()`.
#' @export
#' @family vanilla guides
#' @family grid variants
#'
#' @examples
#' # This is very similar to regular cartesian coords
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   coord_guided(guide = guide_grid_vanilla())
guide_grid_vanilla <- function() {
  construct_grid()
}

# Internal constructor ----------------------------------------------------

construct_grid = function(..., super = GuideGrid) {
  ggproto(
    NULL, super,
    params = list(
      ...
    )
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideGrid <- ggproto(
  "GuideGrid", Guide,

  params = list(),

  draw_guide = function(self, params, theme) {

    params   <- self$setup_params(params)
    elements <- self$setup_elements(params, theme)

    backg <- self$build_background(params, elements)
    xgrid <- self$build_xgrid(params, elements)
    ygrid <- self$build_ygrid(params, elements)

    self$assemble_drawing(
      background = backg,
      xgrid      = xgrid,
      ygrid      = ygrid,
      params     = params
    )
  },

  setup_params = function(self, params) {
    x_major <- params$x$break_positions()
    y_major <- params$y$break_positions()
    x_minor <- setdiff(params$x$break_positions_minor(), x_major)
    y_minor <- setdiff(params$y$break_positions_minor(), y_major)

    c(self$params, params, list(
      x_major = x_major, x_minor = x_minor,
      y_major = y_major, y_minor = y_minor
    ))
  },

  setup_elements = function(params, theme) {
    list(
      x_major  = calc_element("panel.grid.major.x", theme),
      x_minor  = calc_element("panel.grid.minor.x", theme),
      y_major  = calc_element("panel.grid.major.y", theme),
      y_minor  = calc_element("panel.grid.minor.y", theme),
      background = calc_element("panel.background", theme)
    )
  },

  build_xgrid = function(params, elements) {
    major <- params$x_major
    minor <- params$x_minor
    ans <- list()
    if (length(major) > 0) {
      ans$major <- element_grob(
        elements$x_major,
        x = rep(major, each = 2),
        y = rep(0:1, length(major)),
        id.lengths = rep(2, length(major))
      )
    }
    if (length(params$x_minor) > 0) {
      ans$minor <- element_grob(
        elements$x_minor,
        x = rep(minor, each = 2),
        y = rep(0:1, length(minor)),
        id.lengths = rep(2, length(minor))
      )
    }
    ans
  },

  build_ygrid = function(params, elements) {
    major <- params$y_major
    minor <- params$y_minor
    ans <- list()
    if (length(major) > 0) {
      ans$major <- element_grob(
        elements$y_major,
        x = rep(0:1, length(major)),
        y = rep(major, each = 2),
        id.lengths = rep(2, length(major))
      )
    }
    if (length(minor) > 0) {
      ans$minor <- element_grob(
        elements$y_minor,
        x = rep(0:1, length(minor)),
        y = rep(minor, each = 2),
        id.lengths = rep(2, length(minor))
      )
    }
    ans
  },

  build_background = function(params, elements) {
    element_grob(elements$background)
  },

  assemble_drawing = function(background, xgrid, ygrid, params) {
    args <- list(
      background,
      ygrid$minor,
      xgrid$minor,
      ygrid$major,
      xgrid$major
    )
    args <- args[!vapply(args, is.null, logical(1))]
    do.call("grobTree", args)
  }
)
