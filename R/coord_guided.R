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
  guide = NULL,
  xlim  = NULL, ylim = NULL,
  ratio = NULL, expand = TRUE,
  clip = "on", default = FALSE
) {

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

  aspect = function(self, ranges) {
    if (is.null(self$ratio)) {
      return(NULL)
    }
    diff(ranges$y.range) / diff(ranges$x.range) * self$ratio
  },

  render_bg = function(self, panel_params, theme) {
    panel_params$grid_guide$draw(theme, panel_params$grid_params)
  },

  setup_panel_guides = function(self, panel_params, guides, params = list()) {
    # Call parent for training regular guides
    panel_params <- ggproto_parent(CoordCartesian, self)$setup_panel_guides(
      panel_params, guides, params
    )

    # Setup grid guide
    grid <- guides$guides[["grid"]] %||% self$guide %||% guide_grid()
    if (is.character(grid)) {
      fun <- find_global(paste0("guide_", grid), env = global_env(),
                         mode = "function")
      if (is.function(fun)) {
        grid <- fun()
      }
    }

    # Validate guide
    if (!inherits(grid, "Guide")) {
      cli::cli_warn("Unknown guide: {grid}.")
      grid <- guide_grid()
    }

    # Check aesthetics
    if (!all(c("x", "y", "grid") %in% grid$available_aes) &&
        !inherits(grid, "GuideNone")) {
      aes <- grid$available_aes
      aes[aes == "any"] <- "any non-position aesthetic"
      cli::cli_warn(c(paste0(
        "{.fn {snake_class(grid)}} cannot be used for drawing a panel grid."
      ), i = "Use {?one of} {.or {.field {aes}}} instead."))
      grid <- guide_grid()
    }

    panel_params$grid_guide  <- grid
    panel_params$grid_params <- grid$params
    panel_params
  },

  train_panel_guides = function(self, panel_params, layers, params = list()) {

    panel_params <- ggproto_parent(CoordCartesian, self)$train_panel_guides(
      panel_params, layers, params
    )

    grid <- panel_params$grid_guide
    if (inherits(panel_params$grid_guide, "GuideNone")) {
      return(panel_params)
    }
    params <- panel_params$grid_params

    params <- grid$train(params, panel_params[c("x", "y")])
    params <- grid$transform(params, self, panel_params)
    params <- grid$get_layer_key(params, layers)

    panel_params$grid_params <- params
    panel_params
  }
)
