# Public constructor ------------------------------------------------------

#' Swapped grid guide
#'
#' This grid guide swaps the display of major and minor breaks. This may be
#' particularly handy for discrete scales, to draw the major grid as a separator
#' between categorical values, instead of at the center.
#'
#' @param swap A `character(1)`, one of `"x"`, `"y"` or `"xy"` indicated the
#'   grid directions to swap. `"xy"` (default) will swap both the x and y grids.
#' @param force_minor A `logical(1)`. If `TRUE` (default), regular minor breaks
#'   are calculated when missing, which is often the case for discrete scales.
#'   If `FALSE`, just give major grid a minor grid appearance.
#'
#' @inherit guide_grid_vanilla return
#' @export
#' @family grid variants
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(class)) +
#'   geom_bar()
#'
#' # Swapping major grid to minor positions
#' p + coord_guided("grid_swap")
#'
#' # Only swap x-grid, leave y-grid as-is
#' p + coord_guided(guide_grid_swap("x"))
#'
#' # Just display major as minor, don't make new minor when missing
#' p + coord_guided(guide_grid_swap(force_minor = FALSE))
guide_grid_swap <- function(
  swap = "xy", force_minor = TRUE
) {
  swap <- arg_match0(swap, c("x", "y", "xy"))
  swap <- list(
    x = swap %in% c("x", "xy"),
    y = swap %in% c("y", "xy")
  )

  construct_grid(
    swap        = swap,
    force_minor = !isFALSE(force_minor),
    super       = GuideGridSwap
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideGridSwap <- ggproto(
  "GuideGridSwap", GuideGrid,

  setup_params = function(self, params) {
    params <- ggproto_parent(GuideGrid, self)$setup_params(params)

    if (params$swap$x) {

      # Swap x major/minor
      params[c("x_major", "x_minor")] <- params[c("x_minor", "x_major")]


      if (is.null(params$x_major) && params$force_minor) {

        # Calculate minor breaks
        scale  <- params$x
        breaks <- scale$get_breaks()
        if (scale$is_discrete()) {
          breaks <- scale$map(breaks)
        }
        alt <- regular_minor_breaks()(breaks, scale$continuous_range, 2)
        alt <- scale$rescale(alt)
        alt <- setdiff(alt, params$x_minor)

        # Set minor breaks as major
        params$x_major <- alt
        params$x_minor <- NULL
      }
    }
    if (params$swap$y) {

      # Swap y major/minor
      params[c("y_major", "y_minor")] <- params[c("y_minor", "y_major")]

      if (is.null(params$y_major) && params$force_minor) {
        # Calculate minor breaks
        scale  <- params$y
        breaks <- scale$get_breaks()
        if (scale$is_discrete()) {
          breaks <- scale$map(breaks)
        }
        alt <- regular_minor_breaks()(breaks, scale$continuous_range, 2)
        alt <- scale$rescale(alt)
        alt <- setdiff(alt, params$y_minor)

        # Set minor breaks as major
        params$y_major <- alt
        params$y_minor <- NULL
      }
    }
    params
  }
)
