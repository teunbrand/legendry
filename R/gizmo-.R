
#' @name guide-gizmos
#' @title Guide gizmos
#'
#' @description
#' Guide gizmos are a speciality guide components that are very specific to
#' one or a few aesthetics to display.
NULL

extract_colourbar <- function(scale, aesthetic, nbin = 15, alpha = NA, ...) {
  if (scale$is_discrete()) {
    cli::cli_abort("Cannot use {.fn gizmo_colourbar} for discrete scales.")
  }

  limits <- scale$get_limits()
  key <- seq(limits[1], limits[2], length.out = nbin)
  if (length(key) == 0) {
    key <- vec_unique(limits)
  }
  key <- data_frame0(
    colour = alpha(scale$map(key), alpha),
    value  = key,
    .size  = length(key)
  )
  key
}
