# Specialty keys ----------------------------------------------------------

#' Specialty keys
#'
#' @description
#' These functions are helper functions for working with keys in guides. The
#' functions described here are not widely applicable and may only apply
#' to a small subset of guides.
#'
#' * `key_sequence()` is a function factory whose functions create a regularly
#'   spaced sequence between the limits of a scale. It is used in colour bar
#'   guides.
#'
#' @param n The number of colours to use for a gradient.
#'
#' @return
#' For `key_sequence()` a function.
#'
#' @name key_specialty
#' @family keys
#'
#' @examples
NULL

#' @export
#' @rdname key_specialty
key_sequence <- function(n = 15) {
  force(n)
  function(scale, aesthetic = NULL) {
    aesthetic <- aesthetic %||% scale$aesthetics[1]
    df <- map_sequence(scale = scale, aesthetic = aesthetic, nbin = n)
    class(df) <- c("key_sequence", "key", class(df))
    df
  }
}

map_sequence <- function(scale, aesthetic, nbin = 15, ...) {
  if (scale$is_discrete()) {
    cli::cli_abort("Cannot use {.fn key_sequence} for discrete scales.")
  }

  limits <- scale$get_limits()
  key <- seq(limits[1], limits[2], length.out = nbin)
  if (length(key) == 0) {
    key <- vec_unique(limits)
  }
  key <- data_frame0(
    !!aesthetic := scale$map(key),
    .value  = key,
    .size  = length(key)
  )
  key
}
