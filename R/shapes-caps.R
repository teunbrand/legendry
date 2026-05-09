# Cap types ---------------------------------------------------------------

#' Cap options
#'
#' These functions construct various sorts of caps. They construct a matrix
#' that can be supplied as the `shape` argument in [`gizmo_barcap()`].
#'
#' @param n An `<integer[n]>` number of points to use for the cap.
#'
#' @return A `<matrix[n, 2]>` with coordinates for points on the brackets.
#' @name cap_options
#'
#' @details
#' When designing custom cap shapes, the expectation is that the first point
#' starts at the `(0, 0)` coordinate and the last point ends at the `(0, 1)`
#' coordinate. The first column follows the orthogonal direction of the bar
#' whereas the second column follows the direction of the bar.
#'
#' @examples
#' plot(cap_arch(), type = 'l')
NULL

#' @export
#' @describeIn cap_options An equilateral triangle with `n = 3` points.
cap_triangle <- function() {
  cbind(
    c(0.0, 0.5, 1.0), # x
    c(0.0, sqrt(3.0) / 2.0, 0.0)# y
  )
}

#' @export
#' @describeIn cap_options A semicircle.
cap_round <- function(n = 100L) {
  t <- seq(1.0, 0.0, length.out = n) * pi
  cbind(
    cos(t) * 0.5 + 0.5,
    sin(t) * 0.5
  )
}

#' @export
#' @describeIn cap_options Two circular arcs forming an equilateral Gothic arch.
cap_arch <- function(n = 100L) {
  half_n <- round(n / 2L)
  t <- seq((1.0 * pi) / 3.0, 0.0, length.out = half_n)
  cbind(
    c(1.0 - rev(cos(t)), cos(t)),
    c(rev(sin(t)), sin(t))
  )
}

#' @export
#' @describeIn cap_options Four circular arcs forming an 'ogee' arch.
cap_ogee <- function(n = 100L) {
  quart_n <- round(n / 4L)
  t <- seq(0.0, 1.0 / 3.0, length.out = quart_n) * pi
  top <- sqrt(3.0)

  x <- c(0.0, cos(t + pi) + 1.0, rev(cos(t)), -cos(t) + 2.0, rev(cos(t)) + 1.0)
  y <- c(0.0, -sin(t + pi), top - rev(sin(t)), top - sin(t), rev(sin(t)))
  cbind(x / 2.0, y / 2.0)
}

#' @export
#' @describeIn cap_options No cap.
cap_none <- function() {
  cbind(c(0.0, 1.0), c(0.0, 0.0))
}

# Helpers -----------------------------------------------------------------

resolve_cap_shape <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is.character(x)) {
    x <- paste0("cap_", x)
    fun <- find_global(x, env = global_env(), mode = "function")
    if (!is.function(fun)) {
      cli::cli_abort("Cannot find function: {.fn {x}}", call = call)
    }
    x <- fun
  }
  if (is.function(x)) {
    msg <- "{.arg {arg}} must return a {type}, not {obj_type_friendly(x)}."
    x <- x()
  } else {
    msg <- "{.arg {arg}} must be a {type}, not {obj_type_friendly(x)}"
  }
  if (is.matrix(x) && ncol(x) == 2L && nrow(x) > 1L) {
    return(x)
  }
  type <- as_cli("a {.cls matrix}")
  if (ncol(x) != 2L) {
    type <- as_cli("a {.cls matrix} with 2 columns")
    msg <- c(msg, "The provided {.arg {arg}} has {ncol(x)} column{?s}.")
  }
  if (nrow(x) < 2L) {
    type <- as_cli("a {.cls matrix} with 2 or more rows")
    msg <- c(msg, "The provided {.arg {arg}} has {nrow(x)} row{?s}.")
  }
  cli::cli_abort(msg, call = call)
}
