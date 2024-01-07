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
    c(0, 0.5, 1), # x
    c(0, sqrt(3) / 2, 0)# y
  )
}

#' @export
#' @describeIn cap_options A semicircle.
cap_round <- function(n = 100) {
  t <- seq(1, 0, length.out = n) * pi
  cbind(
    cos(t) * 0.5 + 0.5,
    sin(t) * 0.5
  )
}

#' @export
#' @describeIn cap_options Two circular arcs forming an equilateral Gothic arch.
cap_arch <- function(n = 100) {
  half_n <- round(n / 2)
  t <- seq((1 * pi) / 3, 0, length.out = half_n)
  cbind(
    c(1 - rev(cos(t)), cos(t)),
    c(rev(sin(t)), sin(t))
  )
}

#' @export
#' @describeIn cap_options Four circular arcs forming an 'ogee' arch.
cap_ogee <- function(n = 100) {
  quart_n <- round(n / 4)
  t <- seq(0, 1/3, length.out = quart_n) * pi
  top <- sqrt(3)

  x <- c(0, cos(t + pi) + 1, rev(cos(t)), -cos(t) + 2, rev(cos(t)) + 1) / 2
  y <- c(0, -sin(t + pi), top - rev(sin(t)), top - sin(t), rev(sin(t))) / 2
  out <- cbind(x, y)
}

#' @export
#' @describeIn cap_options No cap.
cap_none <- function() {
  cbind(c(0, 1), c(0, 0))
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
  if (is.matrix(x) && ncol(x) == 2 & nrow(x) > 1) {
    return(x)
  }
  if (!is.matrix(x)) {
    type <- as_cli("a {.cls matrix}")
    cli::cli_abort(msg, call = call)
  }
  if (!ncol(x) == 2) {
    type <- as_cli("a {.cls matrix} with 2 columns")
    cli::cli_abort(msg, call = call)
  }
  if (nrow(x) < 2) {
    type <- as_cli("a {.cls matrix} with >1 row")
    msg <- c(msg, "The provided {.arg {arg}} has {nrow(x)} row{?s}.")
  }
  cli::cli_abort(msg)
}
