# Bracket types -----------------------------------------------------------

#' Bracket options
#'
#' These functions construct various sorts of brackets. They construct a matrix
#' that can be supplied as the `bracket` argument in [`guide_axis_nested()`].
#'
#' @param n An `integer(1)` number of points to use for the bracket.
#' @param curvature A `numeric(1)` that controls the curliness of the bracket.
#'   More precisely, it is used to construct the sequence
#'   `seq(-curvature, curvature, length.out = n)` over which the logistic or
#'   arctangent functions is evaluated.
#' @param angle A `numeric(1)`: the angle in degrees for which a circle piece
#'   is drawn. For `bracket_curvy()`, an angle between 180 and 270.
#'
#' @return A `<matrix[n, 2]>` with coordinates for points on the brackets.
#' @name bracket_options
#'
#' @examples
#' plot(bracket_sigmoid(), type = 'l')
NULL

#' @export
#' @describeIn bracket_options A simple line as bracket. Has `n = 2` points.
bracket_line <- function() {
  cbind(c(0, 1), 0.5)
}

#' @export
#' @describeIn bracket_options A square bracket. Has `n = 4` points.
bracket_square <- function() {
  cbind(c(1, 1, 0, 0), c(1, 0, 0, 1))
}

#' @export
#' @describeIn bracket_options A chevron (V-shape) that makes a bracket. Has
#'   `n = 3` points.
bracket_chevron <- function() {
  cbind(c(0, 0.5, 1), c(1, 0, 1))
}

#' @export
#' @describeIn bracket_options One circular arc that makes a bracket.
bracket_round <- function(angle = 180, n = 100) {
  t <- deg2rad(seq(0, angle, length.out = n) - 0.5 * angle + 180)
  y <- rescale(cos(t))
  x <- rescale(sin(t))
  cbind(x, y)
}

#' @export
#' @describeIn bracket_options Two sigmoid curves stacked on top of one another
#'   to form a bracket.
bracket_sigmoid <- function(curvature = 10, n = 100) {
  half_n <- round(n / 2)
  y <- c(seq(1, 0, length.out = half_n), seq(0, 1, length.out = half_n))
  x <- 1 / (1 + exp(-seq(-curvature, curvature, length.out = half_n)))
  x <- rescale(x)
  x <- c(x, 1 + x) / 2
  cbind(x, y)
}

#' @export
#' @describeIn bracket_options Two arctangent curves stacked on top of one
#'   another to form a bracket.
bracket_atan <- function(curvature = 5, n = 100) {
  half_n <- round(n / 2)
  y <- c(seq(1, 0, length.out = half_n), seq(0, 1, length.out = half_n))
  x <- atan(seq(-curvature, curvature, length.out = half_n))
  x <- rescale(x)
  x <- c(x, 1 + x) / 2
  cbind(x, y)
}

#' @export
#' @describeIn bracket_options Four circular arcs that make a bracket.
bracket_curvy <- function(angle = 225, n = 100) {
  quarter_n <- round(n / 4)
  angle <- pmax(pmin(angle, 270), 180) / 180

  t <- c(
    seq(0.5, angle, length.out = quarter_n),
    seq(angle + 1, 1.5, length.out = quarter_n)
  ) * pi
  y <- cos(t) + rep(c(0, cos(angle * pi) * 2), each = quarter_n)
  x <- sin(t) + rep(c(0, sin(angle * pi) * 2), each = quarter_n)
  y <- rescale(y)
  x <- rescale(x, to = c(0.5, 1))
  x <- c(x, 1 - rev(x))
  y <- c(y, rev(y))
  cbind(x, y)
}
