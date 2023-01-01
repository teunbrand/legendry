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
#' @describeIn bracket_options Two sigmoid curves stacked on top of one another
#'   to form a bracket.
bracket_sigmoid <- function(curvature = 10, n = 100) {
  half_n  <- round(n / 2)
  x <- c(seq(1, 0, length.out = half_n), seq(0, 1, length.out = half_n))
  y <- 1 / (1 + exp(-seq(-curvature, curvature, length.out = half_n)))
  y <- rescale(y)
  y <- c(y, 1 + y) / 2
  cbind(x, y)
}

#' @export
#' @describeIn bracket_options Two arctangent curves stacked on top of one
#'   another to form a bracket.
bracket_atan <- function(curvature = 5, n = 100) {
  half_n <- round(n / 2)
  x <- c(seq(1, 0, length.out = half_n), seq(0, 1, length.out = half_n))
  y <- atan(seq(-curvature, curvature, length.out = half_n))
  y <- rescale(y)
  y <- c(y, 1 + y) / 2
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
  x <- cos(t) + rep(c(0, cos(angle * pi) * 2), each = quarter_n)
  y <- sin(t) + rep(c(0, sin(angle * pi) * 2), each = quarter_n)
  x <- rescale(x)
  y <- rescale(y, to = c(0.5, 1))
  x <- c(x, rev(x))
  y <- c(y, 1 - rev(y))
  cbind(x, y)
}

#' @export
#' @describeIn bracket_options A simple line as bracket. Has `n = 2` points.
bracket_line <- function() {
  cbind(0.5, c(0, 1))
}

#' @export
#' @describeIn bracket_options One circular arc that makes a bracket.
bracket_round <- function(angle = 180, n = 100) {
  t <- seq(0, angle, length.out = n) - 0.5 * angle + 180
  t <- t * pi / 180
  x <- rescale(cos(t))
  y <- rescale(sin(t))
  cbind(x, y)
}

#' @export
#' @describeIn bracket_options A chevron (V-shape) that makes a bracket. Has
#'   `n = 3` points.
bracket_chevron <- function() {
  cbind(
    c(1, 0, 1),
    c(0, 0.5, 1)
  )
}

#' @export
#' @describeIn bracket_options A square bracket. Has `n = 4` points.
bracket_square <- function() {
  cbind(
    c(1, 0, 0, 1),
    c(1, 1, 0, 0)
  )
}

# Other bracket related functions -----------------------------------------

validate_bracket <- function(bracket) {
  if (is.character(bracket)) {
    fun <- find_global(paste0("bracket_", bracket), env = global_env(),
                       mode = "function")
    if (is.function(fun)) {
      bracket <- fun()
    } else {
      cli::cli_abort("Unknown {.arg bracket}: {bracket}")
    }
  }
  bracket <- arg_class(bracket, "matrix")
  if (ncol(bracket) != 2) {
    cli::cli_abort(
      "The {.arg bracket} argument must be a {.cls matrix} with 2 columns."
    )
  }
  if (nrow(bracket) < 2) {
    cli::cli_abort(c(
      "The {.arg bracket} argument requires at least 2 points to draw a line.",
      "i" = "The provided {.arg bracket} has {nrow(bracket)} row{?s}."
    ))
  }
  bracket
}

expand_bracket <- function(
    ranges, bracket,
    position, size,
    element = element_line()
) {
  # Set parameters
  aes  <- if (position %in% c("top", "bottom")) "x" else "y"
  orth <- setdiff(c("x", "y"), aes)
  end  <- paste0(aes, "end")
  secondary <- position %in% c("top", "right")
  if (aes == "x") {
    height <- size
    width  <- unit(1, "npc")
  } else {
    height <- unit(1, "npc")
    width  <- size
  }

  # Expand brackets
  major <- bracket[, 2]
  minor <- bracket[, 1]
  n     <- nrow(bracket)
  if (secondary) {
    minor <- 1 - minor
  }

  diff <- unclass(ranges[[end]] - ranges[[aes]])
  mtx  <- sweep(outer(major, diff), 2, ranges[[aes]], `+`)

  # Convert to grobs
  ulevels <- unique(ranges$.level)
  lapply(ulevels, function(lvl) {
    mat <- mtx[, ranges$.level == lvl, drop = FALSE]
    absolute_element(
      element = element,
      !!aes  := as.vector(mat),
      !!orth := rep(minor, ncol(mat)),
      id.lengths = rep(n, ncol(mat)),
      width  = width,
      height = height
    )
  })
}
