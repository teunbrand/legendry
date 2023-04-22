#' Violin colour guide
#'
#' Like regular colour bar guides, this violin guide also shows continuous
#' colour scales mapped to values. In addition, the width of this guide can
#' vary. This varying width can be used to display densities like a violin plot.
#'
#' @param density One of the following: \itemize{
#'   \item{A named `list` with `x` and `y` elements, such as one returned by
#'   the [`density()`][stats::density()] function. The `x` and `y` elements
#'   should be of equal length and at least length 2. Infinite values for `x`
#'   will be set to the scale's limits.}
#'   \item{A `numeric` vector, which will be forwarded to the
#'   [`density()`][stats::density()] function with default arguments.}
#' }
#' @param just A `numeric(1)` in the \[0, 1\] range to determine the alignment
#'   of the violin. A value of `0` sets left- or bottom-alignment, whereas a
#'   value of `1` sets right- or top-alignment. A value of `0.5` (default)
#'   aligns the violin in the middle.
#' @param ticks.ontop A `logical(1)` whether to draw the ticks op top of the
#'   violin (`TRUE`) or underneath the violin (`FALSE`, default).
#' @inheritParams ggplot2::guide_colourbar
#' @inheritDotParams ggplot2::guide_colourbar
#'
#' @return A `<Guide>` ggproto object that can be given to the
#'   [`guides()`][ggplot2::guides()] function, or set as the `guide` argument
#'   in a colour or fill scale.
#' @export
#' @family colour bar variants
#'
#' @examples
#' # A standard plot with continuous colour scale
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = cty)) +
#'   scale_colour_viridis_c()
#'
#' # The guide can be given a variable of which to calculate density
#' p + guides(colour = guide_colour_violin(mpg$cty))
#'
#' # You can also pass the result of `density()` for greater control
#' p + guides(colour = guide_colour_violin(density(mpg$cty, adjust = 0.5)))
#'
#' # Alternatively, you can also pass a list manually
#' p + guides(
#'   colour = guide_colour_violin(density = list(x = c(-Inf, Inf), y = c(0, 1)))
#' )
#'
#' # The `just` argument can be used to align the density elsewhere
#' p + guides(colour = guide_colour_violin(mpg$cty, just = 0))
guide_colour_violin <- function(
  density = list(),
  just = 0.5,
  ticks = element_line("grey80"),
  ticks.length = unit(c(0, 1), "npc"),
  ticks.ontop  = FALSE,
  ...
) {

  if (!is.list(density)) {
    density <- density(density)
  }
  abort_if(
    !all(c("x", "y") %in% names(density)),
    "The {.arg density} argument must be a named {.cls list} with {.field x} ",
    "and {.field y} elements."
  )
  abort_if(
    (length(density$x) != length(density$y)) || length(density$x) < 2,
    "The {.field x} and {.field y} elements in the {.arg density} ",
    "argument must be of equal length and at least length 2.",
    i = c(
      "{.code density$x} is length {length(density$x)}.",
      "{.code density$y} is length {length(density$y)}."
    )
  )
  just <- arg_range(just, c(0, 1))[1]

  guide_colourbar(
    density = density,
    just    = just,
    ticks   = ticks,
    ticks.length = ticks.length,
    ticks.ontop  = ticks.ontop,
    ...,
    super = GuideColourViolin
  )
}

GuideColourViolin <- ggproto(
  "GuideColourViolin", GuideColourbar,
  params = c(
    GuideColourbar$params,
    list(
      density = list(x = c(-Inf, Inf), y = c(1, 1)),
      just = 0.5, ticks.ontop = FALSE
    )
  ),

  extract_decor = function(self, scale, aesthetic, nbin = 300, reverse = FALSE,
                           density, ...) {
    limits <- scale$get_limits()
    bar <- seq(limits[1], limits[2], length.out = nbin)
    if (length(bar) == 0) {
      bar <- unique0(limits)
    }

    # Fuss over the input now that limits are known
    xin <- oob_squish_infinite(density$x, limits)
    # xin <- scales::oob_censor(xin, range = limits)
    finite <- is.finite(xin)

    abort_if(
      sum(finite) < 2,
      "Need at least 2 values in {.arg density$x} to be inside scale limits.",
      i = "The scale limits are [{limits[1]}, {limits[2]}]."
    )
    abort_if(
      any(!finite),
      "{.fun {snake_class(self)}} has dropped {sum(!finite)} non-finite ",
      "values from the {.arg density$x} input."
    )

    apprx <- approx(xin[finite], density$y[finite], xout = bar)
    bar <- data_frame0(
      colour  = scale$map(bar),
      value   = bar,
      density = rescale(apprx$y, to = c(0, 0.95))
    )
    if (reverse) {
      bar <- bar[nrow(bar):1, , drop = FALSE]
    }
    bar
  },

  build_decor = function(decor, grobs, elements, params) {

    norm  <- rescale(decor$value)
    major <- c(norm, rev(norm))
    minor <- decor$density
    minor <- c((1 - minor) * params$just,
               1 - (1 - rev(minor)) * (1 - params$just))

    if (params$direction == "horizontal") {
      x  <- major
      y  <- minor
      x1 <- 0
      x2 <- 1
      y1 <- 0.5
      y2 <- 0.5
    } else {
      x  <- minor
      y  <- major
      x1 <- 0.5
      x2 <- 0.5
      y1 <- 0
      y2 <- 1
    }
    grad <- linearGradient(
      colours = decor$colour,
      stops   = norm,
      x1 = x1, x2 = x2,
      y1 = y1, y2 = y2
    )
    poly <- polygonGrob(
      x = x, y = y,
      gp = gpar(fill = grad, col = NA)
    )
    frame <- NULL
    if (!is_blank(elements$frame)) {
      frame <- polygonGrob(
        x = x, y = y,
        gp = gpar(
          fill = elements$frame$fill %||% NA,
          col  = elements$frame$colour %||% "black",
          lwd  = (elements$frame$linewidth %||% 0.5) * .pt,
          lty  = elements$frame$linetype %||% 1
        )
      )
    }
    if (params$ticks.ontop) {
      list(bar = poly, frame = frame, ticks = grobs$ticks)
    } else {
      list(ticks = grobs$ticks, bar = poly, frame = frame)
    }
  }
)
