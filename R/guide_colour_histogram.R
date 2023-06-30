#' Histogram colour guide
#'
#' Like regular colour bar guides, this histogram guide also shows continuous
#' colour scales mapped to values. In addition, the width of this guide can
#' vary
#'
#' @param hist One of the following: \itemize{
#'   \item{A named `list` with `breaks` and `counts` elements, such as one
#'   returned by the [`hist()`][graphics::hist()] function. The `breaks`
#'   element should be one longer than the `counts` element.}
#'   \item{A `numeric` vecotr, which will be forwarded to the
#'   [`hist()`][graphics::hist()] function with the `plot = FALSE` argument
#'   set.}
#' }
#' @param just A `numeric(1)` in the \[0, 1\] range to determine the alignment
#'   of the histogram. A valueo f `0` (default) sets left- or bottom-alignment,
#'   whereas a value of `1` sets right- or top-alignment. A value of `0.5`
#'   aligns the histogram in the middle.
#' @param ticks.ontop A `logical(1)` whether to draw the ticks on top of the
#'   histogram (`TRUE`) or underneath the histogram (`FALSE`, default).
#' @inheritParams ggplot2::guide_colourbar
#' @inheritDotParams ggplot2::guide_colourbar
#'
#' @return A `<Guide>` ggproto object that can be given to the
#'   [`guides()`][ggplot2::guides()] function, or set as the `guide` argument
#'   in a colour or fill scale.
#'
#' @export
#' @family colour bar variants
#'
#' @examples
#' # A standard plot with a continuous colour scale
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = cty)) +
#'   scale_colour_viridis_c()
#'
#' # The guide can be given a variable of which to calculate a histogram
#' p + guides(colour = guide_colour_histogram(mpg$cty))
#'
#' # You can also pass the result of `hist()` for greater control
#' p + guides(colour = guide_colour_histogram(hist(mpg$cty, breaks = 10)))
#'
#' # Alternatively, you can also pass a list manually
#' p + guides(colour = guide_colour_histogram(
#'   hist = list(breaks = 0:4 * 10, counts = 1:4)
#' ))
#'
#' # The `just` argument can be used to align the histogram elsewhere
#' p + guides(colour = guide_colour_histogram(mpg$cty, just = 1))
guide_colour_histogram <- function(
  hist  = list(),
  just  = 0,
  ticks = element_line("grey80"),
  ticks.length = unit(c(0, 1), "npc"),
  ticks.ontop  = FALSE,
  ...
) {
  hist <- suppress_hist_plot(enquo(hist))

  if (!is.list(hist)) {
    hist <- hist(hist, plot = FALSE)
  }
  abort_if(
    !all(c("breaks", "counts") %in% names(hist)),
    "The {.arg hist} argument must be a named {.cls list} with {.field breaks}",
    " and {.field counts} elements"
  )
  abort_if(
    length(hist$breaks) != (length(hist$counts) + 1),
    "In the {.arg hist} argument, the {.field breaks} element should be one ",
    "longer than {.field counts}.",
    i = c(
      "{.code hist$breaks} is length {length(hist$breaks)}.",
      "{.code hist$counts} is length {length(hist$counts)}."
    )
  )
  abort_if(length(hist$counts) < 1, "Cannot make an empty histogram.")
  arg_class(hist$breaks, "numeric")
  arg_class(hist$counts, "numeric")

  just <- arg_range(just, c(0, 1))[1]

  guide_colourbar(
    hist = hist,
    just = just,
    ticks = ticks,
    ticks.length = ticks.length,
    ticks.ontop = ticks.ontop,
    ...,
    super = GuideColourHistogram
  )
}

GuideColourHistogram <- ggproto(
  "GuideColourHistogram", GuideColourbar,
  params = c(
    GuideColourbar$params,
    list(
      hist = list(breaks = c(-Inf, Inf), counts = 1),
      just = 0.5, ticks.ontop = FALSE
    )
  ),

  extract_decor = function(self, scale, aesthetic, nbin = 300, reverse = FALSE,
                           hist, ...) {
    limits <- scale$get_limits()
    bar <- seq(limits[1], limits[2], length.out = nbin)
    if (length(bar) == 0) {
      bar <- unique0(limits)
    }
    bar <- data_frame0(
      colour = scale$map(bar),
      value  = bar
    )
    # Convert histogram to polygon
    hist <- data_frame0(
      x = rep(hist$breaks, each = 2),
      y = c(0, rep(hist$counts, each = 2), 0)
    )
    list(bar = bar, hist = hist)
  },

  extract_params = function(scale, params, hashables, title = waiver(),
                            direction = "vertical", ...) {

    params$title <- scale$make_title(params$title %|W|% scale$name %|W|%
                                       title)
    params$direction <- arg_match0(
      params$direction %||% direction,
      c("horizontal", "vertical"), arg_nm = "direction"
    )
    valid_label_pos <- switch(
      params$direction,
      horizontal = c("bottom", "top"),
      vertical   = c("right", "left")
    )
    params$label.position <- params$label.position %||% valid_label_pos[1]
    if (!params$label.position %in% valid_label_pos) {
      cli::cli_abort(paste0(
        "When {.arg direction} is {.val {params$direction}}, ",
        "{.arg label.position} must be one of {.or {.val {valid_label_pos}}}, ",
        "not {.val {params$label.position}}."
      ))
    }

    histlim <- range(params$hist$breaks)
    if (params$reverse) {
      histlim <- rev(histlim)
    }

    params$key$.value <- rescale(params$key$.value, from = histlim)
    Guide$extract_params(scale, params, hashables)
  },


  build_decor = function(decor, grobs, elements, params) {

    hist <- decor$hist
    decor <- decor$bar

    hist_lim <- range(hist$x)
    norm     <- rescale(decor$value, from = hist_lim)
    norm_lim <- range(norm)

    major <- rescale(hist$x, from = hist_lim)
    minor <- rescale(hist$y, from = c(0, max(hist$y)), to = c(0, 0.95))
    major <- c(major, rev(major))
    minor <- c((1 - minor) * params$just,
               1 - (1 - rev(minor)) * (1 - params$just))

    if (params$reverse) {
      norm  <- 1 - norm
      major <- 1 - major
    }

    if (params$direction == "horizontal") {
      grad <- linearGradient(
        colours = decor$colour, stops = norm,
        x1 = norm_lim[1], x2 = norm_lim[2], y1 = 0.5, y2 = 0.5
      )
      x <- major
      y <- minor
    } else {
      grad <- linearGradient(
        colours = decor$colour, stops = norm,
        x1 = 0.5, x2 = 0.5, y1 = norm_lim[1], y2 = norm_lim[2]
      )
      x <- minor
      y <- major
    }

    poly <- polygonGrob(
      x = x, y = y,
      gp = gpar(fill = grad, col = NA)
    )

    frame <- NULL
    if (!is_blank(elements$frame)) {
      frame <- polygonGrob(
        x = x, y = y,
        gp = gpar(
          fill =  elements$frame$fill      %||% NA,
          col  =  elements$frame$colour    %||% "black",
          lwd  = (elements$frame$linewidth %||% 0.5) * .pt,
          lty  =  elements$frame$linetype  %||% 1
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

# This is a helper function to set `hist(..., plot = FALSE)`
suppress_hist_plot <- function(hist) {
  if (!quo_is_call(hist)) {
    return(eval_tidy(hist))
  }
  expr <- quo_get_expr(hist)
  if (expr[[1]] != quote(hist)) {
    return(eval_tidy(hist))
  }
  expr <- call_match(expr, graphics::hist)
  expr <- call_modify(expr, plot = FALSE)
  hist <- quo_set_expr(hist, expr)
  eval_tidy(hist)
}
