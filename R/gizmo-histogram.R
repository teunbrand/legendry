# Constructor -------------------------------------------------------------

#' Guide gizmo: histogram
#'
#' This guide displays a histogram of the aesthetic. If the aesthetic is
#' `colour` or `fill`, the shape will reflect this.
#'
#' @param hist One of the following:
#'   * `NULL` for computing histograms on the data values (default).
#'   * an atomic `<vector>` to feed to the `hist.fun` function.
#'   * A named `<list>` with `breaks` and `counts` numeric items, where the
#'   `breaks` item is exactly one element longer than the `counts` item. A
#'   typical way to construct such list is using the [`hist()`][graphics::hist]
#'   function. Please note that `<list>` input is expected in scale-transformed
#'   space, not original data space.
#' @param hist.args A `<list>` with additional arguments to the `hist.fun`
#'   argument. Only applies when `hist` is not provided as a `<list>` already.
#'   Please note that these arguments are only used for binning and counting:
#'   graphical arguments are ignored.
#' @param hist.fun A `<function>` to use for computing histograms when the
#'   `hist` argument is not provided as a list already.
#' @param just A `<numeric[1]>` between 0 and 1. Use 0 for bottom- or
#'   left-aligned histograms, use 1 for top- or right-aligned histograms and 0.5
#'   for centred histograms.
#' @inheritParams gizmo_density
#'
#' @details
#' Non-finite values such as `NA` and `NaN` are ignored while infinite values
#' such as `-Inf` and `Inf` are [squished][scales::oob_squish] to the limits.
#'
#' @return A `<GizmoHistogram>` object.
#' @family gizmos
#' @export
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy, colour = cty)) +
#'   geom_point() +
#'   scale_colour_viridis_c()
#'
#' # Histogram from plot data
#' p + guides(colour = gizmo_histogram())
#'
#' # Using bins instead of gradient
#' p + guides(colour = gizmo_histogram("bins"))
#'
#' # Providing custom values to compute histogram
#' p + guides(colour = gizmo_histogram(hist = runif(1000, min = 5, max = 35)))
#'
#' # Providing precomputed histogram
#' p + guides(colour = gizmo_histogram(hist = hist(mpg$cty, breaks = 10)))
#'
#' # Alternatively, parameters may be passed through hist.args
#' p + guides(colour = gizmo_histogram(hist.arg = list(breaks = 10)))
gizmo_histogram <- function(
  key = "sequence",
  hist = NULL, hist.args = list(), hist.fun = graphics::hist,
  just = 1, oob = oob_keep, alpha = NA,
  # standard arguments
  theme = NULL, position = waiver(), direction = NULL
) {
  hist <- suppress_hist_plot(enquo(hist))
  hist.args$plot <- hist.args$plot %||% FALSE

  check_number_decimal(just, min = 0, max = 1, allow_infinite = FALSE)

  new_guide(
    key = key,
    hist      = hist,
    hist_args = hist.args,
    hist_fun  = hist.fun,
    just = just, oob  = oob, alpha = alpha,
    theme = theme, position = position, direction = direction,
    name = "histogram",
    super = GizmoHistogram
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
GizmoHistogram <- ggproto(
  "GizmoHistogram", GizmoDensity,

  params = new_params(
    hist = NULL, hist_args = list(), hist_fun = graphics::hist,
    just = 0.5, nbin = 15, oob = oob_keep, alpha = NA, key = "sequence"
  ),

  extract_decor = function(scale, hist, hist_args, hist_fun, ...) {
    if (is.null(hist)) {
      return(NULL) # extract data later
    }
    if (is.atomic(hist)) {
      hist <- filter_finite(scale$transform(hist))
      hist <- inject(hist_fun(hist, !!!hist_args))
    }
    check_histogram(hist)
    hist
  },

  get_layer_key = function(params, layers, data = NULL, ...) {
    hist <- params$decor %||% params$hist
    if (length(hist) == 0) {
      values <- filter_finite(vec_c(!!!lapply(data, .subset2, params$aesthetic)))
      hist   <- inject(params$hist_fun(values, !!!params$hist_args))
      check_histogram(hist)
    }
    params$decor  <- normalise_histogram(hist)
    params$limits <- range(params$limits, params$decor$x)
    params
  }
)

# Helpers -----------------------------------------------------------------

normalise_histogram <- function(hist) {
  x <- hist$breaks
  y <- hist$counts

  xlim <- range(filter_finite(x), na.rm = TRUE)
  x    <- oob_squish_infinite(x, xlim)

  ylim <- range(filter_finite(y), na.rm = TRUE)
  y <- oob_squish_infinite(y, ylim)

  list(
    x = rep(x, each = 2),
    y = rescale_max(c(0, rep(y, each = 2), 0), to = c(0, 0.9), from = ylim)
  )
}

check_histogram <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is_missing(x)) {
    cli::cli_abort("{.arg {arg}} cannot be missing.", call = call)
  }
  if (inherits(x, "histogram")) {
    # We'll trust this class
    return(x)
  }
  check_list_names(x, c("breaks", "counts"), arg = arg, call = call)

  if (length(x$breaks) != (length(x$counts) + 1L)) {
    cli::cli_abort(c(paste0(
      "In the {.arg {arg}} argument, the {.field breaks} element should be ",
      "exactly 1 longer than the {.field counts} element."
    ),
    i = "{.code {arg}$breaks} has length {length(x$breaks)}.",
    i = "{.code {arg}$counts} has length {length(x$counts)}."
    ), call = call)
  }
  check_length(x$breaks, min = 2, arg = as_cli("{arg}$breaks"), call = call)
  check_bare_numeric(x$breaks,    arg = as_cli("{arg}$breaks"), call = call)
  check_bare_numeric(x$counts,    arg = as_cli("{arg}$counts"), call = call)
  invisible()
}

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
