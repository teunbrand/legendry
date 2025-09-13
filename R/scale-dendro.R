# Constructors ------------------------------------------------------------

#' Dendrogram scales
#'
#' These are speciality scales for use with hierarchically clustered data. The
#' scale automatically orders the limits according to the clustering result
#' and comes with a [dendrogram axis][guide_axis_dendro()].
#'
#' @param clust A data structure that can be coerced to an
#'   [`<hclust>`][stats::hclust] object through
#'   [`as.hclust()`][stats::as.hclust].
#' @inheritDotParams ggplot2::discrete_scale -scale_name -limits
#' @inheritParams ggplot2::scale_x_discrete
#'
#' @details
#' The scale limits are determined by the order and labels in the `clust`
#' argument. While `limits` is not an argument in these scales, the `breaks`
#' argument can still be used to selectively omit some breaks and the `labels`
#' can be used for formatting purposes.
#'
#' @return A `<ScaleDiscretePosition>` object that can be added to a plot.
#' @seealso [guide_axis_dendro()]
#' @export
#'
#' @examples
#' # Hierarchically cluster data, separately for rows and columns
#' car_clust <- hclust(dist(scale(mtcars)), "ave")
#' var_clust <- hclust(dist(scale(t(mtcars))), "ave")
#'
#' long_mtcars <- data.frame(
#'   car = rownames(mtcars)[row(mtcars)],
#'   var = colnames(mtcars)[col(mtcars)],
#'   value = as.vector(scale(mtcars))
#' )
#'
#' # A standard heatmap adorned with dendrograms
#' p <- ggplot(long_mtcars, aes(var, car, fill = value)) +
#'   geom_tile() +
#'   scale_x_dendro(var_clust) +
#'   scale_y_dendro(car_clust)
#' p
#'
#' # Styling the dendrograms
#' p +
#'   guides(
#'     y = guide_axis_dendro(key_dendro(type = "triangle")),
#'     x = guide_axis_dendro(space = rel(5))
#'   ) +
#'   theme(
#'     axis.text.y.left = element_text(margin = margin(r = 3, l = 3)),
#'     axis.ticks.y = element_line("red"),
#'     axis.ticks.x = element_line(linetype = "dotted")
#'   )
#'
#' # In polar coordinates, plus some formatting
#' p +
#'   coord_radial(
#'     theta = "y", inner.radius = 0.5,
#'     start = 0.25 * pi, end = 1.75 * pi
#'   ) +
#'   guides(
#'     theta = primitive_labels(angle = 90),
#'     theta.sec = primitive_segments("dendro", vanish = TRUE),
#'     r = guide_axis_dendro(angle = 0)
#'   )
scale_x_dendro <- function(clust, ..., expand = waiver(), guide = "axis_dendro",
                           position = "bottom") {

  clust  <- validate_clust(clust)
  limits <- validate_clust_limits(clust)

  args <- list2(...)
  check_dendro_args(args)
  # Pre-validating guide here in case legendry is not loaded (#94)
  guide <- validate_guide(guide)

  sc <- inject(discrete_scale(
    aesthetics = c(
      "x", "xmin", "xmax", "xend", "xintercept",
      "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0"
    ),
    palette = seq_len,
    scale_name = missing_arg(),
    limits = limits,
    !!!args,
    guide = guide,
    expand = expand,
    position = position,
    super = ScaleDiscretePosition
  ))
  sc$range_c <- ContinuousRange$new()
  sc$clust <- clust
  sc
}

#' @rdname scale_x_dendro
#' @export
scale_y_dendro <- function(clust, ..., expand = waiver(), guide = "axis_dendro",
                           position = "left") {
  clust  <- validate_clust(clust)
  limits <- validate_clust_limits(clust)

  args <- list2(...)
  check_dendro_args(args)
  # Pre-validating guide here in case legendry is not loaded (#94)
  guide <- validate_guide(guide)

  sc <- inject(discrete_scale(
    aesthetics = c(
      "y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final",
      "lower", "middle", "upper", "y0"
    ),
    palette = seq_len,
    scale_name = missing_arg(),
    limits = limits,
    !!!args,
    guide = guide,
    expand = expand,
    position = position,
    super = ScaleDiscretePosition
  ))
  sc$range_c <- ContinuousRange$new()
  sc$clust <- clust
  sc
}


# Helpers -----------------------------------------------------------------

validate_clust <- function(clust, .call = caller_env()) {
  try_fetch(
    stats::as.hclust(clust),
    error = function(cnd) {
      cli::cli_abort(
        "The {.arg clust} argument should be convertable to an {.cls hclust}
        object.", parent = cnd, call = .call
      )
    }
  )
}

validate_clust_limits <- function(clust, .call = caller_env()) {
  labels <- clust$labels %||% seq_along(clust$order)
  labels <- labels[clust$order]
  function(x) {
    union(labels, x)
  }
}

check_dendro_args <- function(args, .call = caller_env()) {
  if ("limits" %in% names(args)) {
    cli::cli_abort(
      "Function does not accept {.arg limits} argument: it is derived from the
      labels and order in the {.arg clust} argument.",
      call = .call
    )
  }
  if ("palette" %in% names(args)) {
    cli::cli_abort(
      "Function does not accept {.arg palette} argument: the scale requires
      fixed spacing between items.",
      call = .call
    )
  }
  invisible()
}
