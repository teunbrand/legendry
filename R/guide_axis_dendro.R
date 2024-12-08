# Constructor -------------------------------------------------------------

#' Dendrogram guide
#'
#' This axis is a speciality axis for discrete data that has been
#' hierarchically clustered. Please be aware that the guide cannot affect the
#' scale limits, which should be set appropriately. This guide will give
#' misleading results when this step is skipped!
#'
#' @inheritParams primitive_segments
#' @inheritParams primitive_labels
#' @inheritParams common_parameters
#' @param ticks,axis_line Guides to use as ticks or axis lines. Defaults to
#'   drawing no ticks or axis lines. Can be specified as one of the following:
#'   * A `<Guide>` class object.
#'   * A `<function>` that returns a `<Guide>` class object.
#'   * A `<character[1]>` naming such a function, without the `guide_` or
#'     `primitive_` prefix.
#' @return A `<Guide>` object.
#' @export
#' @family standalone guides
#'
#' @examples
#' # Hierarchically cluster data
#' clust <- hclust(dist(scale(mtcars)), "ave")
#'
#' # Using the guide along with appropriate limits
#' p <- ggplot(mtcars, aes(disp, rownames(mtcars))) +
#'   geom_col() +
#'   scale_y_discrete(limits = clust$labels[clust$order])
#'
#' # Standard usage
#' p + guides(y = guide_axis_dendro(clust))
#'
#' # Adding ticks and axis line
#' p + guides(y = guide_axis_dendro(clust, ticks = "ticks", axis_line = "line")) +
#'   theme(axis.line = element_line())
#'
#' # Controlling space allocated to dendrogram
#' p + guides(y = guide_axis_dendro(clust, space = unit(4, "cm"))) +
#'   theme(axis.ticks.y.left = element_line("red"))
#'
#' # If want just the dendrograme, use `primitive_segments()`
#' p + guides(y = primitive_segments(clust), y.sec = "axis")
guide_axis_dendro <- function(
  key = "dendro", title = waiver(), theme = NULL,
  space = rel(10), vanish = TRUE,
  n.dodge = 1, angle = waiver(), check.overlap = FALSE,
  ticks = "none", axis_line = "none",
  order = 0, position = waiver()
) {

  theme <- replace_null(
    theme %||% theme(),
    legendry.guide.spacing = unit(0, "cm")
  )

  labels <- primitive_labels(
    angle = angle,
    n.dodge = n.dodge,
    check.overlap = check.overlap
  )

  dendro <- primitive_segments(
    key = key,
    space = space,
    vanish = vanish
  )

  compose_stack(
    axis_line, ticks, labels, dendro,
    drop = c(3L, 4L),
    title = title, theme = theme, order = order,
    available_aes = c("any", "x", "y", "r", "theta"),
    position = position
  )
}
