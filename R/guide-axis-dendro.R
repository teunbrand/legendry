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
#' @param labels,ticks,axis_line
#' Guides to use as labels, ticks or axis lines. Can be specified as one of
#' the following:
#' * A `<logical[1]>` which when `FALSE` will set the guide to `guide_none()`
#'   and if `TRUE`, will set guide to appropriate primitive.
#' * A `<Guide>` class object.
#' * A `<function>` that returns a `<Guide>` class object.
#' * A `<character[1]>` naming such a function, without the `guide_` or
#'   `primitive_` prefix.
#'
#' @details
#' ## Styling options
#'
#' Because this guide is pure composite guide, the [theme][ggplot2::theme]
#' options that govern the styling are determined by its constituents. They are
#' linked below so you can find their 'Styling options' sections.
#'
#' | **Primitive** | **Description** |
#' | ------------- | --------------- |
#' | [`compose_stack`] | Stacks the lines, tick marks and labels and dendrogram. |
#' | [`primitive_segments()`] | The dendrogram. |
#' | [`primitive_line()`] | Makes up the axis line. |
#' | [`primitive_ticks()`] | Makes up the tick marks. |
#' | [`primitive_labels()`] | Makes up the labels. |
#'
#' The context-agnostic alternative to using `theme()` is to use
#' [`theme_guide()`]:
#'
#' ```r
#' guide_axis_dendro(theme = theme_guide(
#'   line = element_line(),
#'   text = element_text(),
#'   ticks = element_line(),
#'   ticks.length = unit(5, "mm"),
#' ))
#' ```
#'
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
#' p +
#'   guides(y = guide_axis_dendro(clust, ticks = "ticks", axis_line = "line")) +
#'   theme(axis.line = element_line())
#'
#' # Controlling space allocated to dendrogram
#' p + guides(y = guide_axis_dendro(clust, space = unit(4, "cm"))) +
#'   theme(axis.ticks.y.left = element_line("red"))
#'
#' # If want just the dendrogram, use `labels = FALSE`
#' p + guides(y = guide_axis_dendro(clust, labels = FALSE), y.sec = "axis")
guide_axis_dendro <- function(
  key = "dendro", title = waiver(), theme = NULL,
  labels = TRUE, space = rel(10.0), vanish = TRUE,
  n.dodge = 1L, angle = waiver(), check.overlap = FALSE,
  ticks = "none", axis_line = "none",
  order = 0L, position = waiver()
) {

  theme <- replace_null(
    theme %||% theme(),
    legendry.guide.spacing = unit(0.0, "cm")
  )

  if (isTRUE(labels)) {
    labels <- primitive_labels(
      angle = angle,
      n.dodge = n.dodge,
      check.overlap = check.overlap
    )
  } else if (isFALSE(labels)) {
    labels <- "none"
  }
  if (isTRUE(ticks)) {
    ticks <- primitive_ticks()
  } else if (isFALSE(ticks)) {
    ticks <- "none"
  }
  if (isTRUE(axis_line)) {
    axis_line <- primitive_line()
  } else if (isFALSE(axis_line)) {
    axis_line <- "none"
  }

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
