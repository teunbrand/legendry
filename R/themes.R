
#' Theme wrapper for guides
#'
#' This function has shorthand names for theme elements relating to guides. It
#' is intended to be used as the `guide_*(theme)` argument. Because of this
#' intent, and due to legends and axes having mutually exclusive theme elements,
#' this function sets the elements for both simultaneously.
#'
#' @param text An [`<element_text>`][ggplot2::element_text] setting both
#'   `legend.text` and `axis.text` elements.
#' @param line An [`<element_line>`][ggplot2::element_line] setting both
#'   `legend.axis.line` and `axis.line` elements.
#' @param title An [`<element_text>`][ggplot2::element_text] setting both
#'   `legend.title` and `axis.title` elements.
#' @param subtitle An [`<element_text>`][ggplot2::element_text] setting both
#'   `legendry.legend.subtitle` and `legendry.axis.subtitle` elements.
#' @param text.position,title.position,subtitle.position One of `"top"`,
#'   `"right"`, `"bottom"` or `"right"` setting the following elements:
#'
#'   * `text.position`: sets only `legend.text.position`.
#'   * `title.position`: sets only `legend.title.position`.
#'   * `subtitle.position` sets both `legendry.legend.subtitle.position` and
#'   `legendry.axis.subtitle.position`
#' @param ticks An [`<element_line>`][ggplot2::element_line] setting both
#'   `axis.ticks` and `legend.ticks` elements.
#' @param minor.ticks An [`<element_line>`][ggplot2::element_line] setting
#'   `legendry.legend.minor.ticks` and all 6 of the
#'   `axis.ticks.minor.{r/theta/x.top/x.bottom/y.left/y.right}` elements.
#' @param mini.ticks An [`<element_line>`][ggplot2::element_line] setting both
#'   `legendry.legend.mini.ticks` and `legendry.axis.mini.ticks` elements.
#' @param ticks.length,minor.ticks.length,mini.ticks.length A
#'   [`<unit[1]>`][grid::unit()] setting the following elements:
#'
#'   * `ticks.length`: sets both `legend.ticks.length` and `axis.ticks.length`.
#'   * `minor.ticks.length` sets both `axis.minor.ticks.length` and
#'   `legendry.legend.minor.ticks.length`.
#'   * `mini.ticks.length` sets both `legendry.axis.mini.ticks.length` and
#'   `legendry.legend.mini.ticks.length`.
#' @param spacing A [`<unit[1]>`][grid::unit()] setting the
#'   `legendry.guide.spacing` theme element.
#' @param key An [`<element_rect>`][ggplot2::element_rect] setting the
#'   `legend.key` element.
#' @param key.size,key.width,key.height A [`<unit>`][grid::unit()] setting the
#'   `legend.key.size`, `legend.key.width` and `legend.key.height` elements
#'   respectively.
#' @param key.spacing,key.spacing.x,key.spacing.y A [`<unit[1]>`][grid::unit()]
#'   setting the `legend.key.spacing`, `legend.key.spacing.x` and
#'   `legend.key.spacing.y` elements respectively.
#' @param frame An [`<element_rect>`][ggplot2::element_rect] setting the
#'   `legend.frame` element.
#' @param byrow A `<logical[1]>` setting the `legend.byrow` element.
#' @param background An [`<element_rect>`][ggplot2::element_rect] setting the
#'   `legend.background` element.
#' @param margin A [`<margin>`][ggplot2::margin] setting the `legend.margin`
#'   element.
#' @param bracket An [`<element_line>`][ggplot2::element_line] setting the
#'   `legendry.bracket` element.
#' @param bracket.size A [`<unit[1]>`][grid::unit()] setting the
#'   `legendry.bracket.size` element.
#' @param box An [`<element_rect>`][ggplot2::element_rect] setting the
#'   `legendry.box` element.
#' @param fence,fence.post,fence.rail An
#'   [`<element_line>`][ggplot2::element_line] setting the `legendry.fence`,
#'   `legendry.fence.post` and `legendry.fence.rail` respectively.
#'
#' @return A `<theme>` object that can be provided to a guide.
#' @export
#'
#' @examples
#' red_ticks <- theme_guide(ticks = element_line(colour = "red", linewidth = 0.5))
#'
#' # Both axis and colourbar gain red ticks
#' ggplot(mpg, aes(displ, hwy, colour = cty)) +
#'   geom_point() +
#'   guides(
#'     colour = guide_colourbar(theme = red_ticks),
#'     x = guide_axis(theme = red_ticks)
#'   )
theme_guide <- function(
    text = NULL,
    line = NULL,

    title = NULL,
    subtitle = NULL,

    text.position = NULL,
    title.position = NULL,
    subtitle.position = NULL,

    ticks = NULL,
    minor.ticks = NULL,
    mini.ticks = NULL,

    ticks.length = NULL,
    minor.ticks.length = NULL,
    mini.ticks.length = NULL,

    spacing = NULL,

    key = NULL,
    key.size = NULL,
    key.width = NULL,
    key.height = NULL,
    key.spacing = NULL,
    key.spacing.x = NULL,
    key.spacing.y = NULL,

    frame = NULL,
    byrow = NULL,
    background = NULL,
    margin = NULL,

    bracket = NULL,
    bracket.size = NULL,
    box = NULL,
    fence = NULL,
    fence.post = NULL,
    fence.rail = NULL
) {

  theme <- list(
    legend.text = text,
    axis.text = text,

    axis.line = line,
    legend.axis.line = line,

    legend.title = title,
    axis.title = title,

    legendry.legend.subtitle = subtitle,
    legendry.axis.subtitle = subtitle,

    legend.title.position = title.position,
    legend.text.position = text.position,

    legendry.legend.subtitle.position = subtitle.position,
    legendry.axis.subtitle.position = subtitle.position,

    legend.ticks = ticks,
    axis.ticks = ticks,

    legend.ticks.length = ticks.length,
    axis.ticks.length = ticks.length,

    legendry.legend.minor.ticks = minor.ticks,
    axis.minor.ticks.r = minor.ticks,
    axis.minor.ticks.theta = minor.ticks,
    axis.minor.ticks.x.top = minor.ticks,
    axis.minor.ticks.x.bottom = minor.ticks,
    axis.minor.ticks.y.left = minor.ticks,
    axis.minor.ticks.y.right = minor.ticks,

    legendry.legend.minor.ticks.lenth = minor.ticks.length,
    axis.minor.ticks.length = minor.ticks.length,

    legendry.axis.mini.ticks = mini.ticks,
    legendry.legend.mini.ticks = mini.ticks,

    legendry.axis.mini.ticks.length = mini.ticks.length,
    legendry.legend.mini.ticks.length = mini.ticks.length,

    legendry.guide.spacing = spacing,

    legend.key = key,
    legend.key.spacing = key.spacing,
    legend.key.spacing.x = key.spacing.x,
    legend.key.spacing.y = key.spacing.y,

    legend.frame = frame,
    legend.byrow = byrow,
    legend.background = background,
    legend.margin = margin,

    legendry.bracket = bracket,
    legendry.bracket.size = bracket.size,
    legendry.box = box,
    legendry.fence = fence,
    legendry.fence.post = fence.post,
    legendry.fence.rail = fence.rail
  )
  theme <- theme[!is_each(theme, is.null)]
  theme(!!!theme)
}


register_legendry_elements <- function() {
  register_theme_elements(
    legendry.bracket.size = unit(2, "mm"),
    legendry.bracket = element_line(),
    legendry.fence = element_line(),
    legendry.fence.post = element_line(),
    legendry.fence.rail = element_line(),
    legendry.box = element_rect(colour = "white"),
    legendry.legend.minor.ticks = element_line(),
    legendry.legend.minor.ticks.length = rel(0.75),
    legendry.legend.mini.ticks = element_line(),
    legendry.legend.mini.ticks.length = rel(0.5),
    legendry.legend.subtitle = element_text(size = rel(0.9)),
    legendry.legend.subtitle.position = "top",
    legendry.axis.mini.ticks = element_line(),
    legendry.axis.mini.ticks.length = rel(0.5),
    legendry.guide.spacing = unit(2.25, "pt"),
    legendry.axis.subtitle = element_text(margin = margin(5.5, 5.5, 5.5, 5.5)),
    legendry.axis.subtitle.position = c("left", "top"),
    element_tree = list(
      legendry.bracket.size = el_def("unit"),
      legendry.bracket = el_line("line"),
      legendry.fence = el_line("line"),
      legendry.fence.post = el_line("legendry.fence"),
      legendry.fence.rail = el_line("legendry.fence"),
      legendry.box = el_def("element_rect", "strip.background"),
      legendry.legend.minor.ticks = el_line("legend.ticks"),
      legendry.legend.minor.ticks.length = el_unit("legend.ticks.length"),
      legendry.legend.mini.ticks = el_line("legendry.legend.minor.ticks"),
      legendry.legend.mini.ticks.length = el_unit("legendry.legend.minor.ticks.length"),
      legendry.legend.subtitle = el_def("element_text", "legend.title"),
      legendry.legend.subtitle.position = el_def("character"),
      legendry.axis.mini.ticks = el_line("axis.ticks"),
      legendry.axis.mini.ticks.length = el_unit("axis.minor.ticks.length"),
      legendry.guide.spacing = el_unit("axis.ticks.length"),
      legendry.axis.subtitle = el_def("element_text", "axis.text"),
      legendry.axis.subtitle.position = el_def("character")
    )
  )
}

on_load(register_legendry_elements())

el_unit <- function(...) el_def(c("unit", "rel"), ...)
el_line <- function(...) el_def("element_line",   ...)
