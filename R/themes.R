
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
#'   `gguidance.legend.subtitle` and `gguidance.axis.subtitle` elements.
#' @param text.position,title.position,subtitle.position One of `"top"`,
#'   `"right"`, `"bottom"` or `"right"` setting the following elements:
#'
#'   * `text.position`: sets only `legend.text.position`.
#'   * `title.position`: sets only `legend.title.position`.
#'   * `subtitle.position` sets both `gguidance.legend.subtitle.position` and
#'   `gguidance.axis.subtitle.position`
#' @param ticks An [`<element_line>`][ggplot2::element_line] setting both
#'   `axis.ticks` and `legend.ticks` elements.
#' @param minor.ticks An [`<element_line>`][ggplot2::element_line] setting
#'   `gguidance.legend.minor.ticks` and all 6 of the
#'   `axis.ticks.minor.{r/theta/x.top/x.bottom/y.left/y.right}` elements.
#' @param mini.ticks An [`<element_line>`][ggplot2::element_line] setting both
#'   `gguidance.legend.mini.ticks` and `gguidance.axis.mini.ticks` elements.
#' @param ticks.length,minor.ticks.length,mini.ticks.length A
#'   [`<unit[1]>`][grid::unit()] setting the following elements:
#'
#'   * `ticks.length`: sets both `legend.ticks.length` and `axis.ticks.length`.
#'   * `minor.ticks.length` sets both `axis.minor.ticks.length` and
#'   `gguidance.legend.minor.ticks.length`.
#'   * `mini.ticks.length` sets both `gguidance.axis.mini.ticks.length` and
#'   `gguidance.legend.mini.ticks.length`.
#' @param spacing A [`<unit[1]>`][grid::unit()] setting the
#'   `gguidance.guide.spacing` theme element.
#' @param group.spacing A [`<unit[1]>`][grid::unit()] setting the
#'   `gguidance.legend.group.spacing` element.
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
#'   `gguidance.bracket` element.
#' @param bracket.size A [`<unit[1]>`][grid::unit()] setting the
#'   `gguidance.bracket.size` element.
#' @param box An [`<element_rect>`][ggplot2::element_rect] setting the
#'   `gguidance.box` element.
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
    group.spacing = NULL,

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
    box = NULL
) {

  theme <- list(
    legend.text = text,
    axis.text = text,

    axis.line = line,
    legend.axis.line = line,

    legend.title = title,
    axis.title = title,

    gguidance.legend.subtitle = subtitle,
    gguidance.axis.subtitle = subtitle,

    legend.title.position = title.position,
    legend.text.position = text.position,

    gguidance.legend.subtitle.position = subtitle.position,
    gguidance.axis.subtitle.position = subtitle.position,

    legend.ticks = ticks,
    axis.ticks = ticks,

    legend.ticks.length = ticks.length,
    axis.ticks.length = ticks.length,

    gguidance.legend.minor.ticks = minor.ticks,
    axis.minor.ticks.r = minor.ticks,
    axis.minor.ticks.theta = minor.ticks,
    axis.minor.ticks.x.top = minor.ticks,
    axis.minor.ticks.x.bottom = minor.ticks,
    axis.minor.ticks.y.left = minor.ticks,
    axis.minor.ticks.y.right = minor.ticks,

    gguidance.legend.minor.ticks.lenth = minor.ticks.length,
    axis.minor.ticks.length = minor.ticks.length,

    gguidance.axis.mini.ticks = mini.ticks,
    gguidance.legend.mini.ticks = mini.ticks,

    gguidance.axis.mini.ticks.length = mini.ticks.length,
    gguidance.legend.mini.ticks.length = mini.ticks.length,

    gguidance.guide.spacing = spacing,
    gguidance.legend.group.spacing = group.spacing,

    legend.key = key,
    legend.key.spacing = key.spacing,
    legend.key.spacing.x = key.spacing.x,
    legend.key.spacing.y = key.spacing.y,

    legend.frame = frame,
    legend.byrow = byrow,
    legend.background = background,
    legend.margin = margin,

    gguidance.bracket = bracket,
    gguidance.bracket.size = bracket.size,
    gguidance.box = box
  )
  theme <- theme[lengths(theme) > 0]
  theme(!!!theme)
}


register_gguidance_elements <- function() {
  register_theme_elements(
    gguidance.bracket.size = unit(2, "mm"),
    gguidance.bracket = element_line(),
    gguidance.box = element_rect(colour = "white"),
    gguidance.legend.minor.ticks = element_line(),
    gguidance.legend.minor.ticks.length = rel(0.75),
    gguidance.legend.mini.ticks = element_line(),
    gguidance.legend.mini.ticks.length = rel(0.5),
    gguidance.legend.subtitle = element_text(size = rel(0.9)),
    gguidance.legend.subtitle.position = "top",
    gguidance.legend.group.spacing = rel(2),
    gguidance.axis.mini.ticks = element_line(),
    gguidance.axis.mini.ticks.length = rel(0.5),
    gguidance.guide.spacing = unit(2.25, "pt"),
    gguidance.axis.subtitle = element_text(margin = margin(5.5, 5.5, 5.5, 5.5)),
    gguidance.axis.subtitle.position = c("left", "top"),
    element_tree = list(
      gguidance.bracket.size = el_def("unit"),
      gguidance.bracket = el_line("line"),
      gguidance.box = el_def("element_rect", "strip.background"),
      gguidance.legend.minor.ticks = el_line("legend.ticks"),
      gguidance.legend.minor.ticks.length = el_unit("legend.ticks.length"),
      gguidance.legend.mini.ticks = el_line("gguidance.legend.minor.ticks"),
      gguidance.legend.mini.ticks.length = el_unit("gguidance.legend.minor.ticks.length"),
      gguidance.legend.subtitle = el_def("element_text", "legend.title"),
      gguidance.legend.subtitle.position = el_def("character"),
      gguidance.legend.group.spacing = el_unit("legend.key.spacing"),
      gguidance.axis.mini.ticks = el_line("axis.ticks"),
      gguidance.axis.mini.ticks.length = el_unit("axis.minor.ticks.length"),
      gguidance.guide.spacing = el_unit("axis.ticks.length"),
      gguidance.axis.subtitle = el_def("element_text", "axis.text"),
      gguidance.axis.subtitle.position = el_def("character")
    )
  )
}

on_load(register_gguidance_elements())

el_unit <- function(...) el_def(c("unit", "rel"), ...)
el_line <- function(...) el_def("element_line",   ...)
