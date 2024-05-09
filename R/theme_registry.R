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
    gguidance.side.titles = element_text(margin = margin(5.5, 5.5, 5.5, 5.5)),
    gguidance.side.titles.position = c("left", "top"),
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
      gguidance.side.titles = el_def("element_text", "axis.text"),
      gguidance.side.titles.position = el_def("character")
    )
  )
}

on_load(register_gguidance_elements())

el_unit <- function(...) el_def(c("unit", "rel"), ...)
el_line <- function(...) el_def("element_line",   ...)
