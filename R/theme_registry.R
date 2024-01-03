
el_unit <- function(...) el_def(c("unit", "rel"), ...)
el_line <- function(...) el_def("element_line", ...)

on_load({
  register_theme_elements(
    gguidance.bracket.size = unit(2, "mm"),
    gguidance.bracket = element_line(),
    gguidance.box = element_rect(colour = "white"),
    gguidance.legend.minor.ticks = element_line(),
    gguidance.legend.minor.ticks.length = rel(0.75),
    gguidance.legend.mini.ticks = element_line(),
    gguidance.legend.mini.ticks.length = rel(0.5),
    gguidance.axis.mini.ticks = element_line(),
    gguidance.axis.mini.ticks.length = rel(0.5),
    gguidance.guide.spacing = unit(2.25, "pt"),
    element_tree = list(
      gguidance.bracket.size = el_def("unit"),
      gguidance.bracket = el_line("line"),
      gguidance.box = el_def("element_rect", "strip.background"),
      gguidance.legend.minor.ticks = el_line("legend.ticks"),
      gguidance.legend.minor.ticks.length = el_unit("legend.ticks.length"),
      gguidance.legend.mini.ticks = el_line("gguidance.legend.minor.ticks"),
      gguidance.legend.mini.ticks.length = el_unit("gguidance.legend.minor.ticks.length"),
      gguidance.axis.mini.ticks = el_line("axis.ticks"),
      gguidance.axis.mini.ticks.length = el_unit("axis.minor.ticks.length"),
      gguidance.guide.spacing = el_unit("axis.ticks.length"),
    )
  )
})
