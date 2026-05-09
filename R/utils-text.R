
setup_legend_text <- function(theme, position = NULL, direction = "vertical") {
  position <- position %||%
    calc_element("legend.text.position", theme) %||%
    switch(direction, horizontal = "bottom", vertical = "right")
  gap    <- calc_element("legend.key.spacing", theme) %||% unit(0.0, "pt")
  margin <- calc_element("text", theme)$margin %||% margin()
  margin <- position_margin(position, margin, gap)
  text <- theme(
    text = switch(
      position,
      top    = element_text(hjust = 0.5, vjust = 0.0, margin = margin),
      bottom = element_text(hjust = 0.5, vjust = 1.0, margin = margin),
      left   = element_text(hjust = 1.0, vjust = 0.5, margin = margin),
      right  = element_text(hjust = 0.0, vjust = 0.5, margin = margin),
      element_text(hjust = 0.5, vjust = 0.5, margin = margin)
    )
  )
  calc_element("legend.text", theme + text)
}

setup_side_title <- function(theme, position = NULL, type = "axis") {
  name <- switch(
    type,
    axis = "legendry.axis.subtitle",
    "legendry.legend.subtitle"
  )
  position <- position %||%
    calc_element(paste0(name, ".position"), theme) %||%
    "left"
  gap <- switch(
    type,
    axis = {
      position_name <- suffix_position("axis.ticks.length", position)[[1L]]
      (calc_element(position_name, theme) %||% unit(0.0, "pt")) * 2.0
    },
    calc_element("legend.key.spacing", theme) %||% unit(0.0, "pt")
  )
  margin <- calc_element("text", theme)$margin %||% margin()
  margin <- position_margin(position, margin, gap)
  text <- theme(
    text = switch(
      position,
      top    = element_text(hjust = 0.5, vjust = 0.0, margin = margin),
      bottom = element_text(hjust = 0.5, vjust = 1.0, margin = margin),
      left   = element_text(hjust = 1.0, vjust = 0.5, margin = margin),
      right  = element_text(hjust = 0.0, vjust = 0.5, margin = margin),
      element_text(hjust = 0.5, vjust = 0.5, margin = margin)
    )
  )
  calc_element(name, theme + text)
}

setup_legend_title <- function(theme, position = NULL, direction = "vertical",
                               element = "legend.title") {
  position <- position %||%
    calc_element("legend.title.position", theme) %||%
    switch(direction, horizontal = "left", vertical = "top")
  gap <- calc_element("legend.key.spacing", theme) %||% unit(0.0, "pt")
  margin <- calc_element("text", theme)$margin %||% margin()
  margin <- position_margin(position, margin, gap)
  title <- theme(text = element_text(hjust = 0.0, vjust = 0.5, margin = margin))
  calc_element(element, theme + title)
}

position_margin <- function(
  position, margin = margin(), gap = unit(0.0, "pt")
) {
  switch(
    position,
    top    = replace(margin, 3L, margin[3L] + gap),
    bottom = replace(margin, 1L, margin[1L] + gap),
    left   = replace(margin, 2L, margin[2L] + gap),
    right  = replace(margin, 4L, margin[4L] + gap),
    margin + gap
  )
}
