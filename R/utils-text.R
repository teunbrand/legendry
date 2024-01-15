
setup_legend_text <- function(theme, direction = "vertical") {
  position <- calc_element("legend.text.position", theme)
  position <- position %||% switch(direction, horizontal = "bottom", vertical = "right")
  gap    <- calc_element("legend.key.spacing", theme)
  margin <- calc_element("text", theme)$margin
  margin <- position_margin(position, margin, gap)
  text <- theme(
    text = switch(
      position,
      top    = element_text(hjust = 0.5, vjust = 0.0, margin = margin),
      bottom = element_text(hjust = 0.5, vjust = 1.0, margin = margin),
      left   = element_text(hjust = 1.0, vjust = 0.5, margin = margin),
      right  = element_text(hjust = 0.0, vjust = 0.5, margin = margin)
    )
  )
  calc_element("legend.text", theme + text)
}

setup_legend_title <- function(theme, direction = "vertical") {
  position <- calc_element("legend.title.position", theme)
  position <- position %||% switch(direction, horizontal = "left", vertical = "top")
  gap <- calc_element("legend.key.spacing", theme)
  margin <- calc_element("text", theme)$margin
  margin <- position_margin(position, margin, gap)
  title <- theme(text = element_text(hjust = 0, vjust = 0.5, margin = margin))
  calc_element("legend.title", theme + title)
}


