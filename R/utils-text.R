
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

add_legend_title <- function(gt, title = NULL, position = "top", element = list(hjust = 0.5, vjust = 0.5)) {
  if (is_blank(title) || is_waive(title)) {
    return(gt)
  }

  width  <- unit(width_cm(title),  "cm")
  height <- unit(height_cm(title), "cm")
  vjust  <- element$vjust

  if (position == "top") {
    gt <- gtable_add_rows(gt, height, pos = 0)
    gt <- gtable_add_grob(gt, title, t = 1, l = 1, r = -1, name = "title", clip = "off")
  } else if (position == "bottom") {
    gt <- gtable_add_rows(gt, height, pos = -1)
    gt <- gtable_add_grob(gt, title, t = -1, l = 1, r = -1, name = "title", clip = "off")
  } else if (position == "left") {
    gt <- gtable_add_cols(gt, width, pos = 0)
    gt <- gtable_add_grob(gt, title, t = 1, b = -1, l = 1, name = "title", clip = "off")
  } else if (position == "right") {
    gt <- gtable_add_cols(gt, width, pos = -1)
    gt <- gtable_add_grob(gt, title, t = 1, b = -1, l = -1, name = "title", clip = "off")
  }

  if (position %in% c("top", "bottom")) {
    if (any(unitType(gt$widths) %in% c("null", "npc"))) {
      return(gt)
    }
    hjust  <- element$hjust
    width <- as.numeric(width)
    table_width <- width_cm(sum(gt$widths))
    extra_width <- unit(max(0, width - table_width) * c(hjust, 1 - hjust), "cm")
    gt <- gtable_add_cols(gt, extra_width[1], pos =  0)
    gt <- gtable_add_cols(gt, extra_width[2], pos = -1)
  } else {
    if (any(unitType(gt$heights) %in% c("null", "npc"))) {
      return(gt)
    }
    vjust <- element$vjust
    table_height <- height_cm(sum(gt$heights))
    height <- as.numeric(height)
    extra_height <- unit(max(0, height - table_height) * c(1 - vjust, vjust), "cm")
    gt <- gtable_add_rows(gt, extra_height[1], pos =  0)
    gt <- gtable_add_rows(gt, extra_height[2], pos = -1)
  }
  gt
}
