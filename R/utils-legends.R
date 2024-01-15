
legend_assemble <- function(
  widths, heights,
  layout, grobs, elements,
  n_layers = 1
) {

  if (isTRUE(elements$stretch_x)) {
    widths[unique(layout$key_col)] <- elements$key_width
  }

  if (isTRUE(elements$stretch_y)) {
    heights[unique(layout$key_row)] <- elements$key_height
  }

  gt <- gtable(widths = widths, heights = heights)

  if (!is.zero(grobs$decor)) {
    cols <- rep(layout$key_col, each = n_layers)
    rows <- rep(layout$key_row, each = n_layers)
    names <- names(grobs$decor) %||%
      paste("key", rows, cols, c("bg", seq_len(n_layers - 1)), sep = "-")
    gt <- gtable_add_grob(
      gt, grobs$decor,
      name = names, clip = "off",
      t = rows, r = cols, b = rows, l = cols
    )
  }

  if (!is.zero(grobs$labels)) {
    rows <- layout$label_row
    cols <- layout$label_col
    names <- names(grobs$labels) %||% paste("label", rows, cols, sep = "-")
    gt <- gtable_add_grob(
      gt, grobs$labels,
      name = names, clip = "off",
      t = rows, r = cols, b = rows, l = cols
    )
  }
  return(gt)
}

legend_add_background <- function(gt, background) {
  if (inherits(background, "element")) {
    background <- element_grob(background)
  }
  if (is.zero(background) || is.null(background)) {
    return(gt)
  }
  gt <- gtable_add_grob(
    gt, background, name = "background", clip = "off",
    t = 1, r = -1, b = -1, l = 1, z = -Inf
  )
  gt
}

legend_add_title <- function(gt, title = NULL, position = "top", element = list(hjust = 0.5, vjust = 0.5)) {
  if (is_blank(title) || is_waive(title) || is.zero(title)) {
    return(gt)
  }

  title_width  <- unit(width_cm(title),  "cm")
  title_height <- unit(height_cm(title), "cm")

  if (position == "top") {
    gt <- gtable_add_rows(gt, title_height, pos = 0)
    gt <- gtable_add_grob(gt, title, t = 1, l = 1, r = -1,  name = "title", clip = "off")
  } else if (position == "bottom") {
    gt <- gtable_add_rows(gt, title_height, pos = -1)
    gt <- gtable_add_grob(gt, title, t = -1, l = 1, r = -1, name = "title", clip = "off")
  } else if (position == "left") {
    gt <- gtable_add_cols(gt, title_width, pos = 0)
    gt <- gtable_add_grob(gt, title, t = 1, b = -1, l = 1,  name = "title", clip = "off")
  } else if (position == "right") {
    gt <- gtable_add_cols(gt, title_width, pos = -1)
    gt <- gtable_add_grob(gt, title, t = 1, b = -1, l = -1, name = "title", clip = "off")
  }

  if (position %in% c("top", "bottom")) {
    if (any(unitType(gt$widths) %in% c("null", "npc"))) {
      return(gt)
    }
    hjust  <- element$hjust
    title_width <- as.numeric(title_width)
    table_width <- width_cm(sum(gt$widths))
    extra_width <- max(0, title_width - table_width)
    if (extra_width == 0) {
      return(gt)
    }
    extra_width <- unit(extra_width * c(hjust, 1 - hjust), "cm")
    gt <- gtable_add_cols(gt, extra_width[1], pos =  0)
    gt <- gtable_add_cols(gt, extra_width[2], pos = -1)
  } else {
    if (any(unitType(gt$heights) %in% c("null", "npc"))) {
      return(gt)
    }
    vjust <- element$vjust
    title_height <- as.numeric(title_height)
    table_height <- height_cm(sum(gt$heights))
    extra_height <- max(0, title_height - table_height)
    if (extra_height == 0) {
      return(gt)
    }
    extra_height <- unit(extra_height * c(1 - vjust, vjust), "cm")
    gt <- gtable_add_rows(gt, extra_height[1], pos =  0)
    gt <- gtable_add_rows(gt, extra_height[2], pos = -1)
  }
  gt
}
