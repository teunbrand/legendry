
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
