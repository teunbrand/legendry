# Constructor -------------------------------------------------------------

#' Cross legend guide
#'
#' This is a legend type similar to [`guide_legend()`][ggplot2::guide_legend()]
#' that displays crosses, or: interactions, between two variables.
#'
#' @param key One of the following key specifications:
#'   * A [group split][key_group_split] specification when using the legend to
#'     display a compound variable like `paste(var1, var2)`.
#'   * A [standard key][key_standard] specification, like [`key_auto()`], when
#'     crossing two separate variables across two scales.
#' @param swap A `<logical[1]>` which when `TRUE` exchanges the column and row
#'   variables in the displayed legend.
#' @param col_text An `<element_text>` object giving adjustments to text for
#'   the column labels. Can be `NULL` to display column labels in equal fashion
#'   to the row labels.
#' @param reverse A `<logical[2]>` whether the order of the keys should be
#'   inverted, where the first value controls the row order and second value
#'   the column order. Input as `<logical[1]>` will be recycled.
#' @inheritParams common_parameters
#'
#' @return A `<GuideLegend>` object.
#' @export
#' @family standalone guides
#' @family legend guides
#'
#' @examples
#' # Standard use for single aesthetic. The default is to split labels to
#' # disentangle aesthetics that are already crossed (by e.g. `paste()`)
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = paste(year, drv))) +
#'   guides(colour = "legend_cross")
#'
#' # If legends should be merged between identical aesthetics, both need the
#' # same legend type.
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = paste(year, drv), shape = paste(year, drv))) +
#'   guides(colour = "legend_cross", shape = "legend_cross")
#'
#' # Crossing two aesthetics requires a shared title and `key = "auto"`. The
#' # easy way to achieve this is to predefine a shared guide.
#' my_guide <- guide_legend_cross(key = "auto", title = "My title")
#'
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = drv, shape = factor(year))) +
#'   guides(colour = my_guide, shape  = my_guide)
#'
#' # You can cross more than 2 aesthetics but not more than 2 unique aesthetics.
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = drv, shape = factor(year), size = factor(drv))) +
#'   scale_size_ordinal() +
#'   guides(colour = my_guide, shape = my_guide, size = my_guide)
#'
#' # You can merge an aesthetic that is already crossed with an aesthetic that
#' # contributes to only one side of the cross.
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = paste(year, drv), shape  = drv)) +
#'   guides(
#'     colour = guide_legend_cross(title = "My Title"),
#'     shape  = guide_legend_cross(title = "My Title", key = "auto")
#'   )
guide_legend_cross <- function(
  key = NULL,
  title = waiver(),
  row_title = waiver(),
  col_title = waiver(),
  swap = FALSE,
  col_text = element_text(angle = 90, vjust = 0.5),
  override.aes = list(),
  reverse = FALSE,
  theme = NULL,
  position = NULL,
  direction = NULL,
  order = 0
) {

  check_position(position, theta = FALSE, inside = TRUE, allow_null = TRUE)
  check_argmatch(direction, c("horizontal", "vertical"), allow_null = TRUE)
  check_bool(swap)

  if (length(reverse) == 1L) {
    check_bool(reverse)
  } else {
    check_length(reverse, exact = 2)
    check_bool(reverse[1])
    check_bool(reverse[2])
  }

  dim_order <- if (swap) c("col", "row") else c("row", "col")

  new_guide(
    key = key,
    title = title,
    row_title = row_title,
    col_title = col_title,
    dim_order = dim_order,
    override.aes = rename_aes(override.aes),
    col_text = col_text,
    reverse = reverse,
    theme = theme,
    position = position,
    direction = direction,
    order = order,
    super = GuideLegendCross
  )
}

# Class -------------------------------------------------------------------

GuideLegendCross <- ggproto(

  "GuideLegendCross", GuideLegendBase,

  params = new_params(
    override.aes = list(), reverse = FALSE,
    row_title = waiver(), col_title = waiver(),
    key = NULL, dim_order = c("row", "col"),
    col_text = NULL
  ),

  hashables = exprs(title, "GuideLegendCross"),

  elements = list2(
    !!!GuideLegendBase$elements,
    subtitle = "legendry.legend.subtitle"
  ),

  extract_key = function(scale, aesthetic, key = NULL,
                         dim_order = c("row", "col"), ...) {

    key <- standard_extract_key(scale, aesthetic, key %||% "group_split", ...)
    grouping <- c(".label", ".group")

    # If we don't have grouping columns yet,
    # we cannot start filling in the layout
    if (!all(grouping %in% names(key))) {
      return(key)
    }

    # Start filling in layout, to be finalised later
    row  <- unique0(key[grouping[dim_order == "row"]][[1]])
    col  <- unique0(key[grouping[dim_order == "col"]][[1]])
    grid <- vec_expand_grid(row = row, col = col)

    # Repeat key to match layout
    i    <- vec_match(grid, rename(key[grouping], grouping, dim_order))
    key  <- vec_slice(key, i)

    # Fill in locations
    key$.row_label <- grid$row
    key$.col_label <- grid$col
    key
  },

  merge = function(self, params, new_guide, new_params) {

    old_key <- params$key
    new_key <- new_params$key

    columns   <- c(".row_label", ".col_label")
    old_ready <- all(columns %in% names(old_key))
    new_ready <- all(columns %in% names(new_key))

    if (!old_ready) {
      params$key <- cross_merge_incomplete(old_key, new_key, params$dim_order)
    } else if (new_ready) {
      params$key <- cross_merge_complete(old_key, new_key)
    } else {
      params$key <- cross_merge_partial(old_key, new_key)
    }

    params$override.aes <-
      merge_legend_override(params$override.aes, new_params$override.aes)
    list(guide = self, params = params)
  },

  setup_params = function(params) {

    key <- params$key
    params$n_breaks <- n_breaks <- nrow(key)

    key$.index <- vec_seq_along(key)
    key$.row <- match_self(key$.row_label %||% seq_len(n_breaks))
    key$.col <- match_self(key$.col_label %||% rep_len(1, n_breaks))

    reverse <- rep_len(params$reverse, 2L)
    if (reverse[1]) {
      nrows <- max(key$.row, na.rm = TRUE)
      key$.row <- nrows - key$.row + 1L
    }
    if (reverse[2]) {
      ncols <- max(key$.col, na.rm = TRUE)
      key$.col <- ncols - key$.col + 1L
    }

    params$key <- key
    params
  },

  setup_elements = function(params, elements, theme) {

    theme <- theme + params$theme
    params$theme <- NULL

    text_position  <- theme$legend.text.position  %||% "right"
    title_position <- theme$legend.title.position %||%
      switch(params$direction, vertical = "top", horizontal = "left")
    elements$title <- setup_legend_title(theme, title_position)

    # Resolve text positions
    row <- intersect(c("right", "left"), text_position)[1] %|NA|% "right"
    col <- intersect(c("top", "bottom"), text_position)[1] %|NA|% "bottom"

    # Resolve text theming
    elements$text_row <- setup_legend_text(theme, row)
    elements$text_col <- combine_elements(
      params$col_text,
      setup_legend_text(theme, col)
    )

    elements$subtitle_row <- setup_legend_title(theme, row, element = elements$subtitle)
    elements$subtitle_col <- setup_legend_title(theme, col, element = elements$subtitle)
    original <- theme[[elements$subtitle]]
    if (is.null(original@hjust)) {
      elements$subtitle_row@hjust <- 0.5
      elements$subtitle_col@hjust <- 0.5
    }
    if (is.null(original@angle)) {
      elements$subtitle_row@angle <- switch(row, right = -90, left = 90)
    }

    elements <- Guide$setup_elements(params, elements, theme)
    elements[c("row_position", "col_position")] <- list(row, col)
    elements$title_position <- title_position
    elements
  },

  build_labels = function(key, elements, params) {

    # Render row labels first
    rows <- vec_slice(key, !duplicated(key$.row))
    rows$.label <- rows$.row_label %||% rows[[".label"]]
    rows <- GuideLegendBase$build_labels(rows, list(text = elements[["text_row"]]), params)

    # Then column labels follow
    cols <- vec_slice(key, !duplicated(key$.col))
    cols$.label <- cols$.col_label %||% cols[[".label"]]
    cols <- GuideLegendBase$build_labels(cols, list(text = elements[["text_col"]]), params)

    # We don't combine them yet, as they need to be measured separately later
    list(rows = rows, cols = cols)
  },

  build_title = function(label, elements, params) {
    main <- GuideLegend$build_title(label, elements, params)
    row <- element_grob(
      elements$subtitle_row,
      label = params$row_title,
      margin_x = TRUE,
      margin_y = TRUE
    )
    col <- element_grob(
      elements$subtitle_col,
      label = params$col_title,
      margin_x = TRUE,
      margin_y = TRUE
    )
    list(main = main, col = col, row = row)
  },

  measure_grobs = function(grobs, params, elements) {

    # Get width of keys per column
    col <- params$key[[".col"]]
    widths <- map_dbl(grobs$decor, `[[`, i = "width")
    widths <- pmax(by_group(widths, col, max), elements$width_cm)

    # Weave in width of labels
    widths <- pmax(widths, width_cm(grobs$labels$cols))
    widths <- vec_interleave(elements$spacing_x %||% 0, widths)[-1]
    label_width <- max(width_cm(grobs$labels$rows))
    widths <- switch(
      elements$row_position,
      left = c(label_width, widths),
      c(widths, label_width)
    )

    # Get height of keys per row
    row <- params$key[[".row"]]
    heights <- map_dbl(grobs$decor, `[[`, i = "height")
    heights <- pmax(by_group(heights, row, max), elements$height_cm)

    # Weave in heights of labels
    heights <- pmax(heights, height_cm(grobs$labels$rows))
    heights <- vec_interleave(elements$spacing_y %||% 0, heights)[-1]
    label_height <- max(height_cm(grobs$labels$cols))
    heights <- switch(
      elements$col_position,
      top = c(label_height, heights),
      c(heights, label_height)
    )

    list(widths = widths, heights = heights)
  },

  arrange_layout = function(key, sizes, params, elements) {

    # Account for spacing in between keys
    key_row <- key[[".row"]] * 2 - 1
    key_col <- key[[".col"]] * 2 - 1

    lab_row <- max(key_row) + 1
    lab_col <- max(key_col) + 1

    if (elements$row_position == "left") {
      key_col <- key_col + 1
      lab_col <- 1
    }

    if (elements$col_position == "top") {
      key_row <- key_row + 1
      lab_row <- 1
    }

    cols <- unique(key_col)
    rows <- unique(key_row)

    lab_row <- c(rows, rep(lab_row, length(cols)))
    lab_col <- c(rep(lab_col, length(rows)), cols)

    list(
      key_row = key_row, key_col = key_col,
      label_row = lab_row, label_col = lab_col
    )
  },

  assemble_drawing = function(self, grobs, layout, sizes, params, elements) {

    widths <- unit(sizes$widths, "cm")
    if (isTRUE(elements$stretch_x)) {
      widths[unique0(layout$key_col)] <- elements$key_width
    }

    heights <- unit(sizes$heights, "cm")
    if (isTRUE(elements$stretch_y)) {
      heights[unique0(layout$key_row)] <- elements$key_height
    }

    gt <- gtable(widths = widths, heights = heights)

    # Add keys
    if (!is_zero(grobs$decor)) {
      gt <- gtable_add_grob(
        gt, grobs$decor,
        name = names(grobs$decor),
        clip = "off",
        t = layout$key_row, r = layout$key_col,
        b = layout$key_row, l = layout$key_col
      )
    }

    # Add labels
    grobs$labels <- c(grobs$labels$rows, grobs$labels$cols)
    if (!is_zero(grobs$labels)) {
      gt <- gtable_add_grob(
        gt, grobs$labels,
        name = paste("label", layout$label_row, layout$label_col, sep = "-"),
        clip = "off",
        t = layout$label_row, r = layout$label_col,
        b = layout$label_row, l = layout$label_col
      )
    }

    gt <- self$add_title(
      gt, grobs$title$main, elements$title_position,
      rotate_just(element = elements$title)
    )

    # Add padding and background
    gt <- gtable_add_padding(gt, unit(elements$padding, "cm"))
    if (!is_zero(elements$background)) {
      gt <- gtable_add_grob(
        gt, elements$background,
        name = "background", clip = "off",
        t = 1, r = -1, b = -1, l =1, z = -Inf
      )
    }
    gt
  }
)


# Helpers -----------------------------------------------------------------

cross_merge_complete <- function(old, new) {
  columns <- c(".row_label", ".col_label")
  if (!identical(old[columns], new[columns])) {
    old_aes <- colnames(old)[!startsWith(colnames(old), ".")]
    new_aes <- colnames(new)[!startsWith(colnames(new), ".")]
    cli::cli_abort(
      "Cannot merge legends for {.field {old_aes}} and {.field {new_aes}}."
    )
  }
  data_frame0(!!!defaults(old, new))
}

cross_merge_partial <- function(old, new) {
  new_aes <- colnames(new)[!startsWith(colnames(new), ".")]

  row_match <- match(old$.row_label, new$.label)
  col_match <- match(old$.col_label, new$.label)

  index <- if (!anyNA(row_match)) row_match else col_match
  if (anyNA(index)) {
    old_aes <- colnames(old)[!startsWith(colnames(old), ".")]
    cli::cli_abort(c(
      "Cannot match legend for {.field {new_aes}} aesthetic{?s}.",
      i = "The labels mismatch those of the {.field {old_aes}} aesthetic{?s}."
    ))
  }
  old[new_aes] <- new[index, new_aes]
  old
}

cross_merge_incomplete <- function(old, new, order = c("row", "col")) {
  if (identical(old$.label, new$.label)) {
    return(data_frame0(!!!defaults(old, new)))
  }

  grid <- vec_expand_grid(
    old = vec_seq_along(old),
    new = vec_seq_along(new)
  )

  old <- vec_slice(old, grid$old)
  new <- vec_slice(new, grid$new)

  order <- paste0(".", order, "_label")
  old[order[1]] <- old$.label
  new[order[2]] <- new$.label
  data_frame0(!!!defaults(old, new))
}

merge_legend_override <- function(old, new) {
  new <- c(old, new)
  dup <- duplicated(names(new))
  if (any(dup)) {
    cli::cli_warn(
      "Duplicated {.arg override.aes} are ignored: {.field {names(new)[dup]}}."
    )
  }
  vec_slice(new, !dup)
}
