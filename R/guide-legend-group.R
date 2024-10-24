# Constructor -------------------------------------------------------------

guide_legend_group <- function(
  key = "group_split",
  title = waiver(),
  override.aes = list(),
  nrow = NULL,
  ncol = NULL,
  theme = NULL,
  position = NULL,
  direction = NULL,
  order = 0
) {

  check_position(position, allow_null = TRUE)
  check_argmatch(direction, c("horizontal", "vertical"), allow_null = TRUE)
  check_number_whole(nrow, min = 1, allow_null = TRUE)
  check_number_whole(ncol, min = 1, allow_null = TRUE)
  check_exclusive(nrow, ncol)

  new_guide(
    key = key,
    title = title,
    theme = theme,
    override.aes = override.aes,
    nrow = nrow,
    ncol = ncol,
    order = order,
    available_aes = "any",
    name = "legend_group",
    direction = direction,
    position = position,
    super = GuideLegendGroup
  )
}

# Class -------------------------------------------------------------------

GuideLegendGroup <- ggproto(
  "GuideLegendGroup", GuideLegendBase,

  elements = list2(
    !!!GuideLegendBase$elements,
    subtitle_spacing = "legend.spacing",
    subtitle = "legendry.legend.subtitle",
    subtitle_position = "legendry.legend.subtitle.position"
  ),

  setup_params = function(params) {
    params$direction <- direction <- arg_match0(
      params$direction,
      c("horizontal", "vertical"),
      arg_nm = "direction"
    )
    params$n_breaks <- nrow(params$key)
    params$groups <-
      group_design(params$key, params$nrow, params$ncol, direction)
    params$key <-
      apply_group_design(params$key, params$groups, direction, params$byrow)
    params
  },

  setup_elements = function(params, elements, theme) {
    theme <- theme + params$theme
    params$theme <- NULL

    subtitle_position <- theme$legendry.legend.subtitle.position %||% "top"
    elements$subtitle <-
      setup_legend_title(theme, subtitle_position, element = elements$subtitle)

    elements <- GuideLegendBase$setup_elements(params, elements, theme)
    elements$subtitle_position <- subtitle_position
    elements
  },

  override_elements = function(params, elements, theme) {
    elements <- GuideLegendBase$override_elements(params, elements, theme)
    elements$subtitle_spacing <- convertUnit(
      elements$subtitle_spacing %||% unit(0, "cm"),
      "cm", valueOnly = TRUE
    )
    elements
  },

  build_title = function(label, elements, params) {
    main <- Guide$build_title(label, elements, params)
    subtitles <- lapply(
      params$groups$key,
      function(lab) {
        sub <- element_grob(
          elements$subtitle, label = lab,
          margin_x = TRUE, margin_y = TRUE
        )
        sub$name <- grobName(sub, "guide.subtitle")
        sub
      }
    )
    list(main = main, subtitles = subtitles)
  },

  measure_grobs = function(grobs, params, elements) {
    measures <- GuideLegendBase$measure_grobs(grobs, params, elements)
    measures$sub_widths  <- width_cm( grobs$title$subtitles)
    measures$sub_heights <- height_cm(grobs$title$subtitles)
    measures
  },

  arrange_layout = function(key, sizes, params, elements) {

    layout <- GuideLegendBase$arrange_layout(key, sizes, params, elements)

    group <- as.integer(key$.group)

    key_row <- layout$key_row
    key_col <- layout$key_col
    lab_row <- layout$label_row
    lab_col <- layout$label_col

    t <- by_group(pmin(key_row, lab_row), group, min)
    b <- by_group(pmax(key_row, lab_row), group, max)
    l <- by_group(pmin(key_col, lab_col), group, min)
    r <- by_group(pmax(key_col, lab_col), group, max)

    widths     <- sizes$widths
    heights    <- sizes$heights
    sub_width  <- by_group(sizes$sub_widths,  l, max)
    sub_height <- by_group(sizes$sub_heights, t, max)
    spacing    <- elements$subtitle_spacing

    position <- elements$subtitle_position
    aligned_top  <- all(t == t[1])
    aligned_left <- all(l == l[1])
    if (position != "top" & aligned_top) {
      b[] <- max(b) # align bottom
    }
    if (position != "left" & aligned_left) {
      r[] <- max(r) # align right
    }

    subtitle_cell  <- switch(position, top = t, left = l, bottom = b, right = r)
    cells  <- unique(subtitle_cell)
    subtitle_cell <- subtitle_cell + match(subtitle_cell, cells)



    topleft <- position %in% c("top", "left")
    if (topleft) {
      spacing_index <- (subtitle_cell <- subtitle_cell - 1L) - 1L
    } else {
      spacing_index <- subtitle_cell + 1L
    }

    just <- get_just(elements$subtitle)
    insert <- if (topleft) insert_before else insert_after

    row_add <- col_add <- 0L
    if (position %in% c("top", "bottom")) {
      row_add <- findInterval(key_row, cells, left.open = !topleft)
      t <- b <- subtitle_cell
      heights <- insert(heights, cells, sub_height)
      heights <- set_within(heights, spacing_index, spacing)
      end <- unique(r)
      start <- unique(l)

      if (aligned_top) {
        widths <- set_within(widths, start - 1L, spacing)
      }
      widths <- insert_spillover(widths, start, end, sub_width, position, just$hjust)

      index <- reeindex(length(widths), start, end)
      key_col <- index[key_col]
      lab_col <- index[lab_col]
      l <- index[l] - 1
      r <- index[r] + 1
    } else {
      col_add <- findInterval(key_col, cells, left.open = !topleft)
      l <- r <- subtitle_cell
      widths <- insert(widths, cells, sub_width)
      widths <- set_within(widths, spacing_index, spacing)

      start <- unique(t)
      end   <- unique(b)

      if (aligned_left) {
        heights <- set_within(heights, start - 1L, spacing)
      }
      heights <- insert_spillover(heights, start, end, sub_height, position, just$vjust)

      index <- reeindex(length(heights), start, end)
      key_row <- index[key_row]
      lab_row <- index[lab_row]
      t <- index[t] - 1
      b <- index[b] + 1
    }

    key_row <- key_row + row_add
    lab_row <- lab_row + row_add
    key_col <- key_col + col_add
    lab_col <- lab_col + col_add

    groups <- params$groups
    groups[, c("t", "r", "b", "l")] <- list(t, r, b, l)

    df <- cbind(key, key_row, key_col, label_row = lab_row, label_col = lab_col)
    list(layout = df, heights = heights, widths = widths, groups = groups)
  },

  assemble_drawing = function(self, grobs, layout, sizes, params, elements) {
    widths <- unit(layout$widths, "cm")
    if (isTRUE(elements$stretch_x)) {
      widths[unique0(layout$layout$key_col)] <- elements$key_width
    }
    heights <- unit(layout$heights, "cm")
    if (isTRUE(elements$stretch_y)) {
      heights[unique0(layout$layout$key_row)] <- elements$key_height
    }
    groups <- layout$groups
    layout <- layout$layout
    gt <- gtable(widths = widths, heights = heights)

    if (!is.zero(grobs$decor)) {
      gt <- gtable_add_grob(
        gt, grobs$decor, name = names(grobs$decor),
        t = layout$key_row, l = layout$key_col,
        clip = "off"
      )
    }
    if (!is.zero(grobs$labels)) {
      gt <- gtable_add_grob(
        gt, grobs$labels, name = names(grobs$labels) %||%
          paste("label", layout$label_row, layout$label_col, sep = "-"),
        t = layout$label_row, l = layout$label_col,
        clip = "off"
      )
    }
    if (!is.zero(grobs$title$subtitles)) {
      gt <- gtable_add_grob(
        gt, grobs$title$subtitles, name = names(grobs$title$subtitles) %||%
          paste0("subtitle-", seq_along(grobs$title$subtitles)),
        t = groups$t, r = groups$r, b = groups$b, l = groups$l, clip = "off"
      )
    }
    gt <- self$add_title(gt, grobs$title$main, elements$title_position,
                         get_just(elements$title))
    gt <- gtable_add_padding(gt, unit(elements$padding, "cm"))
    if (!is.zero(elements$background)) {
      gt <- gtable_add_grob(gt, elements$background, name = "background",
                            clip = "off", t = 1, r = -1, b = -1, l = 1, z = -Inf)
    }
    gt
  }
)

# Helpers -----------------------------------------------------------------

group_design <- function(key, nrow = NULL, ncol = NULL,
                         direction = "vertical") {
  groups <- vec_count(key$.group)
  groups <- vec_slice(groups, order(match(groups$key, key$.group)))
  n <- groups$count

  if (is.null(nrow) && is.null(ncol)) {
    if (direction == "horizontal") {
      nrow <- ceiling(n / 5)
    } else {
      ncol <- ceiling(n / 20)
    }
  }

  groups$nrow <- nrow %||% ceiling(n / ncol)
  groups$ncol <- ceiling(n / groups$nrow)
  groups
}

apply_group_design <- function(key, groups, direction = "vertical", byrow = FALSE) {

  nrow <- rep(groups$nrow, groups$count)
  ncol <- rep(groups$ncol, groups$count)

  index <- seq_len(sum(groups$count))
  sub_index <- vec_ave(index, key$.group, seq_along)

  if (byrow) {
    row <- ceiling(sub_index / ncol)
    col <- (sub_index - 1L) %% ncol + 1
  } else {
    row <- (sub_index - 1L) %% nrow + 1
    col <- ceiling(sub_index / nrow)
  }

  if (direction == "vertical") {
    row <- row + rep(cumsum(c(0, groups$nrow[-nrow(groups)])), groups$count)
  } else {
    col <- col + rep(cumsum(c(0, groups$ncol[-nrow(groups)])), groups$count)
  }

  key$.index <- index
  key$.row   <- row
  key$.col   <- col
  key
}

set_within <- function(x, i, value) {
  i <- i[i > 0 & i <= length(x)]
  x[i] <- value
  x
}

insert_spillover <- function(size, start, end, extra, position, just = NULL) {
  cumsize <- cumsum(size)
  extra_size <- pmax(0, extra - (cumsize[end] - c(0, cumsize)[start]))
  just <- (just %||% 0.5) * c(1, -1) + c(0, 1)

  if (position %in% c("left", "right")) {
    just <- rev(just)
  }

  size <- insert_before(size, start, extra_size * just[1])
  insert_after(size, end + match(start, start), extra_size * just[2])
}

reeindex <- function(n, start, end) {
  index <- seq_len(n)
  new_index <- insert_before(index, start, NA)
  new_index <- insert_after(new_index, end + match(start, start), NA)
  match(index, new_index)
}
