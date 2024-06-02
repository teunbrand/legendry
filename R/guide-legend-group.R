# Constructor -------------------------------------------------------------

#' Grouped legend
#'
#' @param key
#' @param override.aes
#' @param nrow
#' @param ncol
#' @inheritParams common_parameters
#'
#' @return
#' @export
#'
#' @examples
guide_legend_group <- function(
  key = key_group_auto(),
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
    key   = key,
    title = title,
    theme = theme,
    override.aes = override.aes,
    nrow = nrow,
    ncol = ncol,
    order = order,
    available_aes = "any",
    name = "legend_group",
    super = GuideLegendGroup
  )
}


# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideLegendGroup <- ggproto(
  "GuideLegendGroup", GuideLegend,

  params = c(GuideLegend$params, list(key = NULL)),

  elements = c(
    GuideLegend$elements,
    subtitle = "gguidance.legend.subtitle",
    subtitle_position = "gguidance.legend.subtitle.position",
    group_spacing = "gguidance.legend.group.spacing"
  ),

  extract_key = function(scale, aesthetic, key, ...) {
    key <- resolve_key(key %||% "group_auto")
    if (is.function(key)) {
      key <- key(scale, aesthetic)
    }
    if ("aesthetic" %in% names(key)) {
      key$aesthetic <-
        scale_transform(key$aesthetic, scale, map = TRUE, "aesthetic")
      key$.value <-
        scale_transform(key$.value, scale, map = FALSE, "value")
    }
    key <- rename(key, "aesthetic", aesthetic)
    key <- validate_key_types(key)
    check_columns(key, ".group")

    if (is.numeric(key$.value)) {
      range <- scale$continuous_range %||% scale$get_limits()
      key <- vec_slice(key, is.finite(oob_censor_any(key$.value, range)))
    }
    key
  },

  setup_params = function(params) {
    check_argmatch(
      params$direction, c("horizontal", "vertical"),
      arg = "direction"
    )
    params$group_direction <- params$group_direction %||% params$direction

    params$n_breaks <- n_breaks <- nrow(params$key)
    params$n_key_layers <- length(params$decor) + 1

    groups <- vec_count(params$key$.group)
    groups <- vec_slice(groups, order(match(groups$key, params$key$.group)))
    groups[c("nrow", "ncol")] <- resolve_legend_shape(
      nrow = params$nrow, ncol = params$ncol,
      n = groups$count, direction = params$direction
    )
    params$groups <- groups

    if (params$direction == "horizontal") {
      params$ncol <- sum(groups$ncol)
      params$nrow <- max(groups$nrow)
    } else {
      params$ncol <- max(groups$ncol)
      params$nrow <- sum(groups$nrow)
    }
    params
  },

  setup_elements = function(params, elements, theme) {

    theme <- theme + params$theme
    sub_position <- calc_element("gguidance.legend.subtitle.position", theme)
    check_position(sub_position, .trbl, arg = "gguidance.legend.subtitle.position")
    gap <- theme$legend.key.spacing <- calc_element("legend.key.spacing", theme)

    margin <- calc_element("text", theme)$margin
    text <- theme(text = element_text(
      hjust = 0, vjust = 0.5,
      margin = position_margin(sub_position, margin, gap * 0.5)
    ))
    elements$subtitle <- calc_element(elements$subtitle, theme + text)
    theme$legend.key.spacing.y <- theme$legend.key.spacing.y %||% rel(1)

    GuideLegend$setup_elements(params, elements, theme)
  },

  build_title = function(label, elements, params) {
    main <- element_grob(
      elements$title, label = label,
      margin_x = TRUE, margin_y = TRUE
    )
    main$name <- grobName(main, "guide.title")
    subtitle <- lapply(
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
    c(list(main), subtitle)
  },

  measure_grobs = function(grobs, params, elements) {

    key <- params$key
    n_layers <- params$n_key_layers

    key_widths  <- get_key_size(grobs$decor, "width", nrow(key))
    key_heights <- get_key_size(grobs$decor, "height", nrow(key))

    key_widths  <- pmax(key_widths,  width_cm(elements$key_width))
    key_heights <- pmax(key_heights, height_cm(elements$key_height))

    lab_widths  <- width_cm(grobs$labels)
    lab_heights <- height_cm(grobs$labels)

    groups   <- params$groups
    n_groups <- nrow(groups)

    ncol <- rep(groups$ncol, groups$count)
    nrow <- rep(groups$nrow, groups$count)

    break_seq <- seq_len(sum(groups$count))
    break_seq <- vec_ave(break_seq, key$.group, seq_along)

    if (elements$byrow %||% FALSE) {
      row <- ceiling(break_seq / ncol)
      col <- (break_seq - 1L) %% ncol + 1
    } else {
      row <- (break_seq - 1L) %% nrow + 1
      col <- ceiling(break_seq / nrow)
    }

    if (params$direction == "vertical") {
      row <- row + rep(cumsum(c(0, groups$nrow[-n_groups])), groups$count)
    } else {
      col <- col + rep(cumsum(c(0, groups$ncol[-n_groups])), groups$count)
    }
    key$row <- row
    key$col <- col

    key_heights <- by_group(key_heights, row, max)
    lab_heights <- by_group(lab_heights, row, max)
    key_widths  <- by_group(key_widths,  col, max)
    lab_widths  <- by_group(lab_widths,  col, max)

    position <- elements$text_position

    hgap <- elements$spacing_x
    widths <- switch(
      position,
      left  = list(lab_widths, key_widths, hgap),
      right = list(key_widths, lab_widths, hgap),
      list(pmax(lab_widths, key_widths), hgap)
    )

    widths <- vec_interleave(!!!widths)
    widths <- widths[-length(widths)]

    vgap <- elements$spacing_y
    heights <- switch(
      position,
      top    = list(lab_heights, key_heights, vgap),
      bottom = list(key_heights, lab_heights, vgap),
      list(pmax(lab_heights, key_heights), vgap)
    )
    heights <- vec_interleave(!!!heights)
    heights <- heights[-length(heights)]

    subtitles <- grobs$title[-1]
    subtitle_width  <- width_cm(subtitles)
    subtitle_height <- height_cm(subtitles)

    just <- with(elements$subtitle, rotate_just(angle, hjust, vjust))

    list(
      key = key,
      widths = widths,
      heights = heights,
      padding = elements$padding,
      label_position = elements$text_position,
      title_position = elements$title_position,
      subtitle_position = elements$subtitle_position,
      subtitle_height = subtitle_height,
      subtitle_width  = subtitle_width,
      subtitle_just   = just,
      group_spacing = height_cm(elements$group_spacing),
      groups = groups
    )
  },

  arrange_layout = function(key, sizes, params, elements) {

    key <- sizes$key
    row <- key$row
    col <- key$col

    key_row <- row * 2 - 1
    key_col <- col * 2 - 1

    position <- sizes$label_position
    key_row  <- key_row + switch(position, top  = row, bottom = row - 1, 0)
    lab_row  <- key_row + switch(position, top  = -1,  bottom = 1,       0)
    key_col  <- key_col + switch(position, left = col, right  = col - 1, 0)
    lab_col  <- key_col + switch(position, left = -1,  right  = 1,       0)

    group <- as.integer(key$.group)

    t <- by_group(pmin(key_row, lab_row), group, min)
    b <- by_group(pmax(key_row, lab_row), group, max)
    l <- by_group(pmin(key_col, lab_col), group, min)
    r <- by_group(pmax(key_col, lab_col), group, max)

    top_aligned  <- all(t == t[1])
    left_aligned <- all(l == l[1])

    position   <- sizes$subtitle_position
    widths     <- sizes$widths
    heights    <- sizes$heights
    sub_width  <- by_group(sizes$subtitle_width,  l, max)
    sub_height <- by_group(sizes$subtitle_height, t, max)
    spacing    <- sizes$group_spacing

    row_add <- 0
    col_add <- 0

    if (position == "bottom" && all(t == t[1])) {
      b <- rep(max(b), length(b))
    }
    if (position == "right" && all(l == l[1])) {
      r <- rep(max(r), length(r))
    }

    var  <- switch(position, top = t, left = l, bottom = b, right = r)
    uni  <- unique(var)
    size <- switch(position, top = , bottom = heights, widths)

    if (position == "top") {
      size    <- insert_before(size, uni, sub_height)
      row_add <- findInterval(key_row, uni)
    } else if (position == "left") {
      size    <- insert_before(size, uni, sub_width)
      col_add <- findInterval(key_col, uni)
    } else if (position == "bottom") {
      size    <- insert_after(size, uni, sub_height)
      row_add <- findInterval(key_row, uni, left.open = TRUE)
    } else if (position == "right") {
      size    <- insert_after(size,  uni, sub_width)
      col_add <- findInterval(key_col, uni, left.open = TRUE)
    }

    var <- var + match(var, uni) - as.numeric(position %in% c("top", "left"))
    n <- switch(position, left = , top = c(1, -1), c(length(size), 1))
    size[setdiff(var, n[1]) + n[2]] <- spacing

    if (position %in% c("top", "bottom")) {
      t <- b <- var
      heights <- size
      if (all(t == t[1])) {
        widths[setdiff(unique(l), 1) - 1] <- spacing
      }
      if (all(l == l[1])) {
        r <- rep(max(r), length(r))
      }
      cum_width <- cumsum(widths)
      ur <- unique(r)
      ul <- unique(l)

      extra_width <- pmax(0, sub_width - (cum_width[ur] - c(0, cum_width)[ul]))
      hjust <- sizes$subtitle_just$hjust * c(1, -1) + c(0, 1)

      m <- match(ul, ul)
      index  <- seq_along(widths)
      new_index <- insert_before(index, ul, NA)
      new_index <- insert_after(new_index, ur + m, NA)
      index <- match(index, new_index)

      widths <- insert_before(widths, ul, extra_width * hjust[1])
      widths <- insert_after(widths, ur + m, extra_width * hjust[2])

      key_col <- index[key_col]
      lab_col <- index[lab_col]
      l <- index[l] - 1
      r <- index[r] + 1
    } else {
      l <- r <- var
      widths <- size
      if (all(l == l[1])) {
        heights[setdiff(unique(t), 1) - 1] <- spacing
      }
      if (all(t == t[1])) {
        b <- rep(max(b), length(b))
      }
      cum_height <- cumsum(heights)
      ut <- unique(t)
      ub <- unique(b)

      extra_height <- pmax(0, sub_height - (cum_height[ub] - c(0, cum_height)[ut]))
      vjust <- sizes$subtitle_just$vjust * c(1, -1) + c(0, 1)

      m <- match(ut, ut)
      index <- seq_along(heights)
      new_index <- insert_before(index, ut, NA)
      new_index <- insert_after(new_index, ub + m, NA)
      index <- match(index, new_index)

      heights <- insert_before(heights, ut, extra_height * vjust[2])
      heights <- insert_after(heights, ub + m, extra_height * vjust[1])

      key_row <- index[key_row]
      lab_row <- index[lab_row]
      t <- index[t] - 1
      b <- index[b] + 1
    }

    key_row <- key_row + row_add
    lab_row <- lab_row + row_add
    key_col <- key_col + col_add
    lab_col <- lab_col + col_add

    groups <- sizes$groups
    groups[, c("t", "r", "b", "l")] <- list(t, r, b, l)

    df <- cbind(key, key_row, key_col, label_row = lab_row, label_col = lab_col)
    list(layout = df, groups = groups, heights = heights, widths = widths)
  },

  assemble_drawing = function(self, grobs, layout, sizes, params, elements) {

    subtitles <- grobs$title[-1]
    grobs$title <- grobs$title[[1]]
    groups <- layout$groups

    gt <- legend_assemble(
      unit(layout$widths, "cm"), unit(layout$heights, "cm"),
      layout$layout, grobs, elements, params$n_key_layers
    )

    gt <- gtable_add_grob(
      gt, subtitles,
      t = groups$t, l = groups$l, r = groups$r, b = groups$b,
      name = paste0("subtitle-", seq_len(nrow(groups)))
    )

    gt <- self$add_title(
      gt, grobs$title, elements$title_position,
      with(elements$title, rotate_just(angle, hjust, vjust))
    )
    gt <- gtable_add_padding(gt, elements$margin)
    gt <- legend_add_background(gt, elements$background)
    gt
  }
)

resolve_legend_shape <- function(nrow, ncol, n, direction) {
  if (!is.null(nrow) && !is.null(ncol) &&
      nrow * ncol < n) {
    cli::cli_abort(paste0(
      "{.arg nrow} * {.arg ncol} needs to be larger than the number of ",
      "breaks ({n})."
    ))
  }
  if (is.null(nrow) && is.null(ncol)) {
    if (direction == "horizontal") {
      nrow <- ceiling(n / 5)
    } else {
      ncol <- ceiling(n / 20)
    }
  }
  nrow <- nrow %||% ceiling(n / ncol)
  ncol <- ceiling(n / nrow)
  list(nrow = nrow, ncol = ncol)
}


