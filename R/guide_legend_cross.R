# Constructor -------------------------------------------------------------

#' Cross table legend guide
#'
#' Like regular legends, this guide shows keys mapped onto values. It is useful
#' for displaying the interaction of two variables more intuitively than a
#' linear legend.
#'
#' @param sep A `character(1)` or `NULL`. When:
#'   \describe{
#'     \item{A `character(1)`}{Acts as a ['regex'][base::regex] pattern to split
#'     *a single set* of labels into two parts. Defaults to any non-alphanumeric
#'     pattern. Note that special regex characters need to be escaped, so
#'     splitting on a period would have to be `"\\."`.}
#'     \item{`NULL`}{Disables string splitting, which should only be used when
#'     combining *two scales* that don't share labels in a single legend. See
#'     examples below.}
#'   }
#' @param label_order A `character(2)` giving the order of dimensions to which
#'   the first and second label should map to. Can either be
#'   `c("row", "column")` or `c("column", "row")`.
#' @param h_label.theme,v_label.theme An
#'   [`<element_text>`][ggplot2::element_text()] (default) or `<element_blank>`
#'   object controlling the appearance of horizontal (left/right) or vertical
#'   (top/bottom) labels respectively. These inherit from the `legend.text`
#'   setting in the plot's theme.
#' @param h_label.position,v_label.position A `character(1)` indicating the
#'   position of a label. `h_label.position` can be one of `"left"` or
#'   `"right"` (default), whereas `v_label.position` can be one of `"top"`
#'   or `"bottom"` (default).

#' @param reverse A `logical(2)` or `logical(1)` that internally gets recycled
#'   to length 2. If `reverse[1]` is `TRUE`, reverses the order of the first
#'   label. If `reverse[2]` is `TRUE`, reverses the order of the second label.
#'   Defaults to `c(FALSE, FALSE)`.
#' @inheritDotParams ggplot2::guide_legend title:title.vjust keywidth:override.aes order
#'
#' @details This guide finds an interaction of two variables by trying to split
#'   a compound label. Unfortunately, this might require some work to format
#'   splittable labels. Moreover, if one intends to use this guide to show
#'   an interaction between, for example: shapes and colours, both the shape
#'   and colour scales should return compound labels.
#'
#' @return A `<Guide>` ggproto object that can be given to the
#'   [`guides()`][ggplot2::guides()] function, or set as the `guide` argument
#'   in a non-position scale.
#' @export
#' @family legend variants
#'
#' @examples
#' # Display interaction by mapping a compound variable
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = paste(cyl, year))) +
#'   guides(colour = "legend_cross")
#'
#' # To make a cross legend for two scales, the plot must satisfy 2 criteria:
#' # 1. Both scales need the same name/title.
#' # 2. The guide needs `sep = NULL` to disable label splitting.
#' # The easiest way to do this is to make one guide and feed it to both scales.
#' guide <- guide_legend_cross(sep = NULL, title = "My Title")
#'
#' ggplot(mtcars, aes(mpg, disp)) +
#'   geom_point(aes(colour = factor(cyl), shape = factor(vs))) +
#'   guides(colour = guide, shape = guide)
guide_legend_cross <- function(
  sep         = "[^[:alnum:]]+",
  label_order = c("row", "column"),

  h_label.theme    = element_text(),
  h_label.position = "right",

  v_label.theme    = element_text(angle = 90, vjust = 0.5),
  v_label.position = "bottom",

  reverse = FALSE,

  ...
) {

  h_label.position <- arg_match0(h_label.position, c("left", "right"))
  v_label.position <- arg_match0(v_label.position, c("top", "bottom"))

  valid_order <- length(label_order) == 2L & is.character(label_order)
  valid_order <- valid_order & all(c("row", "column") %in% label_order)

  abort_if(
    !valid_order,
    'The {.arg label_order} argument should be either ',
    '{.code c("row", "column")} or {.code c("column", "row")}.'
  )

  args <- list2(...)
  super <- args$super %||% GuideLegendCross
  args$super <- NULL

  new_guide(
    label_order   = label_order,
    h_label.theme = h_label.theme,
    h_label.position = h_label.position,
    v_label.theme = v_label.theme,
    v_label.position = v_label.position,
    sep           = sep,
    name = "cross-legend",
    !!!args,
    super = super
  )
}

# Class -------------------------------------------------------------------

GuideLegendCross <- ggproto(
  "GuideLegendCross", GuideLegend,

  params = list(
    title          = waiver(),
    title.position = NULL,
    title.theme    = NULL,

    sep = "[^[:alnum:]]+",

    h_label.theme    = NULL,
    h_label.position = "right",

    v_label.theme    = NULL,
    v_label.position = "bottom",

    keywidth  = NULL,
    keyheight = NULL,

    label_order = c("row", "column"),

    direction = NULL,
    position = NULL,

    name = "cross-legend",
    order = 0,
    hash = character()
  ),

  hashables = exprs(title, sep, direction, name),

  extract_key = function(self, scale, aesthetic, sep = "[^:[:alnum:]]+",
                         reverse = FALSE, label_order = c("row", "column"),
                         ...) {

    if (is.null(sep)) {
      key <- ggproto_parent(GuideLegend, self)$extract_key(scale, aesthetic)
      return(key)
    }

    breaks <- scale$get_breaks()
    label  <- scale$get_labels(breaks)

    # Split and index labels
    split_label <- split_labels(label, sep, snake_class(self))
    split_idx   <- lapply(split_label, function(x) match(x, unique(x)))

    # Reverse grid if appropriate
    reverse <- rep_len(reverse, 2L)
    if (reverse[1]) {
      split_idx[[1]] <- max(split_idx[[1]]) - split_idx[[1]] + 1
    }
    if (reverse[2]) {
      split_idx[[2]] <- max(split_idx[[2]]) - split_idx[[2]] + 1
    }

    # Fill regular parts of key
    mapped <- scale$map(breaks)
    key <- new_data_frame(setNames(list(mapped), aesthetic))
    key$.value <- breaks
    key$.label <- label

    # Supplement split information in key
    lab_ord <- paste0(".", label_order)
    key[, lab_ord] <- split_idx
    key[, paste0(lab_ord, "_label")] <- split_label

    if (is.numeric(breaks)) {
      key[is.finite(breaks), , drop = FALSE]
    } else {
      key
    }
  },

  extract_params = function(scale, params, hashables,
                            title = waiver(), direction = NULL, ...) {

    if (all(c(".row", ".column") %in% names(params$key))) {
      params$ncol <- max(params$key$.column)
      params$nrow <- max(params$key$.row)
    }
    params$reverse <- FALSE

    GuideLegend$extract_params(scale, params, hashables,
                               title = title, direction = direction)

  },

  merge = function(self, params, new_guide, new_params) {

    left_ready  <- all(c(".row", ".column") %in% names(params$key))
    right_ready <- all(c(".row", ".column") %in% names(new_params$key))

    if (left_ready && !right_ready) {
      aesthetic <- new_params$aesthetic
      row_match <- match(params$key$.row_label,    new_params$key$.label)
      col_match <- match(params$key$.column_label, new_params$key$.label)
      drop_row  <- all(is.na(row_match))
      drop_col  <- all(is.na(col_match))

      abort_if(
        drop_row && drop_col,
        "Cannot cross legend for {.field {aesthetic}} aesthetic.",
        i = "The {.field {aesthetic}} labels do not match the others."
      )
      if (!drop_row && !drop_col && !identical(row_match, col_match)) {
        cli::cli_warn(c(
          x = "Ambiguous matches for crossing {.field {aesthetic}} aesthetic.",
          i = "The {.field {aesthetic}} labels matches other labels in >1 scale."
        ))
      }
      if (drop_row) {
        params$key[[aesthetic]] <- new_params$key[[aesthetic]][col_match]
      } else if (drop_col) {
        params$key[[aesthetic]] <- new_params$key[[aesthetic]][row_match]
      }
      params$override.aes <- merge_override(
        params$override.aes,
        new_params$override.aes
      )
      return(list(guide = self, params = params))
    }

    if (left_ready) {
      ans <- ggproto_parent(GuideLegend, self)$merge(params, new_guide,
                                                     new_params)
      return(ans)
    }

    key_old <- params$key
    key_new <- new_params$key


    grid <- expand.grid(left = seq_row(key_old), right = seq_row(key_new))
    left  <- vec_slice(key_old, grid$left)
    right <- vec_slice(key_new, grid$right)

    labels       <- list(left = left$.label, right = right$.label)
    left$.label  <- paste(labels$left, labels$right)
    right$.label <- NULL

    key <- cbind(left, right)
    lab_ord <- paste0(".", params$label_order)
    key[, lab_ord] <- as.list(grid)
    key[, paste0(lab_ord, "_label")] <- labels

    params$ncol <- max(key$.column)
    params$nrow <- max(key$.row)
    params$key <- key

    params$override.aes <- merge_override(
      params$override.aes,
      new_params$override.aes
    )
    list(guide = self, params = params)
  },

  get_layer_key = function(self, params, layers) {

    key_nms <- names(params$key)
    required_cols <- c(".row", ".column", ".row_label", ".column_label")
    if (!all(required_cols %in% key_nms)) {
      if (is.null(params$sep)) {
        cli::cli_abort(c(
          "Scale crossing has failed in {.fun {snake_class(self)}}.",
          "i" = paste0(
            "Using {.arg sep = NULL} in requires exactly two scales with such ",
            " guides, and identical titles."
          )
        ))
      } else {
        cli::cli_abort(paste0(
          "Scale crossing has failed in {.fun {snake_class(self)}} for unclear",
          " reasons."
        ))
      }
    }
    ggproto_parent(GuideLegend, self)$get_layer_key(params, layers)
  },

  override_elements = function(params, elements, theme) {

    title <- combine_elements(params$title.theme, elements$theme.title)
    title$hjust <- params$title.hjust %||% elements$title.align %||%
      title$hjust %||% 0
    title$vjust <- params$title.vjust %||% title$vjust %||% 0.5
    elements$title <- title

    legend.text  <- calc_element("legend.text", theme)

    hlabel.theme <- combine_elements(params$h_label.theme, legend.text)
    hlabel.theme <- set_text_just(
      hlabel.theme, params$h_label.position, params$h_label.theme, theme
    )
    elements$hlabel <- hlabel.theme

    vlabel.theme <- combine_elements(params$v_label.theme, legend.text)
    vlabel.theme <- set_text_just(
      vlabel.theme, params$v_label.position, params$v_label.theme, theme
    )
    elements$vlabel <- vlabel.theme

    elements$key.width  <-  width_cm(params$key.width  %||% elements$key.width)
    elements$key.height <- height_cm(params$key.height %||% elements$key.height)

    gap <- title$size %||% elements$theme.title$size %||% elements$text$size %||% 11
    gap <- unit(gap * 0.5, "pt")

    elements$hgap <-  width_cm(theme$legend.spacing.x %||% gap)
    elements$vgap <- height_cm(theme$legend.spacing.y %||% gap)
    elements$padding <- convertUnit(
      elements$margin %||% margin(),
      "cm", valueOnly = TRUE
    )

    # Evaluate backgrounds early
    elements$background <- ggname(
      "legend.background", element_grob(elements$background)
    )
    elements$key <- ggname(
      "legend.key", element_grob(elements$key)
    )
    elements
  },

  build_labels = function(key, elements, params) {

    temp_key <- key[!duplicated(key$.row), , drop = FALSE]
    temp_key$.label <- temp_key$.row_label
    temp_elem <- `[[<-`(elements, "text", elements$hlabel)
    h_labels <- GuideLegend$build_labels(temp_key, temp_elem, params)

    temp_key <- key[!duplicated(key$.column), , drop = FALSE]
    temp_key$.label <- temp_key$.column_label
    temp_elem <- `[[<-`(elements, "text", elements$vlabel)
    v_labels <- GuideLegend$build_labels(temp_key, temp_elem, params)

    list(h = h_labels, v = v_labels)
  },

  measure_grobs = function(grobs, params, elems) {

    byrow    <- params$label_order[1] == "row"
    n_breaks <- params$n_breaks %||% 1L
    dim      <- c(params$nrow %||% 1L, params$ncol %||% 1L)

    sizes <- params$sizes %||% ggplot2:::measure_legend_keys(
      params$decor, n = n_breaks, dim = dim, byrow = byrow,
      default_width  = elems$key.width,
      default_height = elems$key.height
    )
    widths  <- sizes$widths
    heights <- sizes$heights

    # Measure label sizes
    h_label_widths  <- max(width_cm(grobs$labels$h))
    v_label_widths  <- width_cm(grobs$labels$v)
    h_label_heights <- height_cm(grobs$labels$h)
    v_label_heights <- max(height_cm(grobs$labels$v))

    hgap <- elems$hgap
    vgap <- elems$vgap

    h_position <- params$h_label.position
    v_position <- params$v_label.position

    widths  <- pmax(v_label_widths,  widths)
    heights <- pmax(h_label_heights, heights)

    semi_widths <- switch(
      params$h_label.position,
      "left"  = c(h_label_widths, hgap, head(vec_interleave(widths, hgap), -1)),
      "right" = c(head(vec_interleave(widths, hgap), -1), hgap, h_label_widths)
    )

    semi_heights <- switch(
      params$v_label.position,
      "top"    = c(v_label_heights, vgap, head(vec_interleave(heights, vgap), -1)),
      "bottom" = c(head(vec_interleave(heights, vgap), -1), vgap, v_label_heights),
    )

    title_width  <- width_cm(grobs$title)
    title_height <- height_cm(grobs$title)

    widths <- switch(
      params$title.position,
      "left"  = c(title_width, hgap, semi_widths),
      "right" = c(semi_widths,  hgap, title_width),
      c(semi_widths, max(0, title_width - sum(semi_widths)))
    )
    heights <- switch(
      params$title.position,
      "top"    = c(title_height, vgap, semi_heights),
      "bottom" = c(semi_heights, vgap, title_height),
      c(semi_heights, max(0, title_height - sum(heights)))
    )

    list(
      widths  = widths,
      heights = heights,
      padding = elems$padding
    )
  },

  arrange_layout = function(key, sizes, params) {

    df <- setNames(key[, c(".row", ".column")], c("R", "C"))

    key_row <- label_row <- df$R * 2
    key_col <- label_col <- df$C * 2

    if (params$v_label.position == "top") {
      key_row <- label_row <- key_row + 2
      alt_row <- min(key_row) - 2
    } else {
      alt_row <- max(key_row) + 2
    }
    if (params$h_label.position == "left") {
      key_col <- label_col <- key_col + 2
      alt_col <- min(key_col) - 2
    } else {
      alt_col <- max(key_col) + 2
    }

    n_widths  <- length(sizes$widths)
    n_heights <- length(sizes$heights)
    switch(
      params$title.position,
      "top" = {
        key_row   <- key_row   + 2
        label_row <- label_row + 2
        alt_row   <- alt_row   + 2
        title_row <- 2
        title_col <- seq_len(n_widths) + 1
      },
      "bottom" = {
        title_row <- n_heights + 1
        title_col <- seq_len(n_widths) + 1
      },
      "left" = {
        key_col   <- key_col   + 2
        label_col <- label_col + 2
        alt_col   <- alt_col   + 2
        title_row <- seq_len(n_heights) + 1
        title_col <- 2
      },
      "right" = {
        title_row <- seq_len(n_heights) + 1
        title_col <- n_widths + 1
      }
    )

    df <- cbind(df, key_row, key_col, label_row, label_col, alt_row, alt_col)
    list(layout = df, title_row = title_row, title_col = title_col)

  },

  assemble_drawing = function(grobs, layout, sizes, params, elements) {

    gt <- gtable(
      widths  = unit(c(sizes$padding[4], sizes$widths,  sizes$padding[2]), "cm"),
      heights = unit(c(sizes$padding[1], sizes$heights, sizes$padding[3]), "cm")
    )

    # Add background
    gt <- gtable_add_grob(
      gt, elements$background, name = "background", clip = "off",
      t = 1, r = -1, b = -1, l = 2
    )

    # Add title
    gt <- gtable_add_grob(
      gt, justify_grobs(grobs$title, theme = elements$title),
      name = "title", clip = "off",
      t = min(layout$title_row), r = max(layout$title_col),
      b = max(layout$title_row), l = min(layout$title_col)
    )

    layout <- layout$layout
    n_key_layers <- params$n_key_layers %||% 1L
    key_cols <- rep(layout$key_col, each = n_key_layers)
    key_rows <- rep(layout$key_row, each = n_key_layers)

    gt <- gtable_add_grob(
      gt, grobs$decor, name = names(grobs$decor) %||% paste(
        "key", key_rows, key_cols, c("bg", seq_len(n_key_layers - 1)),
        sep = "-"
      ),
      clip = "off", t = key_rows, r = key_cols, b  = key_rows, l = key_cols
    )
    i <- which(!duplicated(layout$R))
    gt <- gtable_add_grob(
      gt, justify_grobs(grobs$labels$h, theme = elements$hlabel),
      name = paste0("hlabel", layout$label_row[i], layout$alt_col[i], sep = "-"),
      t = layout$label_row[i], l = layout$alt_col[i]
    )

    i <- which(!duplicated(layout$C))
    gt <- gtable_add_grob(
      gt, justify_grobs(grobs$labels$v, theme = elements$vlabel),
      name = paste0("vlabel", layout$alt_row[i], layout$label_col[i], sep = "-"),
      t = layout$alt_row[i], l = layout$label_col[i]
    )

    gt

  }

)


# Helpers -----------------------------------------------------------------

split_labels <- function(labels, sep, fun_nm, n_labs = 2) {
  splitted <- strsplit(labels, sep)
  lens <- lengths(splitted)
  msg <- character(0)

  if (any(lens < n_labs)) {

    splitted <- clapply(splitted, lens < n_labs, function(x) {
      c(x, rep("", n_labs - length(x)))
    })
    failed <- labels[lens < n_labs]
    failed <- chr_vapply(failed, function(x) as_cli("{.val {x}}"))
    msg <- c(msg, "!" = as_cli(
      "Splitting labels on {.val {sep}} has failed for the following labels: ",
      "{.and {failed}}."
    ), "i" = "They have been padded with empty strings.")

  }

  if (any(lens > n_labs)) {
    failed <- labels[lens > n_labs]
    failed <- chr_vapply(failed, function(x) as_cli("{.val {x}}"))
    msg <- c(msg, "!" = as_cli(
      "Splitting labels on {.val {sep}} has produced too many results for ",
      "the following labels: {.and {failed}}."
    ), "i" = as_cli("Only the first {n_labs} were taken."))
  }

  if (length(msg) > 0) {
    cli::cli_warn(c("In {.fn {fun_nm}}:", msg))
  }

  list(
    chr_vapply(splitted, `[[`, i = 1),
    chr_vapply(splitted, `[[`, i = 2)
  )
}

merge_override <- function(current, new) {
  current <- c(current, new)
  is_dup  <- duplicated(names(current))
  if (anyDuplicated(names(current))) {
    dupped <- names(current)[is_dup]
    dupped <- chr_vapply(dupped, function(x) as_cli("{.field {x}}"))
    cli::cli_warn(paste0(
      "The following duplicated {.arg override.aes} {?is/are} ignored: ",
      "{.and {dupped}}."
    ))
  }
  current <- current[!is_dup]
  current
}

set_text_just <- function(
  text, position, naive_text, theme,
  hjust = NULL, vjust = NULL
) {

  if (is_blank(text)) {
    return(text)
  }
  defaults <- legend_just_defaults[[position]]
  if (is.null(naive_text$hjust) && is.null(theme$legend.text$hjust)) {
    text$hjust <- NULL
  }
  if (is.null(naive_text$vjust) && is.null(theme$legend.text$vjust)) {
    text$vjust <- NULL
  }
  text$hjust <- hjust %||% theme$legend.text.align %||% text$hjust %||%
    defaults$hjust
  text$vjust <- vjust %||% text$vjust %||% defaults$vjust
  text
}

legend_just_defaults <- list(
  top    = list(hjust = 0.5, vjust = 0),
  bottom = list(hjust = 0.5, vjust = 1),
  left   = list(hjust = 1,   vjust = 0.5),
  right  = list(hjust = 0,   vjust = 0.5)
)
