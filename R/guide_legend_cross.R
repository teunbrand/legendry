# Public constructor ------------------------------------------------------

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
#' @inheritDotParams guide_legend_vanilla title:title.vjust keywidth:override.aes order
#'
#' @details This guide finds an interaction of two variables by trying to split
#'   a compound label. Unfortunately, this might require some work to format
#'   splittable labels. Moreover, if one intends to use this guide to show
#'   an interaction between, for example: shapes and colours, both the shape
#'   and colour scales should return compound labels.
#'
#' @inherit guide_legend_vanilla return
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
  sep              = "[^[:alnum:]]+",
  label_order      = c("row", "column"),

  h_label.theme    = element_text(),
  h_label.position = "right",

  v_label.theme    = element_text(angle = 90, vjust = 0.5),
  v_label.position = "bottom",

  reverse          = FALSE,

  ...

  ) {

  h_label.position <- arg_match0(
    h_label.position, c("left", "right")
  )
  v_label.position <- arg_match0(
    v_label.position, c("top", "bottom")
  )
  valid_order <- length(label_order) == 2L & is.character(label_order)
  valid_order <- valid_order & all(c("row", "column") %in% label_order)
  if (!valid_order) {
    abort(paste0(
      'The `label_order` argument should be either `c("row", "column")` ',
      'or `c("column", "row")`.'
    ))
  }

  guide_legend_vanilla(
    label_order      = label_order,
    h_label.theme    = h_label.theme,
    h_label.position = h_label.position,
    v_label.theme    = v_label.theme,
    v_label.position = v_label.position,
    sep              = sep,
    super            = GuideLegendCross,
    ...
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideLegendCross <- ggproto(
  "GuideLegendCross", GuideLegend,

  ## Method implementation ------------------------------------------------

  training_routine = function(self, scale, aesthetic = NULL) {

    breaks <- scale$get_breaks()
    if (length(breaks) == 0 || all(is.na(breaks))) {
      self$return_null <- TRUE
      return(invisible())
    }

    aesthetic       <- aesthetic %||% scale$aesthetics[1]
    names(self$key) <- c(aesthetic, ".value", "label")

    self$key <- self$train(scale, aesthetic)

    self$hash <- hash(list(
      self$title, self$params$sep, self$direction, self$name
    ))
    return(invisible())
  },

  train = function(self, scale, aesthetic) {
    sep <- self$params$sep
    if (is.null(sep)) {
      key <- ggproto_parent(GuideLegend, self)$train(scale, aesthetic)
      return(key)
    }

    breaks <- scale$get_breaks()
    label  <- scale$get_labels(breaks)

    # Split and index labels
    split_label <- split_labels(label, sep, snake_class(self))
    split_idx   <- lapply(split_label, function(x) {match(x, unique(x))})

    # Reverse grid if appropriate
    reverse <- rep_len(self$params$reverse, 2L)
    if (reverse[1]) {
      split_idx[[1]] <- max(split_idx[[1]]) - split_idx[[1]] + 1
    }
    if (reverse[2]) {
      split_idx[[2]] <- max(split_idx[[2]]) - split_idx[[2]] + 1
    }

    # Fill regular parts of key
    key <- new_data_frame(setNames(list(scale$map(breaks)), aesthetic))
    lab_ord <- paste0(".", self$params$label_order)
    key$.label <- label

    # Supplement split info in key
    key[, lab_ord] <- split_idx
    key[, paste0(lab_ord, "_label")] <- split_label

    # At this point, the die is cast and layout should be known
    self$params$ncol <- max(split_idx[[which(lab_ord == ".column")]])
    self$params$nrow <- max(split_idx[[which(lab_ord == ".row")]])

    if (!scale$is_discrete()) {
      limits  <- scale$get_limits()
      not_oob <- !is.na(oob_censor_any(breaks, limits))
      key     <- key[!not_oob, , drop = FALSE]
    }
    key
  },

  merging_routine = function(self, new_guide) {
    if (all(c(".row", ".column") %in% names(self$key))) {
      ggproto_parent(GuideLegend, self)$merging_routine(new_guide)
      return(invisible())
    }
    key1 <- self$key
    key2 <- new_guide$key

    grid <- expand.grid(left = seq_row(key1), right = seq_row(key2))

    left  <- vec_slice(key1, grid$left)
    right <- vec_slice(key2, grid$right)

    labels <- list(left = left$.label, right = right$.label)

    left$.label <- paste(labels$left, labels$right)
    right$.label <- NULL

    key <- cbind(left, right)
    lab_ord <- paste0(".", self$params$label_order)
    key[, lab_ord] <- as.list(grid)
    key[, paste0(lab_ord, "_label")] <- labels

    self$params$ncol <- max(key$.column)
    self$params$nrow <- max(key$.row)

    self$key <- key
    override <- merge_override(
      self$params$override.aes,
      new_guide$override.aes %||% new_guide$params$override.aes
    )
    self$params$override.aes <- override
    return(invisible())
  },

  geom = function(self, layers, ...) {
    key_nm <- names(self$key)
    required_cols <- c(".row", ".column", ".row_label", ".column_label")
    if (!(all(required_cols %in% key_nm))) {
      nullsep <- is.null(self$params$sep)
      if (nullsep) {
        abort(c(
          "Scale crossing has failed.",
          "i" = paste0("Using `sep = NULL` in `", snake_class(self), "()` ",
                       "requires exactly two scales with such guides, and ",
                       "identical titles.")
        ))
      } else {
        abort(c("Scale crossing has failed for unclear reasons."))
      }
    }
    ggproto_parent(GuideLegend, self)$geom(layers, ...)
  },

  ## Drawing helpers ------------------------------------------------------

  assemble_drawing = function(layout, sizes, title, labels, keys,
                              elements, params) {
    # Let regular legend take care of everything but labels
    gt <- GuideLegend$assemble_drawing(
      layout, sizes, title, NULL, keys, elements, params
    )
    layout <- layout$layout

    # Add row labels
    i <- which(!duplicated(layout$R))
    gt <- gtable_add_grob(
      gt,
      justify_grobs(labels$h, theme = elements$h_text),
      name = paste0("hlabel", layout$label_row[i], layout$alt_col[i],
                    sep = "-"),
      t = 1 + layout$label_row[i],
      r = 1 + layout$alt_col[i],
      b = 1 + layout$label_row[i],
      l = 1 + layout$alt_col[i]
    )

    # Add column labels
    i <- which(!duplicated(layout$C))
    gt <- gtable_add_grob(
      gt,
      justify_grobs(labels$v, theme = elements$v_text),
      name = paste0("vlabel", layout$alt_row[i], layout$label_col[i],
                    sep = "-"),
      t = 1 + layout$alt_row[i],
      r = 1 + layout$label_col[i],
      b = 1 + layout$alt_row[i],
      l = 1 + layout$label_col[i]
    )
    gt
  },

  setup_layout = function(params, key, sizes) {
    df <- setNames(key[, c(".row", ".column")], c("R", "C"))

    key_row <- label_row <- df$R * 2 - 1
    key_col <- label_col <- df$C * 2 - 1

    if (params$v_label.position == "top") {
      key_row   <- key_row   + 2
      label_row <- label_row + 2
      alt_row   <- min(key_row) - 2
    } else {
      alt_row   <- max(key_row) + 2
    }
    if (params$h_label.position == "left") {
      key_col   <- key_col   + 2
      label_col <- label_col + 2
      alt_col   <- min(key_col) - 2
    } else {
      alt_col   <- max(key_col) + 2
    }
    switch(
      params$title.position,
      "top" = {
        key_row   <- key_row   + 2
        label_row <- label_row + 2
        alt_row   <- alt_row   + 2
        title_row <- 1
        title_col <- 1:length(sizes$total$width)
      },
      "bottom" = {
        title_row <- length(sizes$total$height)
        title_col <- 1:length(sizes$total$width)
      },
      "left" = {
        key_col   <- key_col   + 2
        label_col <- label_col + 2
        alt_col   <- alt_col   + 2
        title_row <- 1:length(sizes$total$height)
        title_col <- 1
      },
      "right" = {
        title_row <- 1:length(sizes$total$height)
        title_col <- length(sizes$total$width)
      }
    )

    df <- cbind(
      df,
      key_row   = key_row,   key_col   = key_col,
      label_row = label_row, label_col = label_col,
      alt_row   = alt_row,   alt_col   = alt_col
    )
    list(layout = df, title_row = title_row, title_col = title_col)
  },

  build_labels =  function(elements, key) {
    temp_key  <- key[!duplicated(key$.row), ]
    temp_key$.label <- temp_key$.row_label
    temp_elem <- `[[<-`(elements, "text", elements$h_text)
    h_labels  <- GuideLegend$build_labels(temp_elem, temp_key)
    temp_key  <- key[!duplicated(key$.column), ]
    temp_key$.label <- temp_key$.column_label
    temp_elem <- `[[<-`(elements, "text", elements$v_text)
    v_labels  <- GuideLegend$build_labels(temp_elem, temp_key)
    list(h = h_labels, v = v_labels)
  },

  measure_parts = function(labels, title, geoms, params, elements) {

    byrow <- params$label_order[1] == "row"
    zeroes <- rep(0, params$nrow * params$ncol - params$n_breaks)

    # Measure keys
    key_size_mat <- lapply(geoms, `[[`, c("data", "size"))
    key_size_mat <- do.call("cbind", key_size_mat) / 10

    if (any(dim(key_size_mat) == 0)) {
      key_size_mat <- matrix(0, ncol = 1, nrow = params$n_breaks)
    }
    key_sizes <- apply(key_size_mat, 1, max)
    key_sizes <- matrix(
      c(key_sizes, zeroes),
      nrow = params$nrow, ncol = params$ncol, byrow = params$byrow
    )
    key_widths  <- pmax(elements$key_width,  apply(key_sizes, 2, max))
    key_heights <- pmax(elements$key_height, apply(key_sizes, 1, max))

    # Horizontal labels
    h_label_widths  <- max(width_cm(labels$h))
    h_label_heights <- height_cm(labels$h)

    # Vertical labels
    v_label_widths  <- width_cm(labels$v)
    v_label_heights <- max(height_cm(labels$v))

    # Combine to semitotals
    hgap <- elements$hgap
    vgap <- elements$vgap
    tgap <- elements$tgap

    h_position <- params$h_label.position
    v_position <- params$v_label.position

    widths  <- pmax(v_label_widths,  key_widths)
    heights <- pmax(h_label_heights, key_heights)

    if (h_position == "left") {
      semi_widths <- c(h_label_widths, tgap, trim_interleave(widths, hgap))
    } else {
      semi_widths <- c(trim_interleave(widths, hgap), tgap, h_label_widths)
    }
    if (v_position == "top") {
      semi_heights <- c(v_label_heights, tgap, trim_interleave(heights, vgap))
    } else {
      semi_heights <- c(trim_interleave(heights, vgap), tgap, v_label_heights)
    }

    # Combine width title sizes
    title_width  <- width_cm(title)
    title_height <- height_cm(title)

    position <- params$title.position
    widths <- switch(
      position,
      "left"  = c(title_width, tgap, semi_widths),
      "right" = c(semi_widths, tgap, title_width),
      c(semi_widths, max(0, title_width - sum(semi_widths)))
    )
    heights <- switch(
      position,
      "top"    = c(title_height, tgap, semi_heights),
      "bottom" = c(semi_heights, tgap, title_height),
      c(semi_heights, max(0, title_height - sum(semi_heights)))
    )

    ans <- list(
      total = list(width = widths, height = heights)
    )
  },

  setup_elements = function(theme, params) {

    # Title
    legend.title <- calc_element("legend.title", theme)
    title.theme  <- combine_elements(params$title.theme, legend.title)
    title.theme$hjust <- params$title.hjust %||% theme$legend.title.align %||%
      title.theme$hjust %||% 0
    title.theme$vjust <- params$title.vjust %||% title.theme$vjust %||% 0.5

    # Labels
    legend.text <- calc_element("legend.text", theme)
    hlabel.theme <- combine_elements(params$h_label.theme, legend.text)
    hlabel.theme <- set_text_just(
      hlabel.theme, params$h_label.position, params$h_label.theme, theme
    )
    vlabel.theme <- combine_elements(params$v_label.theme, legend.text)
    vlabel.theme <- set_text_just(
      vlabel.theme, params$v_label.position, params$v_label.theme, theme
    )

    # Keys
    key_width <- width_cm(
      params$keywidth %||% theme$legend.key.width %||% theme$legend.key.size
    )
    key_height <- height_cm(
      params$keyheight %||% theme$legend.key.height %||% theme$legend.key.size
    )
    key_background <- element_render(theme, "legend.key")

    # Background
    background <- element_render(theme, "legend.background")

    # Spacing
    size <- title.theme$size %||% legend.title$size %||%
      legend.text$size %||% 11
    size <- 0.5 * unit(size, "pt")
    hgap <- width_cm( theme$legend.spacing.x %||% size)
    vgap <- height_cm(theme$legend.spacing.y %||% size)
    tgap <- convertUnit(size, "cm", valueOnly = TRUE)
    padding <- convertUnit(theme$legend.margin %||% margin(),
                           "cm", valueOnly =  TRUE)

    list(
      title          = title.theme,
      h_text         = hlabel.theme,
      v_text         = vlabel.theme,
      key_background = key_background,
      key_width      = key_width,
      key_height     = key_height,
      background     = background,
      padding        = padding,
      hgap           = hgap,
      vgap           = vgap,
      tgap           = tgap
    )
  }
)

# Helpers -----------------------------------------------------------------

split_labels <- function(labels, split, fun_nm, expected = 2) {
  splitted <- strsplit(labels, split)
  lens <- lengths(splitted)
  msg <- character()
  if (any(lens < expected)) {
    splitted <- clapply(splitted, lens < expected, function(x) {
      c(x, rep("", expected - length(x)))
    })
    msg <- c("x" = paste0(
      "Splitting labels on '", split, "' has failed for the following",
      " labels:\n",
      glue_collapse(labels[lens < expected], sep = ", ", last = " and "), "."),
      "i" = "They have been padded with empty strings."
    )
  }
  if (any(lens > expected)) {
    msg <- c(msg, "x" = paste0(
      "Splitting labels on '", split, "' produced too many results for",
      " the following labels:\n",
      glue_collapse(labels[lens > expected], sep = ", ", last = " and "), "."),
      "i" = paste0("Only the first ", expected, " were taken.")
    )
  }
  if (length(msg)) {
    warn(c(paste0("In `", fun_nm, "()`:"), msg))
  }

  list(
    vapply(splitted, `[[`, character(1), 1),
    vapply(splitted, `[[`, character(1), 2)
  )

}
