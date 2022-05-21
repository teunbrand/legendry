# Public constructor ------------------------------------------------------

#' Vanilla legend guide
#'
#' This is mostly a re-implementation of
#'   [`guide_legend()`][ggplot2::guide_legend()] with the following
#'   noticeable changes:
#' * `legend.spacing.{xy}` is applied, regardless of the `byrow` argument.
#' * The default alignment of expressions is left-aligned instead of
#'   right-aligned.
#'
#' @inheritParams ggplot2::guide_legend
#'
#' @return A `Guide` ggproto object.
#' @export
#'
#' @examples
#' # Works in the same way as `guide_legend`.
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = as.factor(cyl))) +
#'   guides(colour = guide_legend_vanilla(nrow = 2))
guide_legend_vanilla <- function(

  # Title
  title          = waiver(),
  title.position = NULL,
  title.theme    = NULL,
  title.hjust    = NULL,
  title.vjust    = NULL,

  # Label
  label          = TRUE,
  label.position = NULL,
  label.theme    = NULL,
  label.hjust    = NULL,
  label.vjust    = NULL,

  # Key
  keywidth  = NULL,
  keyheight = NULL,

  # General
  direction    = NULL,
  default.unit = "lines",
  override.aes = list(),
  nrow    = NULL,
  ncol    = NULL,
  byrow   = FALSE,
  reverse = FALSE,
  order   = 0,
  ...
  ) {

  construct_legend(
    title          = title,
    title.position = title.position,
    title.theme    = title.theme,
    title.hjust    = title.hjust,
    title.vjust    = title.vjust,

    # Label
    label          = label,
    label.position = label.position,
    label.theme    = label.theme,
    label.hjust    = label.hjust,
    label.vjust    = label.vjust,

    # Key
    keywidth  = keywidth,
    keyheight = keyheight,

    # General
    direction    = direction,
    default.unit = default.unit,
    override.aes = override.aes,
    nrow    = nrow,
    ncol    = ncol,
    byrow   = byrow,
    reverse = reverse,
    order   = 0,
    ...
  )

}

# Internal constructor ----------------------------------------------------

#' @keywords internal
construct_legend <- function(
  ...,
  available_aes  = "any",
  keywidth       = NULL,
  keyheight      = NULL,
  title.position = NULL,
  label.position = NULL,
  direction      = NULL,
  name           = "legend",
  default.unit   = "lines",
  order          = 0,
  super          = GuideLegend
) {

  keywidth  <- as_unit(keywidth,  default.unit)
  keyheight <- as_unit(keyheight, default.unit)

  if (!is.null(title.position)) {
    title.position <- arg_match0(
      title.position,
      c("top", "bottom", "left", "right"),
      "title.position"
    )
  }

  if (!is.null(label.position)) {
    label.position <- arg_match0(
      label.position,
      c("top", "bottom", "left", "right"),
      "label.position"
    )
  }

  guide <- construct_guide(
    available_aes  = available_aes,
    name           = name,
    keywidth       = keywidth,
    keyheight      = keyheight,
    label.position = label.position,
    super          = super,
    ...
  )
  guide$title.position <- title.position
  guide$order     <- order
  guide$direction <- direction

  guide

}

# Class -------------------------------------------------------------------

GuideLegend <- ggproto(
  "GuideLegend", Guide,

  ## Guide attributes -----------------------------------------------------

  direction      = NULL,
  order          = 0,
  title.position = NULL, # Not ideal but works
  return_null    = NULL,
  geoms          = list(),

  ## Method implementation ------------------------------------------------

  training_routine = function(self, scale, aesthetic = NULL) {

    breaks <- scale$get_breaks()
    if (length(breaks) == 0 || all(is.na(breaks))) {
      self$return_null <- TRUE
      return(invisible())
    }

    aesthetic       <- aesthetic %||% scale$aesthetics[1]
    names(self$key) <- c(aesthetic, ".value", "label")

    self$train(scale, aesthetic)

    self$hash <- hash(list(
      self$title, self$key$.label, self$direction, self$name
    ))
    return(invisible())
  },

  train = function(self, scale, aesthetic) {
    breaks <- scale$get_breaks()

    key <- new_data_frame(setNames(list(scale$map(breaks)), aesthetic))
    key$.label <- scale$get_labels(breaks)

    if (!scale$is_discrete()) {
      limits  <- scale$get_limits()
      not_oob <- !is.na(oob_censor_any(breaks, limits))
      key     <- vec_slice(key, not_oob)
    }

    if (self$params$reverse) {
      key <- vec_slice(key, rev(seq_row(key)))
    }
    self$key <- key
    return(invisible())
  },

  scan_geoms = function(self, layers, default_mapping) {
    geoms <- self$geom(layers, default_mapping)
    geoms <- geoms[!vapply(geoms, is.null, logical(1))]

    if (length(geoms) == 0) {
      self$return_null <- TRUE
    } else {
      self$geoms <- geoms
    }
    return(invisible())
  },

  geom = function(self, layers, ...) {
    key <- self$key
    key_names <- names(key)
    lapply(layers, function(layer) {
      matched <- match_aes(layer, key_names)
      include <- include_layer_in_guide(layer, matched)

      if (!include) {
        return(NULL)
      }

      if (length(matched) > 0) {
        n <- lengths(layer$aes_params)
        params <- layer$aes_params[n == 1]

        aesthetics <- layer$computed_mapping
        modifiers  <- aesthetics[is_scaled_or_staged_aes(aesthetics)]

        data <- tryCatch(
          layer$geom$use_defaults(key[matched], params, modifiers),
          error = function(...) {
            warn(paste0(
              "Failed to apply `after_scale()` modifications to ",
              snake_class(self), "."
            ))
            layer$geom$use_defaults(guide$key[matched], params, list())
          }
        )
      } else {
        data <- layer$geom$use_defaults(NULL, layer$aes_params)
        data <- data[rep(1, nrow(key)), ]
      }

      data <- modify_list(data, self$params$override.aes)

      if (!is.null(data$size)) {
        data$size[is.na(data$size)] <- 0
      }

      list(
        draw_key = layer$geom$draw_key,
        data     = data,
        params   = c(layer$computed_geom_params, layer$computed_stat_params)
      )
    })
  },

  merging_routine = function(self, new_guide) {
    new_guide$key$.label <- NULL
    self$key <- cbind(self$key, new_guide$key)

    override <- self$params$override.aes
    override <- c(
      override,
      new_guide$override.aes %||% new_guide$params$override.aes
    )
    if (anyDuplicated(names(override))) {
      warn("Duplicated override.aes is ignored.")
    }

    override <- override[!duplicated(names(override))]
    self$params$override.aes <- override
    return(invisible())
  },

  draw_guide = function(self, theme) {
    key    <- self$key
    params <- self$params
    params$direction <- self$direction
    params$title.position <- self$title.position

    params   <- self$setup_params(key, params, self$geoms)
    elements <- self$setup_elements(theme, params)

    title  <- self$build_title(elements, self$title)
    labels <- self$build_labels(elements, key)
    keys   <- self$build_keys(elements, self$geoms, params)

    sizes  <- self$measure_parts(labels, title, self$geoms, params, elements)

    layout <- self$setup_layout(params, sizes)

    self$assemble_drawing(
      layout, sizes, title, labels, keys, elements, params
    )
  },

  ## Drawing helpers ------------------------------------------------------

  assemble_drawing = function(layout, sizes, title, labels, keys,
                              elements, params) {
    padding <- elements$padding
    k_cols  <- rep(layout$layout$key_col, each = params$n_geom)
    k_rows  <- rep(layout$layout$key_row, each = params$n_geom)

    gt <- gtable(
      widths  = unit(c(padding[4], sizes$total$width,  padding[2]), "cm"),
      heights = unit(c(padding[1], sizes$total$height, padding[3]), "cm")
    )
    gt <- gtable_add_grob(
      gt,
      elements$background,
      name = "background",
      clip = "off",
      t = 1, r = -1, b = -1, l = 1
    )
    gt <- gtable_add_grob(
      gt,
      justify_grobs(title, theme = elements$title),
      name = "title",
      clip = "off",
      t = 1 + min(layout$title_row),
      r = 1 + max(layout$title_col),
      b = 1 + max(layout$title_row),
      l = 1 + min(layout$title_col)
    )
    layout <- layout$layout
    gt <- gtable_add_grob(
      gt,
      keys,
      name = paste("key", k_rows, k_cols,
                   c("bg", seq(params$n_geom)), sep = "-"),
      clip = "off",
      t = 1 + k_rows,
      r = 1 + k_cols,
      b = 1 + k_rows,
      l = 1 + k_cols
    )
    gt <- gtable_add_grob(
      gt,
      justify_grobs(labels, theme = elements$text),
      name = paste("label", layout$label_row, layout$label_col, sep = "-"),
      clip = "off",
      t = 1 + layout$label_row,
      r = 1 + layout$label_col,
      b = 1 + layout$label_row,
      l = 1 + layout$label_col
    )
    gt
  },

  setup_layout = function(params, sizes) {
    break_seq <- seq(params$n_breaks)
    if (params$byrow) {
      df <- new_data_frame(
        list(
          R = ceiling(break_seq / params$ncol),
          C = (break_seq - 1) %% params$ncol + 1
        )
      )
    } else {
      dim <- c(length(sizes$key$height), length(sizes$key$width))
      df  <- arrayInd(break_seq, dim)
      df  <- setNames(lapply(seq_col(df), function(i) df[, i]), c("R", "C"))
      df  <- new_data_frame(df)
    }
    key_row <- label_row <- df$R
    key_col <- label_col <- df$C

    switch(
      params$label.position,
      "top" = {
        key_row   <- key_row   * 4 - 1
        key_col   <- key_col   * 2 - 1
        label_row <- label_row * 4 - 3
        label_col <- label_col * 2 - 1
      },
      "bottom" = {
        key_row   <- key_row   * 4 - 3
        key_col   <- key_col   * 2 - 1
        label_row <- label_row * 4 - 1
        label_col <- label_col * 2 - 1
      },
      "left" = {
        key_row   <- key_row   * 2 - 1
        key_col   <- key_col   * 4 - 1
        label_row <- label_row * 2 - 1
        label_col <- label_col * 4 - 3
      },
      "right" = {
        key_row   <- key_row   * 2 - 1
        key_col   <- key_col   * 4 - 3
        label_row <- label_row * 2 - 1
        label_col <- label_col * 4 - 1
      }
    )

    switch(
      params$title.position,
      "top" = {
        key_row   <- key_row + 2
        label_row <- label_row + 2
        title_row <- 1
        title_col <- 1:length(sizes$total$width)
      },
      "bottom" = {
        title_row <- length(sizes$total$height)
        title_col <- 1:length(sizes$total$width)
      },
      "left" = {
        key_col   <- key_col + 2
        label_col <- label_col + 2
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
      label_row = label_row, label_col = label_col
    )
    list(layout = df, title_row = title_row, title_col = title_col)
  },

  measure_parts = function(labels, title, geoms, params, elements) {
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

    # Measure labels
    label_widths  <- width_cm(labels)
    label_heights <- height_cm(labels)

    label_widths <- matrix(
      c(label_widths, zeroes),
      nrow = params$nrow, ncol = params$ncol, byrow = params$byrow
    )
    label_heights <- matrix(
      c(label_heights, zeroes),
      nrow = params$nrow, ncol = params$ncol, byrow = params$byrow
    )
    label_widths  <- apply(label_widths,  2, max)
    label_heights <- apply(label_heights, 1, max)

    # Combine to semitotals
    hgap <- elements$hgap
    vgap <- elements$vgap
    tgap <- elements$tgap
    position <- params$label.position
    semi_widths <- switch(
      position,
      "top"    = trim_interleave(pmax(label_widths, key_widths), hgap),
      "bottom" = trim_interleave(pmax(label_widths, key_widths), hgap),
      "left"   = trim_interleave(label_widths, tgap, key_widths, hgap),
      "right"  = trim_interleave(key_widths, tgap, label_widths, hgap)
    )
    semi_heights <- switch(
      position,
      "top"    = trim_interleave(label_heights, tgap, key_heights, vgap),
      "bottom" = trim_interleave(key_heights, tgap, label_heights, vgap),
      "left"   = trim_interleave(pmax(label_heights, key_heights), vgap),
      "right"  = trim_interleave(pmax(label_heights, key_heights), vgap)
    )

    # Combine with title sizes
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
      key   = list(width = key_widths,   height = key_heights),
      label = list(width = label_widths, height = label_heights),
      semi  = list(width = semi_widths,  height = semi_heights),
      title = list(width = title_height, height = title_width),
      total = list(width = widths,       height = heights)
    )
  },

  build_keys = function(elements, geoms, params) {

    key_size <- c(elements$key_width, elements$key_height) * 10

    draw <- function(i) {
      bg <- elements$key_background
      keys <- lapply(geoms, function(g) {
        g$draw_key(g$data[i, , drop = FALSE], g$params, key_size)
      })
      c(list(bg), keys)
    }
    unlist(lapply(seq_len(params$n_breaks), draw), FALSE)
  },

  build_labels = function(elements, key) {
    lapply(key$.label, function(lab) {
      grob <- element_grob(
        elements$text,
        label    = lab,
        margin_x = TRUE,
        margin_y = TRUE
      )
      grob$name <- grobName(grob, "guide.label")
      grob
    })
  },

  build_title = function(elements, label) {
    grob <- element_grob(
      elements$title,
      label    = label,
      margin_x = TRUE,
      margin_y = TRUE
    )
    grob$name <- grobName(grob, "guide.title")
    grob
  },

  setup_params = function(self, key, params, geoms) {
    params$direction <- arg_match0(
      self$direction,
      c("horizontal", "vertical"),
      arg_nm = "direction"
    )

    params$title.position <- arg_match0(
      self$title.position,
      c("top", "bottom", "left", 'right'),
      arg_nm = "title.position"
    )

    params$label.position <- params$label.position %||% "right"
    params$label.position <- arg_match0(
      params$label.position,
      c("top", "bottom", "left", "right"),
      arg_nm = "label.position"
    )
    params$n_geom <- length(geoms) + 1

    # Setup rows / columns
    params$n_breaks <- n_breaks <- nrow(key)
    if (!is.null(params$nrow) && !is.null(params$ncol) &&
        params$nrow * params$ncol < n_breaks) {
      abort("`nrow` * `ncol` needs to be larger than the number of breaks.")
    }
    if (is.null(params$nrow) && is.null(params$ncol)) {
      if (params$direction == "horizontal") {
        params$nrow <- ceiling(n_breaks / 5)
      } else {
        params$ncol <- ceiling(n_breaks / 20)
      }
    }
    params$nrow <- params$nrow %||% ceiling(n_breaks / params$ncol)
    params$ncol <- params$ncol %||% ceiling(n_breaks / params$nrow)

    params
  },

  setup_elements = function(theme, params) {

    # Title
    legend.title <- calc_element("legend.title", theme)
    title.theme <- params$title.theme %||% legend.title
    title.theme$hjust <- params$title.hjust %||% theme$legend.title.align %||%
      title.theme$hjust %||% 0
    title.theme$vjust <- params$title.vjust %||% title.theme$vjust %||% 0.5

    # Labels
    label.direction <- arg_match0(params$label.position,
                                 c("top", "bottom", "left", "right"))

    elem_text   <- calc_element("legend.text", theme)
    label.theme <- params$label.theme %||% elem_text
    if (params$label) {
      # TODO: Mention omission of right-justified expressions
      just_defaults <- .legend_just_defaults[[label.direction]]
      if (is.null(params$label.theme$hjust) &&
          is.null(theme$legend.text$hjust)) {
        label.theme$hjust <- NULL
      }
      if (is.null(params$label.theme$vjust) &&
          is.null(theme$legend.text$vjust)) {
        label.theme$vjust <- NULL
      }
      hjust <- params$label.hjust %||% theme$legend.text.align %||%
        label.theme$hjust %||% just_defaults$hjust
      vjust <- params$label.vjust %||% label.theme$vjust %||%
        just_defaults$vjust
      label.theme$hjust <- hjust
      label.theme$vjust <- vjust
    }

    # Keys
    key_width  <- width_cm(
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
      elem_text$size %||% 11
    size <- 0.5 * unit(size, "pt")
    hgap <- width_cm(theme$legend.spacing.x  %||% size)
    vgap <- height_cm(theme$legend.spacing.y %||% size)
    tgap <- convertUnit(size, "cm", valueOnly = TRUE)
    padding <- convertUnit(theme$legend.margin %||% margin(),
                           "cm", valueOnly =  TRUE)

    list(
      title          = title.theme,
      text           = label.theme,
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

trim_interleave <- function(..., .n = -1) {
  head(vec_interleave(...), n = .n)
}

match_aes <- function(layer, key_names) {
  all     <- names(c(layer$computed_mapping, layer$stat$default_aes))
  geom    <- c(layer$geom$required_aes, names(layer$geom$default_aes))
  matched <- intersect(intersect(all, geom), key_names)
  matched <- setdiff(matched, names(layer$computed_geom_params))
  setdiff(matched, names(layer$aes_params))
}

include_layer_in_guide <- function(layer, matched) {
  show_legend <- layer$show.legend
  if (!is.logical(show_legend)) {
    warn("`show.legend` must be a logical vector.")
    layer$show.legend <- FALSE
    return(FALSE)
  }

  if (length(matched) > 0) {
    if (is_named(show_legend)) {
      layer$show.legend <- ggplot2:::rename_aes(show_legend)
      show_legend <- show_legend[matched]
      show_legend <- show_legend[!is.na(show_legend)]
      return(length(show_legend) == 0 || any(show_legend))
    }
    return(all(is.na(show_legend)) || isTRUE(show_legend))
  }
  isTRUE(show_legend)
}

is_scaled_or_staged_aes <- function(aesthetics) {
  vapply(aesthetics, function(aes) {
    is_call(get_expr(aes), c("stage", "after_scale"))
  }, logical(1), USE.NAMES = FALSE)
}

.legend_just_defaults <- list(
  "top"    = list(hjust = 0.5, vjust = 0),
  "bottom" = list(hjust = 0.5, vjust = 1),
  "left"   = list(hjust = 1,   vjust = 0.5),
  "right"  = list(hjust = 0,   vjust = 0.5)
)
