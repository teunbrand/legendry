# Constructor -------------------------------------------------------------

guide_legend_base <- function(
  key = NULL,
  title = waiver(),
  theme = NULL,
  design = NULL,
  nrow = NULL,
  ncol = NULL,
  reverse = FALSE,
  override.aes = list(),
  position = NULL,
  direction = NULL,
  order = 0
) {

  check_position(position, allow_null = TRUE)
  check_argmatch(direction, c("horizontal", "vertical"), allow_null = TRUE)
  check_bool(reverse)
  check_number_whole(nrow, allow_null = TRUE)
  check_number_whole(ncol, allow_null = TRUE)

  design <- validate_design(design, allow_null = TRUE)
  if (!is.null(design)) {
    ignored <- c(
      if (!is.null(nrow)) "nrow",
      if (!is.null(ncol)) "ncol"
    )
    if (length(ignored) > 0) {
      cli::cli_warn(
        "The {.and {.arg {ignored}}} argument{?s} {?is/are} ignored \\
        when the {.arg design} argument is provided."
      )
    }
    nrow <- NULL
    ncol <- NULL
  }

  new_guide(
    key = key,
    title = title,
    design = design,
    nrow = nrow,
    ncol = ncol,
    override.aes = override.aes,
    reverse = reverse,
    theme = theme,
    position = position,
    direction = direction,
    order = order,
    super = GuideLegendBase
  )
}

# Class -------------------------------------------------------------------

GuideLegendBase <- ggproto(
  "GuideLegendBase", GuideLegend,

  params = new_params(
    override.aes = list(), reverse = FALSE,
    key = NULL, nrow = NULL, ncol = NULL, design = NULL
  ),

  extract_key = standard_extract_key,

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    # We ensure we know the 'byrow' setting from the beginning
    params$byrow <- params$theme$legend.byrow %||% theme$legend.byrow %||% FALSE
    ggproto_parent(Guide, self)$draw(
      theme = theme, position = position,
      direction = direction, params = params
    )
  },

  setup_params = function(params) {
    params$direction <- arg_match0(
      params$direction,
      c("horizontal", "vertical"),
      arg_nm = "direction"
    )
    params$n_breaks <- nrow(params$key)
    # We embed the design into the key as `.row`/`.col` columns
    params$key <- apply_design(
      params$key, params$design,
      params$nrow, params$ncol,
      params$direction, params$byrow
    )
    params
  },

  setup_elements = function(self, params, elements, theme) {
    # `guide_legend()` defaults to *no* vertical gaps for historical reasons
    # the following ensures theme hierarchy is observed.
    theme$legend.key.spacing.y <- theme$legend.key.spacing.y %||% rel(1)
    ggproto_parent(GuideLegend, self)$setup_elements(params, elements, theme)
  },

  build_decor = function(decor, grobs, elements, params) {
    decor <- render_legend_glyphs(
      index = seq_len(params$n_breaks),
      decor = decor, background = elements$key,
      default_size = c(elements$width_cm, elements$height_cm) * 10
    )
    decor <- decor[params$key$.index]
    names(decor) <- paste("key", params$key$.row, params$key$.col, sep = "-")
    decor
  },

  measure_grobs = function(grobs, params, elements) {

    # Get width of keys per column
    col <- params$key$.col
    widths <- map_dbl(grobs$decor, `[[`, i = "width")
    widths <- pmax(by_group(widths, col, max), elements$width_cm)

    # Weave in width of labels, depending on label position
    label_widths  <- by_group(width_cm(grobs$labels), col, max)
    widths <- switch(
      elements$text_position,
      left  = list(label_widths, widths),
      right = list(widths, label_widths),
      list(pmax(label_widths, widths))
    )
    widths <- vec_interleave(!!!widths, elements$spacing_x %||% 0)
    widths <- widths[-length(widths)] # Remove last spacer

    # Get height of keys per row
    row <- params$key$.row
    heights <- map_dbl(grobs$decor, `[[`, i = "height")
    heights <- pmax(by_group(heights, row, max), elements$height_cm)

    # Weave in height of labels, depending on label position
    label_heights <- by_group(height_cm(grobs$labels), row, max)
    heights <- switch(
      elements$text_position,
      top    = list(label_heights, heights),
      bottom = list(heights, label_heights),
      list(pmax(label_heights, heights))
    )
    heights <- vec_interleave(!!!heights, elements$spacing_y %||% 0)
    heights <- heights[-length(heights)] # Remove last spacer

    list(widths = widths, heights = heights)
  },

  arrange_layout = function(key, sizes, params, elements) {

    row <- key$.row
    col <- key$.col

    # Account for spacing in between keys
    key_row <- row * 2 - 1
    key_col <- col * 2 - 1

    # Resolve position of labels relative to keys
    position <- elements$text_position
    key_row <- key_row + switch(position, top  = row, bottom = row - 1, 0)
    lab_row <- key_row + switch(position, top  = -1,  bottom = 1,       0)
    key_col <- key_col + switch(position, left = col, right  = col - 1, 0)
    lab_col <- key_col + switch(position, left = -1,  right  = 1,       0)

    data_frame0(
      key_row = key_row,
      key_col = key_col,
      label_row = lab_row,
      label_col = lab_col
    )
  }
)

# Helpers -----------------------------------------------------------------

render_legend_glyphs <- function(index, decor, background, default_size) {
  lapply(index, function(i) {
    glyphs <- lapply(decor, function(dec) {
      data <- vec_slice(dec$data, i)
      if (!(data$.draw %||% TRUE)) {
        return(zeroGrob())
      }
      key <- dec$draw_key(data, dec$params, default_size)
      set_key_size(key, data$linewidth, data$size, default_size / 10)
    })
    gTree(
      width    = max(map_dbl(glyphs, attr, which = "width"),  0,  na.rm = TRUE),
      height   = max(map_dbl(glyphs, attr, which = "height"), 0,  na.rm = TRUE),
      children = inject(gList(background, !!!glyphs))
    )
  })
}

set_key_size <- function(key, lwd = NULL, size = NULL, default = NULL) {
  width  <- attr(key, "width")
  height <- attr(key, "height")
  if (!is.null(width) && !is.null(height)) {
    return(key)
  }
  if (!is.null(size) || !is.null(lwd)) {
    size <- size[1] %||% 0 %|NA|% 0
    lwd  <- lwd[1]  %||% 0 %|NA|% 0
    size <- (size + lwd) / 10
  } else {
    size <- NULL
  }
  attr(key, "width")  <- width  %||% size %||% default[1]
  attr(key, "height") <- height %||% size %||% default[2]
  key
}

apply_design <- function(
  key, design = NULL, nrow = NULL, ncol = NULL,
  direction = "horizontal", byrow = FALSE
) {
  n_breaks <- nrow(key)

  # Handle case where there is no design, à la ggplot2::guide_legend
  if (is.null(design)) {
    if (is.null(nrow) && is.null(ncol)) {
      if (direction == "horizontal") {
        nrow <- ceiling(n_breaks / 5)
      } else {
        ncol <- ceiling(n_breaks / 20)
      }
    }
    nrow <- nrow %||% ceiling(n_breaks / ncol)
    ncol <- ncol %||% ceiling(n_breaks / nrow)

    design <- matrix(
      seq_len(nrow * ncol),
      nrow = nrow, ncol = ncol,
      byrow = byrow
    )
  }

  max_design <- max(design, na.rm = TRUE)
  if (isTRUE(max_design < n_breaks)) {
    cause <- if (is.null(design)) {
      "{.arg nrow} * {.arg ncol} ({nrow * ncol}) is insufficient "
    } else {
      "The {.arg design} argument has insufficient levels "
    }
    cli::cli_warn(
      paste0(cause, "to accommodate the number of breaks ({n_breaks}).")
    )
  }

  key$.index <- seq_len(nrow(key))

  index <- match(design, key$.index)
  rows <- as.vector(row(design))
  cols <- as.vector(col(design))

  key <- vec_slice(key, index)
  key$.row <- rows
  key$.col <- cols
  vec_slice(key, !is.na(key$.index))
}

validate_design <- function(design = NULL, trim = TRUE, allow_null = TRUE) {
  if (is.null(design)) {
    if (allow_null) {
      return(NULL)
    }
    cli::cli_abort("The {.arg design} argument cannot be {.code NULL}.")
  }
  design <- parse_design_character(design)
  if (!is.matrix(design) && is.atomic(design)) {
    design <- as.matrix(design)
  }
  check_matrix(design)
  if (typeof(design) == "character") {
    design[design == "#"] <- NA
  }
  levels <- unique(sort(design))
  design <- matrix(
    match(design, levels),
    nrow = nrow(design),
    ncol = ncol(design)
  )

  if (trim) {
    filled <- !is.na(design)
    design <- design[rowSums(filled) > 0, colSums(filled) > 0, drop = FALSE]
  }

  if (!is.numeric(levels)) {
    attr(design, "levels") <- levels
  }

  design
}

parse_design_character <- function(design, call = caller_env()) {

  if (!is.character(design)) {
    return(design)
  }

  # Check is here to ensure scalar character
  check_string(design, allow_empty = FALSE, call = call)

  # Inspired by patchwork::as_areas()
  design <- trimws(strsplit(design, "\n")[[1]])
  design <- strsplit(design[nzchar(design)], "")

  nrow <- length(design)
  ncol <- lengths(design)
  if (length(unique(ncol)) != 1L) {
    cli::cli_abort(
      "The {.arg design} argument must be rectangular.",
      call = call
    )
  }

  matrix(
    unlist(design, FALSE, FALSE),
    nrow = nrow, ncol = ncol[1], byrow = TRUE
  )
}
