# Public constructor ------------------------------------------------------

#' Vanilla continuous colour bar guide
#'
#' This is mostly a re-implementation of
#' [`guide_colourbar()`][ggplot2::guide_colourbar] with the following noticeable
#' changes:
#' * The length of ticks can be controlled by the `ticks.length` argument.
#' * The ticks and frame are now specified by elements, instead of separate
#'   arguments controlling various aspects of their appearance.
#'
#' @inheritParams ggplot2::guide_colourbar
#' @param frame An [`<element_rect>`][ggplot2::element_rect()] or
#'   `<element_blank>` (default) object controlling the appearance of the frame
#'   drawn around the bar. If not `<element_blank>`, a missing `size` is set
#'   to 0.5 mm, a missing `linetype` is set to 1 and a missing `fill` is set
#'   to `NA`. Thereafter, inherits from `rect` in the plot's theme.
#' @param ticks An [`<element_line>`][ggplot2::element_line()] (default) or
#'   `<element_blank>` object controlling the appearance of the tick marks on
#'   the bar. If not `<element_blank>`, a missing `size` is set to 0.5 mm,
#'   a missing `colour` is set to `"white"` and a missing `lineend` is set to
#'   `"butt"`. Thereafter, inherits from `line` in the plot's theme.
#' @param ticks.length A [`<unit>`][grid::unit()] object of length 1 or 2
#'   (internally recycled to 2), setting how long the tick marks should be. The
#'   first element controls the length of the bottom or left ticks, whereas the
#'   second element does that for the top or right ticks. Can be negative units
#'   to place ticks exterior to the bar. The default is 1/5th the size of the
#'   bar.
#'
#' @return A `<Guide>` ggproto object that can be given to the
#'   [`guides()`][ggplot2::guides()] function, or set as the `guide` argument
#'   in a colour or fill scale.
#' @export
#' @family vanilla guides
#'
#' @examples
#' # Mostly works in the same way as `guide_colourbar()`
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = cty)) +
#'   guides(colour = guide_colourbar_vanilla(
#'     ticks = element_line(colour = "black"), # Change ticks appearance
#'     frame = element_rect(colour = "black"), # Change frame appearance
#'     ticks.length = unit(c(0.5, -0.2), "npc"), # Vary tick mark length
#'   ))
guide_colourbar_vanilla <- function(

  # Title
  title           = waiver(),
  title.position  = NULL,
  title.theme     = NULL,
  title.hjust     = NULL,
  title.vjust     = NULL,

  # Label
  label           = TRUE,
  label.position  = NULL,
  label.theme     = NULL,
  label.hjust     = NULL,
  label.vjust     = NULL,

  # Bar
  barwidth        = NULL,
  barheight       = NULL,
  nbin            = 300,
  raster          = TRUE,

  # Frame
  frame           = element_blank(),

  # Ticks
  ticks           = element_line(),
  ticks.length    = unit(1/5, "npc"),
  draw.ulim       = TRUE,
  draw.llim       = TRUE,

  # General
  direction       = NULL,
  default.unit    = "line",
  reverse         = FALSE,
  order           = 0,
  available_aes   = c("colour", "color", "fill"),
  ...
) {
  construct_colourbar(
    # Title
    title           = title,
    title.position  = title.position,
    title.theme     = title.theme,
    title.hjust     = title.hjust,
    title.vjust     = title.vjust,

    # Label
    label           = label,
    label.position  = label.position,
    label.theme     = label.theme,
    label.hjust     = label.hjust,
    label.vjust     = label.vjust,

    # Bar
    barwidth        = barwidth,
    barheight       = barheight,
    nbin            = nbin,
    raster          = raster,

    # Frame
    frame           = frame,

    # Ticks
    ticks           = ticks,
    ticks.length    = ticks.length,
    draw.ulim       = draw.ulim,
    draw.llim       = draw.llim,

    # General
    direction       = direction,
    default.unit    = default.unit,
    reverse         = reverse,
    order           = order,
    available_aes   = available_aes,
    ...
  )
}

# Internal Constructor ----------------------------------------------------

construct_colourbar <- function(
  ...,
  available_aes  = c("colour", "color", "fill"),
  barwidth       = NULL,
  barheight      = NULL,
  title.position = NULL,
  label.position = NULL,
  direction      = NULL,
  frame          = element_blank(),
  ticks          = element_line(),
  ticks.length   = unit(1/5, "npc"),
  name           = "colourbar",
  default.unit   = "lines",
  order          = 0,
  super          = GuideColourbar
) {

  barwidth     <- as_unit(barwidth,     default.unit)
  barheight    <- as_unit(barheight,    default.unit)

  # Make sure ticks.length is a length 2 unit object
  ticks.length <- as_unit(ticks.length, default.unit)
  ticks.length <- arg_class(ticks.length, "unit")
  if (length(ticks.length) != 2) {
    ticks.length <- rep(ticks.length, length.out = 2)
  }

  # Ensure ticks is appropriate element and set defaults
  ticks <- arg_class(ticks, c("element_line", "element_blank"))
  ticks <- combine_elements(
    ticks,
    element_line(
      colour  = "white",
      size    = 0.5 / .pt,
      lineend = "butt"
    )
  )

  # Ensure frame is appropriate element and set defaults
  frame <- arg_class(frame, c("element_rect", "element_blank"))
  frame <- combine_elements(
    frame,
    element_rect(
      size     = 0.5 / .pt,
      linetype = 1,
      fill     = NA
    )
  )

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
    barwidth       = barwidth,
    barheight      = barheight,
    label.position = label.position,
    frame          = frame,
    ticks          = ticks,
    ticks.length   = ticks.length,
    super          = super,
    ...
  )
  guide$title.position <- title.position
  guide$direction <- direction
  guide$order     <- order

  guide
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideColourbar <- ggproto(
  "GuideColourbar", Guide,

  ## Guide attributes -----------------------------------------------------

  direction      = NULL,
  order          = 0,
  title.position = NULL,
  return_null    = NULL,
  bar            = NULL,

  ## Method implementation ------------------------------------------------

  training_routine = function(self, scale, aesthetic = NULL) {

    if (length(intersect(scale$aesthetics, self$available_aes)) < 1) {
      warn(paste0(snake_class(self), "needs appropriate scales: ",
                  glue_collapse(self$available_aes, sep = ", ", last = " or ")))
      self$return_null <- TRUE
      return(invisible())
    }
    if (scale$is_discrete()) {
      warn(paste0(snake_class(self), "needs continuous scales."))
      self$return_null <- TRUE
      return(invisible())
    }

    breaks <- scale$get_breaks()
    if (length(breaks) < 1 || all(is.na(breaks))) {
      self$return_null <- TRUE
      return(invisible())
    }

    aesthetic <- aesthetic %||% scale$aesthetics[1]

    trained  <- self$train(scale, aesthetic)
    self$key <- trained$key
    self$bar <- trained$bar

    self$hash <- hash(list(self$title, self$key$.label, self$bar, self$name))
    return(invisible())
  },

  train = function(self, scale, aesthetic = NULL) {
    breaks <- scale$get_breaks()

    # Make key
    key  <- new_data_frame(setNames(list(scale$map(breaks)), aesthetic))
    key$.value <- breaks
    key$.label <- scale$get_labels(breaks)

    # Make bar
    limits <- scale$get_limits()
    bar    <- seq(limits[1], limits[2], length.out = self$params$nbin %||% 300)
    if (length(bar) == 0) {
      bar <- unique(limits)
    }
    bar <- new_data_frame(list(colour = scale$map(bar), value = bar),
                          n = length(bar))
    if (isTRUE(self$params$reverse)) {
      key <- key[nrow(key):1, ]
      bar   <- bar[nrow(bar):1, ]
    }
    list(key = key, bar = bar)
  },

  scan_geoms = function(self, layers, default_mapping) {
    geoms <- self$geom(layers, default_mapping)
    geoms <- geoms[!vapply(geoms, is.null, logical(1))]

    if (length(geoms) == 0) {
      self$return_null <- TRUE
    }
    return(invisible())
  },

  geom = function(self, layers, ...) {
     key <- self$key
     key_names <- names(key)
     lapply(layers, function(layer) {
       matched <- match_aes(layer, key_names)

       if (length(matched) == 0) {
         return(NULL)
       }

       if (include_layer_in_guide(layer, matched)) {
         layer
       } else {
         NULL
       }
     })
  },

  draw_guide = function(self, theme) {
    key    <- self$key
    bar    <- self$bar
    params <- self$params

    params   <- self$setup_params(key, params)
    elements <- self$setup_elements(theme, params)

    title  <- self$build_title(elements, self$title)
    labels <- self$build_labels(elements, key, params)
    ticks  <- self$build_ticks(elements, params)
    bar    <- self$build_bar(elements, bar, params)

    sizes  <- self$measure_parts(labels, title, params, elements)
    layout <- self$setup_layout(elements, params, sizes)

    self$assemble_drawing(
      layout, sizes, title, labels, bar, ticks, elements, params
    )
  },

  ## Drawing helpers ------------------------------------------------------

  assemble_drawing = function(
    layout, sizes, title, labels, bar, ticks, elements, params
  ) {
    padding <- elements$padding

    gt <- gtable(
      widths  = unit(c(padding[4], sizes$widths,  padding[2]), "cm"),
      heights = unit(c(padding[1], sizes$heights, padding[3]), "cm")
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
      bar,
      name = "bar",
      clip = "off",
      t = 1 + min(layout$bar_row),
      r = 1 + max(layout$bar_col),
      b = 1 + max(layout$bar_row),
      l = 1 + min(layout$bar_col)
    )
    gt <- gtable_add_grob(
      gt,
      labels,
      name = "label",
      clip = "off",
      t = 1 + min(layout$label_row),
      r = 1 + max(layout$label_col),
      b = 1 + max(layout$label_row),
      l = 1 + min(layout$label_col)
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
    gt <- gtable_add_grob(
      gt,
      ticks,
      name = "ticks",
      clip = "off",
      t = 1 + min(layout$bar_row),
      r = 1 + max(layout$bar_col),
      b = 1 + max(layout$bar_row),
      l = 1 + min(layout$bar_col)
    )
    gt
  },

  setup_layout = function(elements, params, sizes) {
    position  <- params$label.position
    bar_row   <- 1 + 2 * (position == "top")
    bar_col   <- 1 + 2 * (position == "left")
    label_row <- 1 + 2 * (position == "bottom")
    label_col <- 1 + 2 * (position == "right")

    ncol <- length(sizes$widths)
    nrow <- length(sizes$heights)

    switch(
      params$title.position,
      "top" = {
        bar_row   <- bar_row   + 2
        label_row <- label_row + 2
        title_row <- 1
        title_col <- seq_len(ncol)
      },
      "bottom" = {
        title_row <- nrow
        title_col <- seq_len(ncol)
      },
      "left" = {
        bar_col   <- bar_col   + 2
        label_col <- label_col + 2
        title_row <- seq_len(nrow)
        title_col <- 1
      },
      "right" = {
        title_row <- seq_len(nrow)
        title_col <- ncol
      }
    )

    list(
      bar_row   = bar_row,
      bar_col   = bar_col,
      label_row = label_row,
      label_col = label_col,
      title_row = title_row,
      title_col = title_col
    )
  },

  measure_parts = function(labels, title, params, elements) {
    label_width  <- width_cm(labels)
    label_height <- height_cm(labels)

    title_width  <- width_cm(title)
    title_height <- height_cm(title)

    hgap    <- elements$hgap
    vgap    <- elements$vgap
    padding <- elements$padding

    switch(
      params$label.position,
      "top" = {
        widths  <- elements$barwidth
        heights <- c(label_height, vgap, elements$barheight)
      },
      "bottom" = {
        widths  <- elements$barwidth
        heights <- c(elements$barheight, vgap, label_height)
      },
      "left" = {
        widths  <- c(label_width, hgap, elements$barwidth)
        heights <- elements$barheight
      },
      "right" = {
        widths  <- c(elements$barwidth, hgap, label_width)
        heights <- elements$barheight
      }
    )

    switch(
      params$title.position,
      "top" = {
        widths  <- c(widths, max(0, title_width - sum(widths)))
        heights <- c(title_height, vgap, heights)
      },
      "bottom" = {
        widths  <- c(widths, max(0, title_width - sum(widths)))
        heights <- c(heights, vgap , title_height)
      },
      "left" = {
        widths  <- c(title_width, hgap, widths)
        heights <- c(heights, max(0, title_height - sum(heights)))
      },
      "right" = {
        widths  <- c(title_width, hgap, title_width)
        heights <- c(heights, max(0, title_height - sum(heights)))
      }
    )

    list(widths = widths, heights = heights)
  },

  build_bar = function(elements, bar, params) {
    width  <- elements$barwidth
    height <- elements$barheight

    if (isTRUE(params$raster)) {
      image <- switch(
        params$direction,
        "horizontal" = t(bar$colour),
        "vertical"   = rev(bar$colour)
      )
      bargrob <- rasterGrob(
        image  = image,
        width  = width,
        height = height,
        default.units = "cm",
        gp = gpar(col = NA),
        interpolate = TRUE
      )
    } else {
      if (params$direction == "horizontal") {
        bw <- width / nrow(bar)
        bx <- (seq(nrow(bar)) - 1) * bw
        bargrob <- rectGrob(
          x = bx, y = 0,
          vjust = 0, hjust = 0,
          width = bw, height = height,
          default.units = "cm",
          gp = gpar(col = NA, fill = bar$colour)
        )
      } else {
        bh <- height / nrow(bar)
        by <- (seq(nrow(bar)) - 1) * bh
        bargrob <- rectGrob(
          x = 0, y = by,
          vjust = 0, hjust = 0,
          width = width, height = bh,
          default.units = "cm",
          gp = gpar(col = NA, fill = bar$colour)
        )
      }
    }

    frame <- element_grob(
      elements$frame,
      width  = unit(width,  "cm"),
      height = unit(height, "cm")
    )

    grob <- grobTree(bargrob, frame)

    grob
  },

  build_ticks = function(elements, params) {

    pos <- params$tick_pos * elements$barlength / params$nbin
    if (isFALSE(params$draw.ulim)) pos <- pos[-1]
    if (isFALSE(params$draw.llim)) pos <- pos[-length(pos)]

    if (params$direction == "horizontal") {
      x <- unit(rep(pos, each = 4), "cm")
      y <- rep(unit.c(
        unit(0, "npc"),
        unit(0, "npc") + params$ticks.length[1],
        unit(1, "npc") - params$ticks.length[2],
        unit(1, "npc")
      ), times = length(pos))
      # y <- unit(rep(c(0, 1/5, 4/5, 1), length(pos)), "npc")
    } else {
      y <- unit(rep(pos, each = 4), "cm")
      x <- rep(unit.c(
        unit(0, "npc"),
        unit(0, "npc") + params$ticks.length[1],
        unit(1, "npc") - params$ticks.length[2],
        unit(1, "npc")
      ), times = length(pos))
    }
    id <- rep(2, length(pos) * 2)
    grob <- element_grob(
      elements$ticks,
      x = x,
      y = y,
      id.lengths = id
    )
    grob
  },

  build_labels = function(elements, key, params) {
    pos <- params$tick_pos * elements$barlength / params$nbin
    pos <- unit(pos, "cm")

    if (params$direction == "horizontal") {
      x <- pos
      y <- rep(elements$text$vjust, length(pos))
      margin_x = FALSE
      margin_y = TRUE
    } else {
      x <- rep(elements$text$hjust, length(pos))
      y <- pos
      margin_x = TRUE
      margin_y = FALSE
    }
    labels <- unlanguage(key$.label)

    grob <- element_grob(
      elements$text,
      label = labels,
      x = x,
      y = y,
      margin_x = margin_x,
      margin_y = margin_y
    )
    grob$name <- grobName(grob, "guide.label")
    grob
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

  setup_params = function(self, key, params) {
    params$direction <- arg_match0(
      self$direction,
      c("horizontal", "vertical"),
      arg_nm = "direction"
    )

    params$title.position <- arg_match0(
      self$title.position,
      c("top", "bottom", "left", "right"),
      arg_nm = "title.position"
    )

    if (params$direction == "horizontal") {
      label.position <- params$label.position %||% "bottom"
      label.position <- arg_match0(
        label.position, c("top", "bottom"), arg_nm = "label.position"
      )
    } else {
      label.position <- params$label.position %||% "right"
      label.position <- arg_match0(
        label.position, c("left", "right"), arg_nm = "label.position"
      )
    }
    params$label.position <- label.position

    params$tick_pos <- rescale(
      self$key$.value,
      c(0.5, params$nbin - 0.5),
      self$bar$value[c(1, nrow(self$bar))]
    )

    params$n_breaks <- nrow(key)

    params
  },

  setup_elements = function(theme, params) {

    # Title theme
    legend.title <- calc_element("legend.title", theme)
    title        <- combine_elements(params$title.theme, legend.title)
    title$hjust <- params$title.hjust %||% theme$legend.title.align %||%
      title$hjust %||% 0
    title$vjust <- params$title.vjust %||% title$vjust %||% 0.5

    # Label theme
    legend.text <- calc_element("legend.text", theme)
    label       <- combine_elements(params$label.theme, legend.text)

    # Label justification
    if (is.null(params$label.theme$hjust) && is.null(theme$legend.text$hjust)) {
      label$hjust <- NULL
    }
    if (is.null(params$label.theme$vjust) && is.null(theme$legend.text$vjust)) {
      label$vjust <- NULL
    }
    just_defaults <- .legend_just_defaults[[params$label.position]]

    label$hjust <- params$label.hjust %||% theme$legend.text.align %||%
      label$hjust %||% just_defaults$hjust
    label$vjust <- params$label.vjust %||% label$vjust %||% just_defaults$vjust

    # Adjust label margins if tick length is negative
    adjust <- unit.pmax(params$ticks.length * -1, unit(0, "pt"))
    mar <- label$margin
    switch(
      params$label.position,
      "top"    = {mar[3] <- mar[3] + adjust[2]},
      "bottom" = {mar[1] <- mar[1] + adjust[1]},
      "right"  = {mar[4] <- mar[4] + adjust[2]},
      "left"   = {mar[2] <- mar[2] + adjust[1]}
    )
    label$margin <- mar

    if (isFALSE(params$label)) {
      label <- element_blank()
    }

    # Ticks and frame
    ticks <- combine_elements(params$ticks, calc_element("line", theme))
    frame <- combine_elements(params$frame, calc_element("rect", theme))

    # Bar size
    if (params$direction == "horizontal") {
      barwidth  <- width_cm( params$barwidth  %||% (theme$legend.key.width * 5))
      barheight <- height_cm(params$barheight %||%  theme$legend.key.height)
      barlength <- barwidth
    } else {
      barwidth  <- width_cm( params$barwidth  %||%  theme$legend.key.width)
      barheight <- height_cm(params$barheight %||% (theme$legend.key.height * 5))
      barlength <- barheight
    }

    size <- title$size %||% legend.title$size %||% elem_text$size %||% 11
    size <- 0.5 * unit(size, "pt")

    hgap <- width_cm( theme$legend.spacing.x %||% size)
    vgap <- height_cm(theme$legend.spacing.y %||% size)

    padding <- convertUnit(theme$legend.margin %||% margin(), "cm",
                           valueOnly = TRUE)

    background = element_render(theme, "legend.background")

    list(
      # Elements
      title      = title,
      text       = label,
      ticks      = ticks,
      frame      = frame,
      background = background,

      # Measurements
      barwidth   = barwidth,
      barheight  = barheight,
      barlength  = barlength,
      hgap       = hgap,
      vgap       = vgap,
      padding    = padding
    )
  }
)
