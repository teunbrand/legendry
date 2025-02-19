# Constructor -------------------------------------------------------------

#' Circle size guide
#'
#' This guide displays the sizes of points as a series of circles. It is
#' typically paired with [`geom_point()`][ggplot2::geom_point] with
#' [`draw_key_point()`][ggplot2::draw_key_point] glyphs.
#'
#' @param key A [standard key][key_standard] specification. Defaults to
#'   [`key_auto()`]. See more information in the linked topic.
#' @param hjust,vjust A `<numeric[1]>` between 0 and 1 giving the horizontal
#'   and vertical justification, respectively, of the central shapes. It is
#'   recommended `hjust = 0.5` when text is placed on the left or right and
#'   `vjust = 0.5` is recommended when text is placed on top or in the bottom.
#' @param text_position A string, one of `"ontop"`, `"top"`, `"right"`,
#'   `"bottom"`, or `"left"` do describe the placement of labels. The
#'   default (`NULL`), will take the `legend.text.position` theme setting.
#' @param clip_text A `<logical[1]>` whether to give text in the `"ontop"`
#'   position a small rectangle of background colour.
#' @inheritParams common_parameters
#'
#' @return A `<GuideCircles>` object.
#' @export
#' @family standalone guides
#' @details
#' Please note that the default size scales scale to area, not radius, so
#' equidistant breaks will appear at irregularly spaced positions due to
#' labelling the diameter of a circle.
#'
#' This graph was designed with standard round [shapes][ggplot2::scale_shape]
#' in mind, i.e. shapes 1, 16, 19 and 21. For that reason, `shape = 1` is the
#' default `override.aes` argument. Other shapes will probably be drawn but the
#' quality of their alignment and label placement may be unsatisfactory.
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mtcars, aes(disp, mpg)) +
#'   geom_point(aes(size = hp), alpha = 0.3)
#'
#' # By default, the sizes aren't large enough to make this guide clear
#' p + scale_size_area(guide = "circles")
#'
#' # Update with a more approrpriate scale
#' p <- p +
#'   scale_size_area(
#'     max_size = 30,
#'     limits = c(0, NA),
#'     breaks = c(0, 25, 100, 250)
#'   )
#' p + guides(size = "circles")
#'
#' # Horizontal orientation
#' p + guides(size = guide_circles(
#'   vjust = 0.5, hjust = 0, text_position = "bottom"
#' ))
#'
#' # Alternative text placement
#' p + guides(size = guide_circles(
#'   text_position = "ontop",
#'   clip_text = TRUE
#' ))
#'
#' # More styling options
#' p + guides(size = guide_circles(override.aes = aes(colour = "red")))+
#'   theme(
#'     # Key background
#'     legend.key = element_rect(colour = "black", fill = 'white'),
#'     # Padding around central shapes
#'     legendry.legend.key.margin = margin(1, 1, 1, 1, "cm"),
#'     legend.ticks = element_line(colour = "blue"),
#'     legend.text.position = "left"
#'   )
guide_circles <- function(
  key = NULL,
  title = waiver(),
  theme = NULL,
  hjust = 0.5,
  vjust = 0,
  text_position = NULL,
  clip_text = FALSE,
  override.aes = list(shape = 1),
  position = waiver(),
  direction = NULL
) {

  check_number_decimal(hjust, min = 0, max = 1, allow_infinite = FALSE)
  check_number_decimal(vjust, min = 0, max = 1, allow_infinite = FALSE)
  check_bool(clip_text)
  check_argmatch(text_position, c(.trbl, "ontop"), allow_null = TRUE)

  new_guide(
    key = key,
    title = title,
    theme = theme,
    hjust = hjust,
    vjust = vjust,
    text_position = text_position,
    clip_text = clip_text,
    override.aes = rename_aes(override.aes),
    position = position,
    direction = direction,
    super = GuideCircles
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
GuideCircles <- ggproto(
  "GuideCircles", Guide,

  params = new_params(key = "auto", hjust = 0.5, vjust = 0, clip_text = TRUE,
                      text_position = "ontop", override.aes = list()),

  elements = list(
    background = "legend.background",
    margin     = "legend.margin",
    key        = "legend.key",
    text       = "legend.text",
    padding    = "legendry.legend.key.margin",
    ticks      = "legend.ticks",
    title      = "legend.title",
    title_position = "legend.title.position"
  ),

  extract_params = function(scale, params, title = waiver(), ...) {
    params$title    <- scale$make_title(params$title %|W|% scale$name %|W|% title)
    params$position <- params$position %|W|% NULL
    params$limits   <- scale$get_limits()
    params
  },

  process_layers = function(self, params, layers, data = NULL) {
    GuideLegend$process_layers(params, layers, data)
  },

  build_decor = function(decor, grobs, elements, params) {

    key <- params$key

    glyphs <- lapply(
      decor, draw_circles,
      hjust = params$hjust,
      vjust = params$vjust
    )

    x <- glyphs[[1]]$xpos
    y <- glyphs[[1]]$ypos

    key_bg <- element_grob(elements$key)
    padding <- cm(elements$padding)
    width <- width_cm(glyphs[[1]]$width)
    height <- height_cm(glyphs[[1]]$height)
    width  <- unit(c(padding[4], width,  padding[2]), "cm")
    height <- unit(c(padding[1], height, padding[3]), "cm")

    position <- params$text_position %||% elements$text_position
    if (position == "ontop") {
      text <- Map(
        element_grob, label = key$.label[order(-key$size)], x = x, y = y,
        MoreArgs = list(element = elements$text)
      )
      if (isTRUE(params$clip_text)) {
        glyphs <- censor_text_background(
          x, y, text, glyphs,
          vjust = elements$text$vjust, colour = elements$key$fill
        )
      }
      grob <- gTree(children = inject(gList(!!!glyphs, !!!text)))
    } else {
      ticks <- draw_circle_ticks(elements$ticks, x, y, padding, position)

      x <- switch(position, left = , right = NULL, x)
      y <- switch(position, top = , bottom = NULL, y)

      text <- element_grob(
        elements$text, x = x, y = y, label = key$.label[order(-key$size)],
        margin_x = position %in% c("left", "right"),
        margin_y = position %in% c("top", "bottom")
      )
      grob <- gTree(children = inject(gList(ticks, !!!glyphs)))
    }

    gt <- gtable(widths = width, heights = height)
    gt <- gtable_add_grob(
      gt, key_bg, clip = "off", name = "key-bg",
      t = 1, l = 1, b = 3, r = 3
    )
    gt <- gtable_add_grob(gt, grob, t = 2, l = 2, clip = "off", name = "circles")
    if (position == "ontop") {
      return(gt)
    }

    gt <- switch(
      position,
      left   = gtable_add_cols(gt, unit(width_cm(text),  "cm"),  0),
      right  = gtable_add_cols(gt, unit(width_cm(text),  "cm"), -1),
      bottom = gtable_add_rows(gt, unit(height_cm(text), "cm"), -1),
      top    = gtable_add_rows(gt, unit(height_cm(text), "cm"),  0)
    )
    gt <- gtable_add_grob(
      gt, text, clip = "off", name = "labels",
      t = switch(position, top = 1, bottom = 4, 2),
      l = switch(position, left = 1, right = 4, 2)
    )
    gt
  },

  setup_elements = function(params, elements, theme) {
    theme <- theme + params$theme
    theme$legend.axis.line <- theme$legend.axis.line %||% calc_element("panel.grid", theme)

    # Setup title
    title_position <- calc_element("legend.title.position", theme) %||%
      switch(params$direction, horizontal = "left", vertical = "top")
    elements$title <- setup_legend_title(theme, title_position, params$direction)

    text_position <- params$text_position %||%
      calc_element("legend.text.position", theme) %||% "right"
    if (text_position != "ontop") {
      elements$text <- setup_legend_text(theme, text_position, params$direction)
    }

    is_char <- is_each(elements, is.character)
    elements[is_char] <- lapply(elements[is_char], calc_element, theme = theme)
    elements$title_position <- title_position
    elements$text_position  <- text_position
    elements
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    params <- replace_null(params, position = position, direction = direction)

    elems  <- self$setup_elements(params, self$elements, theme)

    title <- self$build_title(params$title, elems, params)
    grob  <- self$build_decor(params$decor, NULL, elems, params)
    grobs <- list(circles = grob, title = title)

    self$assemble_drawing(grobs, params = params, elements = elems)
  },

  assemble_drawing = function(self, grobs, layout = NULL, sizes = NULL,
                              params = list(), elements = list()) {

    # Add title
    gt <- self$add_title(
      grobs$circles, grobs$title, elements$title_position,
      with(elements$title, rotate_just(angle, hjust, vjust))
    )

    # Add padding
    padding <- rep(0, 4)
    padding[c(1, 3)] <- height_cm(elements$margin[c(1, 3)])
    padding[c(2, 4)] <-  width_cm(elements$margin[c(2, 4)])
    gt <- gtable_add_padding(gt, unit(padding, "cm"))

    # Add background
    background <- element_grob(elements$background)
    if (!is.zero(background)) {
      gt <- gtable_add_grob(
        gt, background, name = "background", clip = "off",
        t = 1, r = -1, b = -1, l = 1, z = -Inf
      )
    }
    gt
  }
)


# Helpers -----------------------------------------------------------------

censor_text_background <- function(x, y, text, background,
                                   vjust = 0.5, padding = 0.1,
                                   colour = "white") {

  width  <- width_cm(text)  + padding
  height <- height_cm(text) + padding

  mask <- rectGrob(
    x = x, y = y - unit((vjust - 0.5) * height, "cm"),
    width  = unit(width, "cm"),
    height = unit(height, "cm"),
    gp = gpar(fill = colour, col = NA, lty = 0),
    vp = viewport(clip = "on")
  )

  c(background, list(mask))
}

draw_circle_ticks <- function(element, x, y, padding, position) {

  n <- length(x)
  f <- function(x, i) unit(rep(x, n), "npc")

  xend <- switch(
    position,
    left  = unit(rep(0, n), "npc") - unit(padding[4], "cm"),
    right = unit(rep(1, n), "npc") + unit(padding[2], "cm"),
    x
  )

  yend <- switch(
    position,
    top    = unit(rep(1, n), "npc") + unit(padding[1], "cm"),
    bottom = unit(rep(0, n), "npc") - unit(padding[3], "cm"),
    y
  )

  interleave <- vec_interleave(seq_len(n), seq_len(n) + n)
  x <- unit.c(x, xend)[interleave]
  y <- unit.c(y, yend)[interleave]

  element_grob(element, x = x, y = y, id.lengths = rep(2, n))
}

draw_circles <- function(decor, vjust = 0, hjust = 0.5) {

  data <- vec_slice(decor$data, decor$data$.draw %||% TRUE)
  order <- order(-data$size)
  glyph <- lapply(seq_len(nrow(data))[order], function(i) {
    decor$draw_key(vec_slice(data, i), decor$params, 0)
  })
  size  <- map_dbl(glyph, function(x) max(x$gp$fontsize %||% 0))
  shape <- map_dbl(glyph, function(x) if (is.numeric(x$pch)) x$pch[1] else 1L)

  # These are some arcane incantations I've long forgotten why they work
  size <- size * magic_num * pch_mult[shape]
  ydiv <-  pch_div0[shape] * (1 - vjust) + pch_div1[shape] * vjust
  xdiv <- (pch_div0[shape] * (1 - hjust) + pch_div1[shape] * hjust) * asp[shape]

  yoffset <- (max(size) - size) / ydiv
  xoffset <- (max(size) - size) / xdiv

  ypos <- (vjust - 0.5) * 2
  ypos <- unit(ynpc[shape], "npc") + unit(ypos * yoffset, "pt")

  xpos <- (hjust - 0.5) * 2
  xpos <- unit(0.5, "npc") + unit(xpos * xoffset, "pt")

  glyph <- Map(function(grob, x, y) {
    vp <- editViewport(grob$vp %||% viewport(), x = x, y = y)
    editGrob(grob, vp = vp)
  }, grob = glyph, x = xpos, y = ypos)

  if (all(shape %in% c(0, 7, 12, 14, 15, 22))) {
    xpos <- xpos + unit((hjust - 0.5) * -size, "pt")
    ypos <- ypos + unit((vjust - 0.5) * -size, "pt")
  } else {
    angle <- 1.5 * pi - atan2(vjust * 2 - 1, hjust * 2 - 1)
    xpos <- xpos + unit(sin(angle) * 0.5 * size, "pt")
    ypos <- ypos + unit(cos(angle) * 0.5 * size * asp[shape], "pt")
  }

  gTree(
    height = unit(max(size), "pt"),
    width  = unit(max(size * asp[shape]), "pt"),
    xpos = xpos, ypos = ypos,
    children = inject(gList(!!!glyph))
  )
}

# Magic constants ---------------------------------------------------------

# Don't bother asking me to implement other shapes. I've already forgotten
# why the calculations do what they do.

magic_num <- .pt / .stroke

pch_div <- rep(
  c(2, 3, 2, 1.5, 2, 3, 2, 3, 1.5),
  c(1L, 1L, 3L, 1L, 10L, 1L, 6L, 1L, 1L)
)

pch_div0 <- pch_div
pch_div1 <- pch_div
pch_div1[c(2, 17, 24)] <- 1.5
pch_div1[c(6, 25)] <- 3

ynpc <- rep(0.5, 25)
ynpc[c(2, 17, 24)] <- 0.35
ynpc[c(6, 25)] <- 0.65

asp <- rep(1, 25)
asp[c(2, 6, 17, 24, 25)] <- 2 / sqrt(3)

pch_mult <- c(
  1, 1.166, 1.41466666666667, 1, 1.41466666666667, 1.166, 1,
  1, 1.41466666666667, 1, 1.55466666666667, 1, 1, 1, 1, 1, 1.166,
  1, 1, 0.666666666666667, 1, 0.886, 1.25333333333333, 1.166, 1.166
)
