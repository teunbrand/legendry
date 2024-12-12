# Constructor -------------------------------------------------------------

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
    override.aes = override.aes,
    position = position,
    direction = direction,
    super = GuideCircles
  )
}

# Class -------------------------------------------------------------------

GuideCircles <- ggproto(
  "GuideCircles", Guide,

  params = new_params(key = "auto", hjust = 0.5, vjust = 0, clip_text = TRUE,
                      text_position = "ontop", override.aes = list()),

  elements = list(
    background = "legend.background",
    margin     = "legend.margin",
    key        = "legend.key",
    text       = "legend.text",
    padding    = "legend.key.spacing",
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

  get_layer_key = GuideLegend$get_layer_key,

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
    padding <- cm(elements$padding %||% 0) * 2
    width  <- unit(width_cm(glyphs[[1]]$width)   + padding, "cm")
    height <- unit(height_cm(glyphs[[1]]$height) + padding, "cm")

    position <- params$text_position %||% elements$text_position
    if (position == "ontop") {
      text <- Map(
        element_grob, x = x, y = y, label = key$.label,
        MoreArgs = list(element = elements$text)
      )
      if (isTRUE(params$clip_text)) {
        glyphs <- censor_text_background(
          x, y, text, glyphs,
          vjust = elements$text$vjust, colour = elements$key$fill
        )
      }
      grob <- gTree(children = inject(gList(key_bg, !!!glyphs, !!!text)))
    } else {
      ticks <- draw_circle_ticks(elements$ticks, x, y, position)

      x <- switch(position, left = , right = NULL, x)
      y <- switch(position, top = , bottom = NULL, y)

      text <- element_grob(
        elements$text, x = x, y = y, label = key$.label,
        margin_x = position %in% c("left", "right"),
        margin_y = position %in% c("top", "bottom")
      )
      grob <- gTree(children = inject(gList(key_bg, ticks, !!!glyphs)))
    }

    gt <- gtable(widths = width, heights = height)
    gt <- gtable_add_grob(gt, grob, t = 1, l = 1, clip = "off", name = "circles")
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
      t = 1 + as.numeric(position == "bottom"),
      l = 1 + as.numeric(position == "right")
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

draw_circle_ticks <- function(element, x, y, position) {

  n <- length(x)
  f <- function(x) unit(rep(x, n), "npc")

  xend <- switch(position, left   = f(0), right = f(1), x)
  yend <- switch(position, bottom = f(0),   top = f(1), y)

  interleave <- vec_interleave(seq_len(n), seq_len(n) + n)
  x <- unit.c(x, xend)[interleave]
  y <- unit.c(y, yend)[interleave]

  element_grob(element, x = x, y = y, id.lengths = rep(2, n))
}

draw_circles <- function(decor, vjust = 0, hjust = 0.5) {

  data <- vec_slice(decor$data, decor$data$.draw %||% TRUE)
  glyph <- lapply(seq_len(nrow(data)), function(i) {
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
