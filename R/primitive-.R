#' @name guide-primitives
#' @title Guide primitives
#'
#' @description
#' Guide primitives are the building blocks of more complex guides. On their
#' own, they are not very useful as a visual reflection of a scale.
#'
#' Their purpose is to be combined with one another to form a more complex,
#' complete guides that *do* reflect a scale in some way.
#'
#' @details
#' The guide primitives are simple, but flexible in that they are not tailored
#' for one particular aesthetic. That way they can be reused and combined
#' at will.
NULL

# Convenience function for automatically setting sensible gtable based on
# a single grob
primitive_grob <- function(grob, size, position, name) {

  if (!position %in% .trbl) {
    if (is.grob(grob)) {
      grob <- gList(grob)
    } else {
      grob <- inject(gList(!!!grob))
    }
    grob <- gTree(offset = sum(size), children = grob)
    return(grob)
  }

  origin   <- unit(as.numeric(position %in% c("left", "bottom")), "npc")
  opposite <- opposite_position(position)

  if (position %in% c("top", "bottom")) {
    width  <- unit(1, "npc")
    height <- size
    vp     <- viewport(y = origin, height = sum(size), just = opposite)
    gt <- gtable(widths = width, heights = height)
    gt <- gtable_add_grob(gt, grob, t = seq_along(size), l = 1L, clip = "off", name = name)
  } else {
    height <- unit(1, "npc")
    width  <- size
    vp     <- viewport(x = origin, width = sum(size), just = opposite)
    gt <- gtable(widths = width, heights = height)
    gt <- gtable_add_grob(gt, grob, t = 1L, l = seq_along(size), clip = "off", name = name)
  }

  absoluteGrob(gList(gt), width = sum(width), height = sum(height), vp = vp)
}

primitive_setup_elements <- function(params, elements, theme) {
  if (params$aesthetic %in% c("x", "y")) {
    elements <- suffix_position(elements$position, params$position)
  } else {
    elements <- elements$legend
  }
  theme <- theme + params$theme
  if (identical(elements$text, "legend.text")) {
    elements$text <- setup_legend_text(theme, direction = params$direction)
  }
  if (identical(elements$title, "legend.title")) {
    elements$title <- setup_legend_title(theme, direction = params$direction)
  }
  if (identical(elements$ticks_length, "legend.ticks.length")) {
    theme$legend.ticks.length <- theme$legend.ticks.length %||%
      (calc_element("legend.key.size", theme) * 0.2)
  }
  is_char <- is_each(elements, is.character)
  elements[is_char] <- lapply(elements[is_char], calc_element, theme = theme)
  elements$offset <- cm(params$stack_offset %||% 0)
  elements
}

primitive_extract_params = function(scale, params, ...) {
  params$position <- params$position %|W|% NULL
  params$limits   <- scale$get_limits()
  params
}

primitive_setup_params <- function(params) {
  if (params$aesthetic %in% c("x", "y")) {
    return(params)
  }

  if (!is_empty(params$key)) {
    key   <- params$key
    value <- guide_rescale(key$.value, params$limits)
    oppo  <- key$oppo %||% as.numeric(params$position %in% c("left", "bottom"))
    key$x <- key$x %||% switch(params$position, left = , right = oppo, value)
    key$y <- key$y %||% switch(params$position, bottom = , top = oppo, value)
    params$key <- key
  }

  decor <- params$decor
  if (!is_empty(params$decor)) {
    decor <- params$decor
    oppo  <- decor$oppo %||% as.numeric(params$position %in% c("left", "bottom"))
    decor$x <- decor$x %||% switch(params$position, left = , right = oppo, 0.5)
    decor$y <- decor$y %||% switch(params$position, bottom = , top = oppo, 0.5)
    params$decor <- decor
  }
  params
}
