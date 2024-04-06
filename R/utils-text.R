
setup_legend_text <- function(theme, direction = "vertical") {
  position <- calc_element("legend.text.position", theme)
  position <- position %||% switch(direction, horizontal = "bottom", vertical = "right")
  gap    <- calc_element("legend.key.spacing", theme)
  margin <- calc_element("text", theme)$margin
  margin <- position_margin(position, margin, gap)
  text <- theme(
    text = switch(
      position,
      top    = element_text(hjust = 0.5, vjust = 0.0, margin = margin),
      bottom = element_text(hjust = 0.5, vjust = 1.0, margin = margin),
      left   = element_text(hjust = 1.0, vjust = 0.5, margin = margin),
      right  = element_text(hjust = 0.0, vjust = 0.5, margin = margin)
    )
  )
  calc_element("legend.text", theme + text)
}

setup_legend_title <- function(theme, direction = "vertical") {
  position <- calc_element("legend.title.position", theme)
  position <- position %||% switch(direction, horizontal = "left", vertical = "top")
  gap <- calc_element("legend.key.spacing", theme)
  margin <- calc_element("text", theme)$margin
  margin <- position_margin(position, margin, gap)
  title <- theme(text = element_text(hjust = 0, vjust = 0.5, margin = margin))
  calc_element("legend.title", theme + title)
}

get_text_dim_cm <- function(label, style, type = "both") {
  if (inherits(style, "element_text")) {
    style <- get_text_gp(style)
  }
  check_inherits(style, "gpar", "a {.cls gpar} object")
  pushViewport(viewport(gp = style), recording = FALSE)
  on.exit(popViewport(recording = FALSE))
  switch(
    type,
    width  = convertWidth( stringWidth(label),  unitTo = "cm", valueOnly = TRUE),
    height = convertHeight(stringHeight(label), unitTo = "cm", valueOnly = TRUE),
    list(
      width  = convertWidth( stringWidth(label),  unitTo = "cm", valueOnly = TRUE),
      height = convertHeight(stringHeight(label), unitTo = "cm", valueOnly = TRUE)
    )
  )
}

get_text_gp <- function(x) {
  if (!inherits(x, "element_text")) {
    return(NULL)
  }
  gpar(
    fontfamily = x$family,
    fontface   = x$face,
    fontsize   = x$size,
    lineheight = x$lineheight
  )
}

get_fontmetrics <- function(x) {
  if (inherits(x, "element_text")) {
    x <- get_text_gp(x)
  }
  check_inherits(x, "gpar", "a {.cls gpar} object")
  res <- 72
  info <- systemfonts::font_info(
    family = x$fontfamily,
    italic = x$font %in% c(3, 4),
    bold   = x$font %in% c(2, 4),
    size   = x$fontsize,
    res    = res
  )
  info <- as.list(info)
  i <- match(c("max_ascend", "underline_size"), names(info))
  info[i[1]:i[2]] <- lapply(info[i[1]:i[2]], function(x) .in2cm * x / res)
  info
}
