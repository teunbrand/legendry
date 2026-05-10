.label_params <- c(
  "text_colour", "colour",
  "text_size", "size",
  "face", "hjust", "vjust", "angle",
  "lineheight"
)
.line_params <- c(
  "line_colour",    "colour",
  "line_linewidth", "linewidth",
  "line_linetype",  "linetype"
)
.rect_params <- c(
  "rect_colour",    "colour",
  "rect_linewidth", "linewidth",
  "rect_linetype",  "linetype",
  "rect_fill",      "fill"
)
.point_params <- c(
  "point_colour", "colour",
  "point_size",  "size",
  "point_fill", "fill",
  "shape", "stroke"
)
.element_params <- unique(
  c(.label_params, .line_params, .rect_params, .point_params)
)

# Collects elemental properties from key into list, taking into account
# some simple inheritance from a global pool of properties, i.e.
# for text elements, the `text_colour` takes precedence over `colour`.
element_key_properties <- function(key, type, ...) {
  props <- switch(
    type,
    point = list(
      colour     = key$.point_colour   %||% key$.colour,
      size       = key$.point_size     %||% key$.size,
      fill       = key$.point_fill     %||% key$.fill,
      shape      = key$.shape,
      stroke     = key$.stroke
    ),
    rect = list(
      colour     = key$.rect_colour    %||% key$.colour,
      linewidth  = key$.rect_linewidth %||% key$.linewidth,
      linetype   = key$.rect_linetype  %||% key$.linetype,
      fill       = key$.rect_fill      %||% key$.fill
    ),
    line = list(
      colour     = key$.line_colour    %||% key$.colour,
      linewidth  = key$.line_linewidth %||% key$.linewidth,
      linetype   = key$.line_linetype  %||% key$.linetype
    ),
    text = list(
      colour     = key$.text_colour    %||% key$.colour,
      size       = key$.text_size      %||% key$.size,
      face       = key$.face,
      hjust      = key$.hjust,
      vjust      = key$.vjust,
      angle      = key$.angle,
      lineheight = key$.lineheight
    ),
    list()
  )
  extra <- lapply(list2(...), rep0, length.out = nrow(key))
  props <- replace_null(props, !!!extra)
  props[lengths(props) > 0]
}

destructure_element <- function(element) {
  if (!S7::S7_inherits(element)) {
    return(element)
  }
  S7::props(element)
}

element_classes <- function(..., allow_null = TRUE) {
  type <- do.call(c, list(...))
  classes <- character()
  if (allow_null) {
    classes <- c(classes, "NULL")
  }
  if ("rect" %in% type) {
    classes <- c(classes, "ggplot2::element_rect", "element_rect")
  }
  if ("line" %in% type) {
    classes <- c(classes, "ggplot2::element_line", "element_line")
  }
  if ("text" %in% type) {
    classes <- c(classes, "ggplot2::element_text", "element_text")
  }
  if ("blank" %in% type) {
    classes <- c(classes, "ggplot2::element_blank", "element_blank")
  }
  if ("point" %in% type) {
    classes <- c(classes, "ggplot2::element_point")
  }
  if ("polygon" %in% type) {
    classes <- c(classes, "ggplot2::element_polygon")
  }
  classes
}


