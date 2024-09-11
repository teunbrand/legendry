guide_axis_nested <- function(
  key   = "range_auto",
  regular_key = "auto",
  type  = "bracket",
  title = waiver(),
  theme = NULL,
  angle = waiver(),
  cap   = "none",
  bidi  = FALSE,
  oob   = "squish",
  drop_zero = TRUE,
  pad_discrete = 0.4,
  levels_text = NULL,
  ...,
  order = 0,
  position = waiver()
) {

  theme <- theme %||% theme()
  theme$gguidance.guide.spacing <-
    theme$gguidance.guide.spacing %||% unit(0, "cm")

  nesting <- switch(
    arg_match0(type, c("bracket", "box")),
    bracket = primitive_bracket,
    box = primitive_box
  )

  if (identical(key, "range_auto")) {
    labels <- guide_none
  } else {
    labels <- primitive_labels(angle = angle)
  }

  compose_stack(
    primitive_line(cap = cap, position = position),
    primitive_ticks(bidi = bidi, position = position),
    labels,
    nesting(
      key = key %||% "range_auto", angle = angle,
      oob = oob, drop_zero = drop_zero, pad_discrete = pad_discrete,
      levels_text = levels_text, ...
    ),
    key = regular_key %||% "auto",
    side.titles = NULL, drop = 3:4, title = title, theme =theme,
    order = order, available_aes = c("any", "x", "y", "r", "theta"),
    position = position
  )
}
