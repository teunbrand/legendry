# Constructor -------------------------------------------------------------

#' Guide primitive: title
#'
#' This function constructs a title [guide primitive][guide-primitives].
#'
#' @inheritParams common_parameters
#'
#' @return A `<PrimitiveTitle>` primitive guide that can be used inside other
#'   guides.
#' @export
#' @family primitives
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'  geom_point()
#'
#' # Adding as secondary guides
#' p + guides(
#'   x.sec = primitive_title("Horizontal Title"),
#'   y.sec = primitive_title(c("along vertical", "Multiple tiles"))
#' )
primitive_title = function(title = waiver(), angle = waiver(),
                           theme = NULL, position = waiver()) {
  if (!is_waive(angle)) {
    check_number_decimal(
      angle, min = -360, max = 360,
      allow_infinite = FALSE, allow_null = TRUE
    )
  }

  new_guide(
    title = NULL,
    my_title = title,
    angle = angle,
    theme = theme,
    position = position,
    available_aes = c("any", "x", "y", "r", "theta"),
    super = PrimitiveTitle
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
PrimitiveTitle <- ggproto(
  "PrimitiveTitle", Guide,

  hashables = exprs(my_title),

  params = new_params(my_title = waiver(), angle = waiver()),

  extract_key = function(scale, aesthetic, ...) {
    # Need to keep track of limits for r/r.sec positions
    data_frame0(!!aesthetic := c(-Inf, Inf), .value = scale$get_limits())
  },

  extract_params = function(scale, params, title = waiver(), ...) {
    params$my_title <-
      scale$make_title(params$my_title %|W|% scale$name %|W|% title)
    primitive_extract_params(scale, params, ...)
  },

  transform = function(self, params, coord, panel_params) {
    if (params$position %in% c("theta", "theta.sec")) {
      params$bbox  <- panel_params$bbox  %||% list(c(x = c(0, 1), y = c(0, 1)))
      params$arc   <- panel_params$arc   %||% c(0, 2 * pi)
      params$donut <- panel_params$donut %||% c(0, 0.4)
    }
    params$key <-
      transform_key(params$key, params$position, coord, panel_params)
    params
  },

  setup_params = primitive_setup_params,

  setup_elements = function(params, elements, theme) {
    prefix <- ""
    suffix <- ""
    if (params$aesthetic %in% c("x", "y")) {
      suffix <- switch(
        params$position,
        theta = ".x.bottom",
        theta.sec = ".x.top",
        paste0(".", params$aesthetic, ".", params$position)
      )
      prefix <- "axis."
    } else {
      prefix <- "legend."
    }
    elements <- list(title = paste0(prefix, "title", suffix))
    elements$offset <- cm(params$stack_offset %||% 0)
    Guide$setup_elements(params, elements, theme)
  },

  measure_grobs = function(grobs, params, elements) {
    switch(
      params$position,
      top = , bottom = height_cm(grobs),
      left = , right = width_cm(grobs),
      attr(grobs, "offset")
    )
  },

  build_title = function(label, elements, params) {
    label <- validate_labels(label)
    switch(
      params$position,
      theta = , theta.sec = draw_theta_title(label, elements, params),
      draw_cart_title(label, elements, params)
    )
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    params <- replace_null(params, position = position, direction = direction)
    params <- self$setup_params(params)

    elems <- self$setup_elements(params, self$elements, theme)
    title <- self$build_title(params$my_title, elems, params)
    size  <- self$measure_grobs(title, params, elements)

    primitive_grob(
      grob = title,
      size = unit(size, "cm"),
      position = params$position,
      name = "title"
    )
  }
)

# Helpers -----------------------------------------------------------------

draw_theta_title <- function(label, elements, params) {

  title <- elements$title
  position <- params$position
  hjust <- title$hjust
  offset <- elements$offset
  donut <- params$donut
  bbox <- params$bbox
  r <- if (position == "theta") donut[2] else donut[1]

  theta <- rescale(hjust, from = c(0, 1), to = params$arc)

  n_labels <- length(label)
  if (n_labels > 1) {
    theta <- rescale(
      seq(0, 1, length.out = n_labels),
      from = c(0, 1), to = params$arc
    )
  }

  angle <- params$angle %|W|% NULL
  if (is.null(angle)) {
    angle <- title$angle
  } else {
    angle <- flip_text_angle(angle - rad2deg(theta))
  }
  rad <- deg2rad(angle)

  margin <- cm(max(title$margin))
  offset <- offset + margin

  x <- rescale(r * sin(theta) + 0.5, from = bbox$x)
  y <- rescale(r * cos(theta) + 0.5, from = bbox$y)

  if (position == "theta.sec") {
    theta <- theta + pi
  }

  hjust <- 0.5 - sin(theta + rad) / 2
  vjust <- 0.5 - cos(theta + rad) / 2

  x <- unit(x, "npc") + unit(offset * sin(theta), "cm")
  y <- unit(y, "npc") + unit(offset * cos(theta), "cm")

  grob <- element_grob(
    element = title,
    label = label,
    x = x, y = y,
    hjust = hjust, vjust = vjust,
    angle = angle
  )

  if (inherits(grob, "textpath")) {
    height <- measure_textpath_labels(grob)
  } else {
    height <- measure_theta_labels(title, label, margin, theta + rad, hjust, vjust)
  }
  attr(grob, "offset") <- height
  grob
}

draw_cart_title <- function(label, elements, params) {
  if (length(label) < 1) {
    return(zeroGrob())
  }

  limits <- sort(params$key[[params$aesthetic]])
  limits <- oob_squish_infinite(limits)

  title    <- elements$title
  position <- params$position
  hjust <- title$hjust
  vjust <- title$vjust

  angle <- (params$angle %|W|% NULL) %||% title$angle

  singles <- lapply(label, function(lab) {
    element_grob(title, lab, margin_x = TRUE, margin_y = TRUE, angle = angle,
                 hjust = 0.5, vjust = 0.5)
  })
  widths  <- width_cm(singles)
  heights <- height_cm(singles)

  if (position %in% c("left", "right")) {
    x <- hjust
    y <- rescale(vjust, from = c(0, 1), to = limits)
  } else {
    x <- rescale(hjust, from = c(0, 1), to = limits)
    y <- vjust
  }

  n_labels <- length(label)
  if (n_labels > 1) {
    if (position %in% c("top", "bottom")) {
      x <- seq(limits[1], limits[2], length.out = n_labels)
      hjust <- seq(0, 1, length.out = n_labels)
    } else {
      y <- seq(limits[1], limits[2], length.out = n_labels)
      vjust <- seq(0, 1, length.out = n_labels)
    }
  }

  x <- unit(x, "npc") + unit(widths  * (0.5 - hjust), "cm")
  y <- unit(y, "npc") + unit(heights * (0.5 - vjust), "cm")

  element_grob(
    title,
    label = label,
    x = x, y = y,
    hjust = 0.5, vjust = 0.5,
    angle = angle,
    margin_x = TRUE,
    margin_y = TRUE
  )
}
