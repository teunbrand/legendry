# Constructor -------------------------------------------------------------

#' Guide primitive: labels
#'
#' This functions contructs a labels [guide primitive][guide-primitives].
#'
#' @inheritParams ggplot2::guide_axis
#'
#' @return A `GuideLabels` primitive guide that can be used inside other guides.
#' @export
#' @family primitives
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'  geom_point()
#'
#' # Adding as secondary guides
#' p + guides(x.sec = "labels", y.sec = guide_labels(n.dodge = 2))
guide_labels <- function(angle = waiver(), n.dodge = 1, check.overlap = FALSE,
                         theme = NULL, position = waiver()) {
  if (!is_waive(angle)) {
    check_number_decimal(
      angle, min = -360, max = 360,
      allow_infinite = FALSE, allow_null = TRUE
    )
  }
  check_number_whole(n.dodge, min = 1)
  check_bool(check.overlap)

  new_guide(
    angle = angle,
    n_dodge = n.dodge,
    check_overlap = check.overlap,
    theme = theme,
    position = position,
    available_aes = c("any", "x", "y", "r", "theta"),
    super = GuideLabels
  )
}


# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideLabels <- ggproto(
  "GuideLabels", Guide,

  params = c(
    Guide$params,
    list(angle = waiver(), n_dodge = 1, check_overlap = FALSE)
  ),

  extract_params = function(scale, params, ...) {
    params$position <- params$position %|W|% NULL
    params
  },

  transform = function(self, params, coord, panel_params) {
    params$key <-
      transform_key(params$key, params$position, coord, panel_params)
    params
  },

  setup_elements = function(params, elements, theme) {
    prefix <- ""
    suffix <- ""
    if (params$aesthetic %in% c("x", "y")) {
      suffix <- switch(
        params$position,
        theta = ".x.bottom", theta.sec = ".x.top",
        paste0(".", params$aesthetic, ".", params$position)
      )
      prefix <- "axis."
    } else {
      prefix <- "legend."
    }
    elements <- list(text = paste0(prefix, "text", suffix))
    if (is.null(params$stack_offset)) {
      elements$offset <- 0
    } else {
      elements$offset <- convertUnit(params$stack_offset, "cm", valueOnly = TRUE)
    }
    Guide$setup_elements(params, elements, theme)
  },

  override_elements = function(params, elements, theme) {
    elements$text <- angle_labels(elements$text, params$angle, params$position)
    elements
  },

  build_labels = function(key, elements, params) {
    if (".type" %in% names(key)) {
      key <- vec_slice(key, key$.type == "major")
    }

    n_labels <- nrow(key)

    if (n_labels < 1 || is_blank(elements$text)) {
      return(list(zeroGrob()))
    }

    dodge_value <- rep(seq_len(params$n_dodge %||% 1), length.out = n_labels)
    dodge_index <- unname(split(seq_len(n_labels), dodge_value))
    angle <- params$angle %|W|% NULL

    offset <- elements$offset
    grobs  <- list()

    for (i in seq_along(dodge_index)) {
      index <- dodge_index[[i]]
      grob  <- draw_labels(
        vec_slice(key, index), elements$text, angle, offset,
        params$position, check_overlap = params$check_overlap
      )
      offset <- offset + attr(grob, "size") %||% 0
      grobs[[i]] <- grob
    }
    if (params$position %in% c("top", "left")) grobs <- rev(grobs)

    attr(grobs, 'offset') <- offset - elements$offset
    grobs
  },

  measure_grobs = function(grobs, params, elements) {
    switch(
      params$position,
      top = , bottom = height_cm(grobs),
      left = , right = width_cm(grobs),
      attr(grobs, "offset")
    )
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    params <- replace_null(params, position = position, direction = direction)
    elems  <- self$setup_elements(params, self$elements, theme)
    elems  <- self$override_elements(params, elems, theme)
    labels <- self$build_labels(params$key, elems, params)
    size   <- self$measure_grobs(labels, params, elems)

    primitive_grob(labels, unit(size, "cm"), params$position, "labels")
  }

)

# Helpers -----------------------------------------------------------------

draw_labels <- function(key, element, angle, offset,
                        position, check_overlap = NULL) {

  aes <- switch(position, top = , bottom = "x", "y")
  margin_x <- switch(position, left = , right = TRUE, FALSE)
  margin_y <- switch(position, top = , bottom = TRUE, FALSE)

  n_breaks  <- nrow(key)

  if (check_overlap %||% FALSE) {
    order <- label_priority(n_breaks)
    key <- vec_slice(key, order)
  }

  just <- rotate_just(angle %||% element$angle, element$hjust, element$vjust)
  x <- switch(position, left = , right = just$hjust, key$x)
  y <- switch(position, top = , bottom = just$vjust, key$y)

  # Resolve positions
  x <- rep(x, length.out = n_breaks)
  y <- rep(y, length.out = n_breaks)
  if (!is.unit(x)) x <- unit(x, "npc")
  if (!is.unit(y)) y <- unit(y, "npc")

  if (position %in% .trbl) {
    # Classic labels
    grob <- element_grob(
      element = element,
      label = key$.label,
      x = x, y = y,
      margin_x = margin_x,
      margin_y = margin_y,
      check.overlap = check_overlap
    )
    return(grob)
  }

  # Theta labels
  if (is_null(angle)) {
    angle <- element$angle
  } else {
    angle <- flip_text_angle(angle - rad2deg(key$theta))
  }
  rad   <- deg2rad(angle)
  theta <- key$theta %||% (pi * switch(
    position, top = 0, bottom = 1, left = 1.5, right = 0.5
  ))

  margin <- convertUnit(max(element$margin), "cm", valueOnly = TRUE)
  offset <- offset + margin

  x <- x + unit(offset * sin(theta), "cm")
  y <- y + unit(offset * cos(theta), "cm")

  hjust <- 0.5 - sin(theta + rad) / 2
  vjust <- 0.5 - cos(theta + rad) / 2

  grob <- element_grob(
    element = element,
    label = key$.label,
    x = x, y = y,
    hjust = hjust, vjust = vjust,
    angle = angle,
    check.overlap = check_overlap
  )

  if (inherits(grob, "textpath")) {
    height <-
      measure_textpath_labels(grob)
  } else {
    height <-
      measure_theta_labels(element, key$.label, margin, theta + rad, hjust, vjust)
  }
  attr(grob, "size") <- height
  grob
}

measure_textpath_labels <- function(grob) {
  labels <- grob$textpath$label
  height <- vapply(labels, function(x) attr(x, "metrics")$height, numeric(1))
  max(height) * .in2cm
}

measure_theta_labels <- function(element, labels, margin, angle, hjust, vjust) {

  singles <- lapply(labels, function(lab) {
    element_grob(
      element, label = lab,
      margin = unit(rep(margin, length.out = 4), "cm"),
      margin_x = TRUE, margin_y = TRUE
    )
  })
  widths  <- width_cm(singles)
  heights <- height_cm(singles)

  xmin <- widths * -hjust
  xmax <- widths * (1 - hjust)

  ymin <- heights * -vjust
  ymax <- heights * (1 - vjust)

  x <- vec_interleave(xmin, xmin, xmax, xmax)
  y <- vec_interleave(ymin, ymax, ymax, ymin)

  angle <- rep(angle, each = 4)
  max(x * sin(angle) + y * cos(angle))
}

angle_labels <- function(element, angle, position) {
  if (!inherits(element, "element_text") || is_waive(angle) || is_null(angle)) {
    return(element)
  }

  # Initialise parameters
  angle <- angle %% 360
  hjust <- NULL
  vjust <- NULL

  if (position == "bottom") {

    hjust <- if (angle %in% c(0, 180))  0.5 else if (angle < 180) 1 else 0
    vjust <- if (angle %in% c(90, 270)) 0.5 else if (angle > 90 & angle < 270) 0 else 1

  } else if (position == "left") {

    hjust <- if (angle %in% c(90, 270)) 0.5 else if (angle > 90 & angle < 270) 0 else 1
    vjust <- if (angle %in% c(0, 180))  0.5 else if (angle < 180) 0 else 1

  } else if (position == "top") {

    hjust <- if (angle %in% c(0, 180))  0.5 else if (angle < 180) 0 else 1
    vjust <- if (angle %in% c(90, 270)) 0.5 else if (angle > 90 & angle < 270) 1 else 0

  } else if (position == "right") {

    hjust <- if (angle %in% c(90, 270)) 0.5 else if (angle > 90 & angle < 270) 1 else 0
    vjust <- if (angle %in% c(0, 180))  0.5 else if (angle < 180) 1 else 0

  }

  element$angle <- angle %||% element$angle
  element$hjust <- hjust %||% element$hjust
  element$vjust <- vjust %||% element$vjust

  element
}

validate_labels <- function(labels) {
  if (!is.list(labels)) {
    return(labels)
  }
  if (any(vapply(labels, is.language, logical(1)))) {
    do.call(expression, labels)
  } else {
    unlist(labels)
  }
}


label_priority <- function(n) {
  if (n <= 0) {
    return(numeric(0))
  }
  c(1, n, label_priority_between(1, n))
}

label_priority_between <- function(min, max) {
  n <- max - min + 1
  if (n <= 2) {
    return(numeric(0))
  }
  mid <- min - 1 + (n + 1) %/% 2
  c(mid, label_priority_between(min, mid), label_priority_between(mid, max))
}
