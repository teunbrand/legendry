# Constructor -------------------------------------------------------------

#' Guide primitive: labels
#'
#' This function constructs a labels [guide primitive][guide-primitives].
#'
#' @param key A [standard key][key_standard] specification. See more information
#'   in the linked topic.
#' @param n.dodge An positive `<integer[1]>` setting the number of layers text
#'   labels can occupy to avoid overlapping labels.
#' @param check.overlap A `<logical[1]>` indicating whether to check for and
#'   omit overlapping text. If `TRUE`, first, last and middle labels are
#'   recursively prioritised in that order. If `FALSE`, all labels are drawn.
#' @inheritParams common_parameters
#'
#' @return A `<PrimitiveLabels>` primitive guide that can be used inside other
#'   guides.
#' @export
#' @family primitives
#'
#' @details
#' # Styling options
#'
#' Below are the [theme][ggplot2::theme] options that determine the styling of
#' this guide, which may differ depending on whether the guide is used in
#' an axis or in a legend context.
#'
#' ## As an axis guide
#'
#' * `axis.text.{x/y}.{position}` an [`<element_text>`][ggplot2::element_text]
#'   for the display of the labels.
#'
#' ## As a legend guide.
#'
#' * `legend.text` an [`<element_text>`][ggplot2::element_text] for the display
#'   of the labels.
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'  geom_point()
#'
#' # Adding as secondary guides
#' p + guides(
#'   x.sec = primitive_labels(),
#'   y.sec = primitive_labels(n.dodge = 2)
#' )
primitive_labels <- function(key = NULL, angle = waiver(), n.dodge = 1L,
                             check.overlap = FALSE,
                             theme = NULL, position = waiver()) {
  if (!is_waive(angle)) {
    check_number_decimal(
      angle, min = -360.0, max = 360.0,
      allow_infinite = FALSE, allow_null = TRUE
    )
  }
  check_number_whole(n.dodge, min = 1.0)
  check_bool(check.overlap)

  new_guide(
    angle = angle,
    n_dodge = n.dodge,
    check_overlap = check.overlap,
    key = key,
    theme = theme,
    position = position,
    available_aes = c("any", "x", "y", "r", "theta"),
    super = PrimitiveLabels
  )
}


# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
PrimitiveLabels <- ggproto(
  "PrimitiveLabels", Guide,

  params = new_params(
    angle = waiver(), n_dodge = 1L, check_overlap = FALSE, key = NULL
  ),

  elements = list(
    position = list(text = "axis.text"),
    legend   = list(text = "legend.text")
  ),

  hashables = exprs(key$.label),

  extract_params = primitive_extract_params,

  extract_key = standard_extract_key,

  transform = function(self, params, coord, panel_params) {
    params$key <-
      transform_key(params$key, params$position, coord, panel_params)
    params
  },

  setup_params = primitive_setup_params,

  setup_elements = primitive_setup_elements,

  override_elements = function(params, elements, theme) {
    elements$text <- angle_labels(elements$text, params$angle, params$position)
    elements
  },

  build_labels = function(key, elements, params) {
    if (".type" %in% names(key)) {
      key <- vec_slice(key, key$.type == "major")
    }

    n_labels <- nrow(key)

    if (n_labels < 1L || is_blank(elements$text)) {
      return(list(zeroGrob()))
    }

    dodge_value <- rep_len(seq_len(params$n_dodge %||% 1L), n_labels)
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
      offset <- offset + get_size_attr(grob)
      grobs[[i]] <- grob
    }
    if (params$position %in% c("top", "left")) grobs <- rev(grobs)

    attr(grobs, "offset") <- offset - elements$offset
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
    params <- self$setup_params(params)

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

  n_breaks  <- length(key$.label)
  if (n_breaks < 1L || is_blank(element)) {
    return(zeroGrob())
  }

  margin_x <- switch(position, left = , right = TRUE, FALSE)
  margin_y <- switch(position, top = , bottom = TRUE, FALSE)
  hjust <- vjust <-  NULL

  check_overlap <- check_overlap %||% FALSE
  if (check_overlap) {
    order <- label_priority(n_breaks)
    key <- vec_slice(key, order)
  }

  just <- rotate_just(angle, element = element)
  x <- switch(position, left = , right = just$hjust, key$x)
  y <- switch(position, top = , bottom = just$vjust, key$y)
  just <- NULL

  # Resolve positions
  x <- rep_len(x, n_breaks)
  y <- rep_len(y, n_breaks)
  if (!is.unit(x)) x <- unit(x, "npc")
  if (!is.unit(y)) y <- unit(y, "npc")

  labels <- validate_labels(key$.label)

  # Theta labels
  if (is_theta(position)) {
    theta <- get_theta(key, position)

    if (is_null(angle)) {
      # Single verbatim angle from theme
      angle <- element$angle
    } else {
      # Adaptive angle based on theta
      angle <- flip_text_angle(angle - rad2deg(theta))
    }
    angle_radians <- deg2rad(angle)
    text_radians <- angle_radians + theta

    margin <- cm(max(element$margin))
    offset <- offset + margin

    x <- x + unit(offset * sin(theta), "cm")
    y <- y + unit(offset * cos(theta), "cm")

    hjust <- 0.5 - sin(text_radians) / 2.0
    vjust <- 0.5 - cos(text_radians) / 2.0
  }

  props <- element_key_properties(
    key, "text",
    hjust = hjust,
    vjust = vjust,
    angle = angle
  )

  grob <- inject(element_grob(
    element = element,
    label = labels,
    x = x, y = y,
    !!!props,
    margin_x = margin_x,
    margin_y = margin_y,
    check.overlap = check_overlap
  ))

  if (!is_theta(position)) {
    return(grob)
  }
  # For composition purposes we need to keep track of the text size along
  # the radial axis, not just simply width/height.
  height <- if (inherits(grob, "textpath")) {
    measure_textpath_labels(grob)
  } else {
    measure_theta_labels(element, labels, margin, text_radians, props)
  }
  attr(grob, "size") <- height
  grob
}

measure_textpath_labels <- function(grob) {
  labels <- grob$textpath$label
  height <- map_dbl(labels, function(x) attr(x, "metrics")$height)
  max(height) * .in2cm
}

measure_theta_labels <- function(element, labels, margin, angle, params) {

  n_labels <- length(labels)
  hjust <- params$hjust %||% 0.5
  vjust <- params$vjust %||% 0.5
  size  <- params$size

  singles <- lapply(seq_len(n_labels), function(i) {
    element_grob(
      element, label = labels[[i]],
      margin = margin(),
      size = size[[i]]
    )
  })

  widths  <- width_cm(singles)
  heights <- height_cm(singles)

  xmin <- widths * -hjust
  xmax <- widths * (1.0 - hjust)

  ymin <- heights * -vjust
  ymax <- heights * (1.0 - vjust)

  x <- vec_interleave(xmin, xmin, xmax, xmax)
  y <- vec_interleave(ymin, ymax, ymax, ymin)

  angle <- rep(angle, each = 4L)
  max(x * sin(angle) + y * cos(angle), na.rm = TRUE) + max(cm(margin))
}

label_priority <- function(n) {
  if (n <= 0L) {
    return(numeric(0L))
  }
  c(1L, n, label_priority_between(1L, n))
}

label_priority_between <- function(min, max) {
  n <- max - min + 1L
  if (n <= 2L) {
    return(numeric(0L))
  }
  mid <- min - 1L + (n + 1L) %/% 2L
  c(mid, label_priority_between(min, mid), label_priority_between(mid, max))
}

get_theta <- function(key, position) {
  key$theta %||% rep(
    switch(
      position,
      top    = 0.0,
      bottom = 1.0,
      left   = 1.5,
      right  = 0.5
    ),
    nrow(key)
  )
}
