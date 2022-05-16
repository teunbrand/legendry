# ggplot2:::warn_for_guide_position()
check_position <- function(guide) {
  key <- guide$key
  breaks_are_unique <- !duplicated(key$.value)
  empty <- is.null(key) || nrow(key) == 0 || ncol(key) == 0
  empty <- empty || inherits(key, "waiver")
  if (empty || sum(breaks_are_unique) == 0) {
    return()
  }

  if (guide$position %in% c("top", "bottom")) {
    pos_aes <- "x"
  } else if (guide$position %in% c("left", "right")) {
    pos_aes <- "y"
  } else {
    return()
  }

  if (length(unique(key[[pos_aes]][breaks_are_unique])) == 1) {
    warn(paste0(
      "Position guide is perpendicular to the intended axis. ",
      "Did you mean to specify a different guide `position`?"
    ))
  }
}

# ggplot2:::snake_class() + ggplot2:::snakeize()
snake_class <- function(x) {
  x <- class(x)[1]
  x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
  x <- gsub(".", "_", x, fixed = TRUE)
  x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
  chartr(
    paste0(LETTERS, collapse = ""),
    paste0(letters, collapse = ""),
    x
  )
}

# ggplot2:::axis_label_element_overrides()
override_axis_label <- function(axis_position, angle = NULL) {
  if (is.null(angle)) {
    return(element_text(angle = NULL, hjust = NULL, vjust = NULL))
  }

  # it is not worth the effort to align upside-down labels properly
  if (angle > 90 || angle < -90) {
    abort("`angle` must be between 90 and -90")
  }

  if (axis_position == "bottom") {
    element_text(
      angle = angle,
      hjust = if (angle > 0) 1 else if (angle < 0) 0 else 0.5,
      vjust = if (abs(angle) == 90) 0.5 else 1
    )
  } else if (axis_position == "left") {
    element_text(
      angle = angle,
      hjust = if (abs(angle) == 90) 0.5 else 1,
      vjust = if (angle > 0) 0 else if (angle < 0) 1 else 0.5,
    )
  } else if (axis_position == "top") {
    element_text(
      angle = angle,
      hjust = if (angle > 0) 0 else if (angle < 0) 1 else 0.5,
      vjust = if (abs(angle) == 90) 0.5 else 0
    )
  } else if (axis_position == "right") {
    element_text(
      angle = angle,
      hjust = if (abs(angle) == 90) 0.5 else 0,
      vjust = if (angle > 0) 1 else if (angle < 0) 0 else 0.5,
    )
  } else {
    abort(glue("Unrecognized position: '{axis_position}'"))
  }
}

# ggplot2:::axis_label_priority()
prioritise_labels <- function(n) {
  if (n <= 0) {
    return(numeric(0))
  }
  c(1, n, label_priority(1, n))
}

# ggplot2:::axis_label_priority_between()
label_priority <- function(x, y) {
  n <- y - x + 1
  if (n <= 2) {
    return(numeric(0))
  }

  mid <- x - 1 + (n + 1) %/% 2
  c(
    mid,
    label_priority(x, mid),
    label_priority(mid, y)
  )
}
