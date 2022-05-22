# Borrowed internals ------------------------------------------------------

# The functions in this document are either literally copied, or copied with
# modification from ggplot2. The copyright of these functions belong to the
# ggplot2 authors. These functions are borrowed under the MIT licence that
# applies to the ggplot2 package and can be found at the link below:
# https://ggplot2.tidyverse.org/LICENSE.html

# ggplot2:::warn_for_guide_position()
check_position <- function(guide) {
  key <- guide$key
  breaks_are_unique <- !duplicated(key$.value)
  empty <- is.null(key) || nrow(key) == 0 || ncol(key) == 0
  empty <- empty || inherits(key, "waiver")
  if (empty || sum(breaks_are_unique) == 1) {
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

# ggplot2:::justify_grobs(), but with `theme` as a convenience argument
justify_grobs <- function(
    grobs,
    x = NULL,
    y = NULL,
    hjust = NULL,
    vjust = NULL,
    angle = NULL,
    theme,
    debug = NULL
) {
  if (!inherits(grobs, "grob")) {
    if (is.list(grobs)) {
      return(lapply(grobs, justify_grobs, x, y, hjust, vjust,
                    angle, theme, debug))
    } else {
      abort("Need individual grobs or list of grobs as argument.")
    }
  }

  if (inherits(grobs, "zeroGrob")) {
    return(grobs)
  }

  just <- rotate_just(
    angle = angle %||% theme$angle %||% 0,
    hjust = hjust %||% theme$hjust %||% 0.5,
    vjust = vjust %||% theme$vjust %||% 0.5
  )

  x <- x %||% unit(just$hjust, "npc")
  y <- y %||% unit(just$vjust, "npc")

  debug <- debug %||% theme$debug %||% FALSE
  if (isTRUE(debug)) {
    children <- gList(
      rectGrob(gp = gpar(fill = "lightcyan", col = NA)),
      grobs
    )
  } else {
    children = gList(grobs)
  }

  result_grob <- gTree(
    children = children,
    vp = viewport(
      x      = x,
      y      = y,
      width  = grobWidth(grobs),
      height = grobHeight(grobs),
      just   = unlist(just)
    )
  )

  if (isTRUE(debug)) {
    grobTree(
      result_grob,
      pointsGrob(x, y, pch = 20, gp = gpar(col = "mediumturquoise"))
    )
  } else {
    result_grob
  }
}

# ggplot2:::rotate_just()
rotate_just <- function(angle, hjust, vjust) {
  angle <- (angle %||% 0) %% 360
  if (0 <= angle & angle < 90) {
    hnew <- hjust
    vnew <- vjust
  } else if (90 <= angle & angle < 180) {
    hnew <- 1 - vjust
    vnew <- hjust
  } else if (180 <= angle & angle < 270) {
    hnew <- 1 - hjust
    vnew <- 1 - vjust
  } else if (270 <= angle & angle < 360) {
    hnew <- vjust
    vnew <- 1 - hjust
  }
  list(hjust = hnew, vjust = vnew)
}

# ggplot2:::width_cm()
width_cm <- function(x) {
  if (is.grob(x)) {
    convertWidth(grobWidth(x), "cm", TRUE)
  } else if (is.unit(x)) {
    convertWidth(x, "cm", TRUE)
  } else if (is.list(x)) {
    vapply(x, width_cm, numeric(1))
  } else {
    abort("Cannot determine width.")
  }
}

# ggplot2:::height_cm()
height_cm <- function(x) {
  if (is.grob(x)) {
    convertHeight(grobHeight(x), "cm", TRUE)
  } else if (is.unit(x)) {
    convertHeight(x, "cm", TRUE)
  } else if (is.list(x)) {
    vapply(x, height_cm, numeric(1))
  } else {
    abort("Cannot determine height.")
  }
}

# ggplot2:::modify_list()
modify_list <- function(old, new) {
  for (i in names(new)) old[[i]] <- new[[i]]
  old
}

# ggplot2:::is.waive
is_waive <- function(x) inherits(x, "waiver")

# ggplot2:::combine_elements
combine_elements <- function(e1, e2) {

  # If e2 is NULL, nothing to inherit
  if (is.null(e2) || inherits(e1, "element_blank")) {
    return(e1)
  }

  # If e1 is NULL inherit everything from e2
  if (is.null(e1)) {
    return(e2)
  }

  # If neither of e1 or e2 are element_* objects, return e1
  if (!inherits(e1, "element") && !inherits(e2, "element")) {
    return(e1)
  }

  # If e2 is element_blank, and e1 inherits blank inherit everything from e2,
  # otherwise ignore e2
  if (inherits(e2, "element_blank")) {
    if (e1$inherit.blank) {
      return(e2)
    } else {
      return(e1)
    }
  }

  # If e1 has any NULL properties, inherit them from e2
  n <- names(e1)[vapply(e1, is.null, logical(1))]
  e1[n] <- e2[n]

  # Calculate relative sizes
  if (inherits(e1$size, "rel")) {
    e1$size <- e2$size * unclass(e1$size)
  }

  e1
}

# ggplot2:::rename_aes
rename_aes <- function(x) {
  names(x) <- standardise_aes_names(names(x))
  duplicated <- names(x)[duplicated(names(x))]
  if (length(duplicated) > 0L) {
    udup <- unique(duplicated)
    warn(paste0(
      "Duplicated aesthetics after name standardisation: ",
      glue_collapse(udup, sep = ", ", last = " and "), '.'
    ))
  }
  x
}
