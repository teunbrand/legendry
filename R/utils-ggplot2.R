# Borrowed ggplot2 internals ----------------------------------------------

# The functions in this document are either literally copied, or copied with
# modification from ggplot2. The copyright of these functions belong to the
# ggplot2 authors. These functions are borrowed under the MIT licence that
# applies to the ggplot2 package and can be found at the link below:
# https://ggplot2.tidyverse.org/LICENSE.html

# nocov start

is_waive <- function(x) inherits(x, "waiver")

`%|W|%` <- function(x, y) if (is_waive(x)) y else x

opposite_position <- function(position) {
  switch(
    position,
    top    = "bottom",
    bottom = "top",
    left   = "right",
    right  = "left"
  )
}

absoluteGrob <- function(grob, width = NULL, height = NULL,
                         xmin = NULL, ymin = NULL, vp = NULL) {
  gTree(
    children = grob,
    width = width, height = height,
    widths = width, heights = height,
    xmin = xmin, ymin = ymin,
    vp = vp, cl = "absoluteGrob"
  )
}

unique0 <- function(x, ...) if (is.null(x)) x else vec_unique(x, ...)

is_empty <- function(df) {
  length(df) == 0 || nrow(df) == 0 || is_waive(df)
}

is_zero <- function(x) is.null(x) || inherits(x, "zeroGrob")

replace_null <- function(obj, ..., env = caller_env()) {
  dots <- enexprs(...)
  nms  <- names(dots)
  nms  <- nms[map_lgl(.subset(obj, nms), is.null)]
  obj[nms] <- inject(list(!!!dots[nms]), env = env)
  obj
}

.rad2deg <- 180 / pi
rad2deg <- function(rad) rad * .rad2deg

.deg2rad <- pi / 180
deg2rad <- function(deg) deg * .deg2rad

flip_text_angle <- function(angle) {
  angle <- angle %% 360
  flip  <- angle > 90 & angle < 270
  angle[flip] <- angle[flip] + 180
  angle
}

width_cm <- function(x) {
  if (is.grob(x)) x <- grobWidth(x)
  if (is.unit(x)) {
    convertWidth(x, "cm", valueOnly = TRUE)
  } else if (is.list(x)) {
    map_dbl(x, width_cm)
  } else {
    cli::cli_abort("Don't know how to get width of {.cls {class(x)}} object.")
  }
}

height_cm <- function(x) {
  if (is.grob(x)) x <- grobHeight(x)
  if (is.unit(x)) {
    convertHeight(x, "cm", TRUE)
  } else if (is.list(x)) {
    map_dbl(x, height_cm)
  } else {
    cli::cli_abort("Don't know how to get height of {.cls {class(x)}} object.")
  }
}

data_frame0 <- function(...) data_frame(..., .name_repair = "minimal")

find_global <- function(name, env, mode = "any") {
  if (exists(name, envir = env, mode = mode)) {
    return(get(name, envir = env, mode = mode))
  }
  nsenv <- asNamespace("legendry")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }
  nsenv <- asNamespace("ggplot2")
  if (exists(name, envir = nsenv, mode = mode)) {
    return(get(name, envir = nsenv, mode = mode))
  }
  NULL
}

is_mapped_discrete <- function(x) inherits(x, "mapped_discrete")

as_cli <- function(..., env = caller_env()) {
  cli::cli_fmt(cli::cli_text(..., .envir = env))
}

rotate_just <- function(angle = NULL, hjust = NULL, vjust = NULL, element = NULL) {
  angle <- (angle %||% 0) %% 360

  if (!is.null(element)) {
    angle <- element$angle
    hjust <- element$hjust
    vjust <- element$vjust
  }

  # Apply recycle rules
  size  <- vec_size_common(angle, hjust, vjust)
  angle <- vec_recycle(angle, size)
  hjust <- vec_recycle(hjust, size)
  vjust <- vec_recycle(vjust, size)

  # Find quadrant on circle
  case <- findInterval(angle, c(0, 90, 180, 270, 360))

  hnew <- hjust
  vnew <- vjust

  is_case <- which(case == 2) # 90 <= x < 180
  hnew[is_case] <- 1 - vjust[is_case]
  vnew[is_case] <- hjust[is_case]

  is_case <- which(case == 3) # 180 <= x < 270
  hnew[is_case] <- 1 - hjust[is_case]
  vnew[is_case] <- 1 - vjust[is_case]

  is_case <- which(case == 4) # 270 <= x < 360
  hnew[is_case] <- vjust[is_case]
  vnew[is_case] <- 1 - hjust[is_case]

  list(hjust = hnew, vjust = vnew)
}

combine_elements <- function(e1, e2) {
  if (inherits(e1, "S7_object") || inherits(e2, "S7_object")) {
    # TODO: this is a dirty hack that should be resolved at some point
    combine <- utils::getFromNamespace("combine_elements", asNamespace("ggplot2"))
    return(combine(e1, e2))
  }

  if (is.null(e2) || is_blank(e2)) {
    return(e1)
  }
  if (is.null(e1)) {
    return(e2)
  }
  if (is_rel(e1)) {
    if (is_rel(e2)) {
      return(rel(unclass(e1) * unclass(e2)))
    }
    if (is.numeric(e2) || is.unit(e2)) {
      return(unclass(e1) * e2)
    }
    return(e1)
  }
  if (!is_theme_element(e1) && !is_theme_element(e2)) {
    return(e1)
  }
  if (is_blank(e2)) {
    out <- if (e1$inherit.blank) e2 else e1
    return(out)
  }
  n <- names(e1)[map_lgl(e1, is.null)]
  e1[n] <- e2[n]

  if (is_rel(e1$size)) {
    e1$size <- e2$size * unclass(e1$size)
  }
  if (is_rel(e1$linewidth)) {
    e1$linewidth <- e2$linewidth * unclass(e1$linewidth)
  }
  if (is.subclass(e2, e1)) {
    new <- defaults(e1, e2)
    e2[names(new)] <- new
    return(e2)
  }
  e1
}

`%0%` <- function(e1, e2) if (length(e1) == 0) e2 else e1

is_rel <- function(x) inherits(x, "rel")

defaults <- function(x, y) c(x, y[setdiff(names(y), names(x))])

is.subclass <- function(x, y) {{
  inheritance <- inherits(x, class(y), which = TRUE)
  !any(inheritance == 0) && length(setdiff(class(x), class(y))) > 0
}}

get_key_size <- function(keys, which = "width", n) {
  size <- lapply(keys, attr, which = which)
  size[lengths(size) != 1] <- 0
  size <- matrix(unlist(size), ncol = n)
  apply(size, 2, max)
}

polar_bbox <- function(arc, margin = c(0.05, 0.05, 0.05, 0.05),
                       inner_radius = c(0, 0.4)) {
  if (abs(diff(arc) >= 2 * pi)) {
    return(list(x = c(0, 1), y = c(0, 1)))
  }
  xmax <- 0.5 * sin(arc) + 0.5
  ymax <- 0.5 * cos(arc) + 0.5
  xmin <- inner_radius[1] * sin(arc) + 0.5
  ymin <- inner_radius[1] * cos(arc) + 0.5
  margin <- rep(margin, length.out = 4)
  margin <- c(
    max(ymin) + margin[1],
    max(xmin) + margin[2],
    min(ymin) - margin[3],
    min(xmin) - margin[4]
  )
  pos_theta <- c(0, 0.5, 1, 1.5) * pi
  in_sector <- in_arc(pos_theta, arc)
  bounds <- ifelse(
    in_sector,
    c(1, 1, 0, 0),
    c(max(ymax, margin[1]), max(xmax, margin[2]),
      min(ymax, margin[3]), min(xmax, margin[4]))
  )
  list(x = c(bounds[4], bounds[2]), y = c(bounds[3], bounds[1]))
}

rename_aes <- function(x, arg = caller_arg(x)) {
  force(arg)
  names(x) <- standardise_aes_names(names(x))
  dups <- names(x)[duplicated(names(x))]
  if (length(dups) > 0L) {
    cli::cli_warn(
      "Duplicated aesthetics in {.arg {arg}} after name standardisation: \\
      {.field {unique(dups)}}."
    )
  }
  x
}

in_arc <- function(theta, arc) {
  if (abs(diff(arc)) > 2 * pi - sqrt(.Machine$double.eps)) {
    return(rep(TRUE, length(theta)))
  }
  arc <- arc %% (2 * pi)
  if (arc[1] < arc[2]) {
    in_range(theta, arc)
  } else {
    !(theta < arc[1] & theta > arc[2])
  }
}

# nocov end
