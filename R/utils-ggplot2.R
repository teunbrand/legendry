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

.trbl <- c("top", "right", "bottom", "left")

absoluteGrob <- function(grob, width = NULL, height = NULL,
                         xmin = NULL, ymin = NULL, vp = NULL) {
  gTree(
    children = grob,
    width = width, height = height,
    xmin = xmin, ymin = ymin,
    vp = vp, cl = "absoluteGrob"
  )
}

replace_null <- function(obj, ..., env = caller_env()) {
  dots <- enexprs()
  nms  <- names(dots)
  nms  <- nms[vapply(obj[nms], is.null, logical(1))]
  obj[nms] <- inject(list(!!!dots[nms]), env = env)
  obj
}

# nocov end
