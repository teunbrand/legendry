#' Vanilla grid guide
#'
#' This guide will draw a regular panel grid, with optional break adjustments.
#'
#' @param breaks The default major breaks specification inherited by the
#'   `x_breaks` and `y_breaks` arguments.
#' @param minor_breaks The default minor breaks specification inherited by the
#'   `x_minor_breaks` and `y_minor_breaks` arguments.
#' @param x_breaks,y_breaks Specifications for major breaks.
#'   Inherits from the `breaks` argument. One of:
#'   * `NULL` for no breaks.
#'   * `waiver()` for default breaks computed by the scale.
#'   * A `numeric` vector for positions. For discrete scales, can also be a
#'   `character` vector.
#'   * A `function` that takes the limits as input and returns breaks as
#'   output. Also accepts [lambda][rlang::as_function()] function notation.
#' @param x_minor_breaks,y_minor_breaks Specifications of minor breaks. Inherits
#'   from the `minor_breaks` argument. One of:
#'   * `NULL` for no minor breaks.
#'   * `waiver()` for default minor breaks computed by the scale.
#'   * A `numeric` vector for positions. For discrete scales, can also be a
#'   `character` vector.
#'   * A `function` that takes the limits as input and returns a vector of minor
#'   breaks as output. Also accepts [lambda][rlang::as_function()] function
#'   notation.
#' @param ... Currently not in use.
#'
#' @return A `<Guide>` ggproto object that can be given to the
#'   [`guides()`][ggplot2::guides()] function, or set as the `guide` argument
#'   in [`coord_guided()`].
#' @export
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(class, displ)) +
#'   geom_boxplot()
#'
#' # Giving a vector of x-breaks between discrete categories
#' p + coord_guided(guide = guide_grid(x_breaks = 0.5:7.5))
#'
#' # Identical to above, but passing a function for the x-breaks, and using
#' # the `guides()` interface.
#' p + coord_guided() +
#'   guides(grid = guide_grid(x_breaks = breaks_between()))
guide_grid <- function(
  breaks         = waiver(),
  minor_breaks   = waiver(),
  x_breaks       = waiver(),
  y_breaks       = waiver(),
  x_minor_breaks = waiver(),
  y_minor_breaks = waiver(),
  ...
) {

  # Trick to easily inherit from this guide
  args <- list2(...)
  super <- args$super %||% GuideGrid
  args$super <- NULL

  new_guide(
    x_breaks = allow_lambda(x_breaks %|W|% breaks),
    y_breaks = allow_lambda(y_breaks %|W|% breaks),
    x_minor_breaks = allow_lambda(x_minor_breaks %|W|% minor_breaks),
    y_minor_breaks = allow_lambda(y_minor_breaks %|W|% minor_breaks),
    name  = "grid",
    available_aes = c("x", "y", "grid"),
    !!!args,
    super = super
  )
}

GuideGrid <- ggproto(
  "GuideGrid", Guide,

  params = list2(
    x_breaks = NULL,
    y_breaks = NULL,
    x_minor_breaks = NULL,
    y_minor_breaks = NULL,
    !!!Guide$params
  ),

  elements = list(
    x_major = "panel.grid.major.x",
    x_minor = "panel.grid.minor.x",
    y_major = "panel.grid.major.y",
    y_minor = "panel.grid.minor.y",
    background = "panel.background"
  ),

  extract_key = function(scale, aesthetic, x_breaks, y_breaks, x_minor_breaks,
                         y_minor_breaks, ...) {
    # Get breaks from scale
    x <- scale_grid_extract(x_breaks, x_minor_breaks, scale$x)
    y <- scale_grid_extract(y_breaks, y_minor_breaks, scale$y)

    # Format as data.frames for transform method
    list(
      x_major = data_frame0(x = x$major),
      x_minor = data_frame0(x = x$minor),
      y_major = data_frame0(y = y$major),
      y_minor = data_frame0(y = y$minor)
    )
  },

  transform = function(self, params, coord, panel_params) {
    # Transform every key
    params$key <- lapply(params$key, coord$transform,
                         panel_params = panel_params)
    # Discard empty keys
    params$key <- params$key[list_sizes(params$key) > 0]
    params
  },

  # The draw method is a simplified version of `Guide$draw()` because we don't
  # need to bother with labels, titles, measurements, arrangements or any of
  # that.
  draw = function(self, theme, params = self$params) {

    key <- params$key
    params <- self$setup_params(params)
    elems <- self$setup_elements(params, self$elements, theme)
    elems <- self$override_elements(params, elems, theme)

    if (sum(list_sizes(key)) < 1) {
      out <- self$draw_early_exit(params, elems)
      return(out)
    }

    grobs <- list(
      background = self$build_decor(elements = elems, params = params),
      grid = self$build_ticks(key, elems, params)
    )
    grobs <- grobs[lengths(grobs) > 0]

    grob <- do.call(grobTree, grobs)
    grob$name <- grobName(grob, "grill")
    grob
  },

  build_decor = function(decor, grobs, elements, params) {
    element_grob(elements$background)
  },

  build_ticks = function(key, elements, params, position = params$position) {

    if (sum(list_sizes(key)) == 0) {
      return(NULL)
    }

    grobs <- lapply(key, function(df) {
      aes   <- names(df)
      other <- setdiff(c("x", "y"), aes)
      var <- df[[1]]

      list2(
        !!aes   := unit(rep(var, each = 2), "npc"),
        !!other := unit(rep(c(0, 1), times = length(var)), "npc"),
        id.lengths = rep(2, length(var))
      )
    })

    grobs <- Map(
      function(args, elem) {
        element_grob(elem, x = args$x, y = args$y, id.lengths = args$id.lengths)
      },
      args = grobs,
      elem = elements[names(key)]
    )
    do.call(grobTree, grobs)
  },

  draw_early_exit = function(self, params, elements) {
    grob <- self$build_decor(elements = elements)
    grob <- grobTree(grob)
    grob$name <- grobName(grob, "grill")
    grob
  }
)

#' Breaks between discrete categories
#'
#' This is a function factory that returns a function for placing breaks in
#' between discrete categories for use in grid guides.
#'
#' @param extremes A `logical(1)` whether breaks should be placed before and
#'   after the first and last category. If `TRUE`, these are returned.
#'   If `FALSE`, these are omitted.
#'
#' @return A `function` that can be passed to grid guides.
#' @export
#' @note
#' This function should *not* be used for `scale_{aes}_{type}(breaks = ...)`
#' input.
#'
#' @examples
#' # By default, before and after breaks are included
#' breaks_between()(c("A", "B", "C"))
#'
#' # Only between categories
#' breaks_between(FALSE)(c("A", "B", "C"))
#'
#' # Continuous input returns waiver
#' breaks_between()(1:3)
breaks_between <- function(extremes = TRUE) {
  force(extremes)
  check_bool(extremes)
  function(limits) {
    if (!is_discrete(limits)) {
      return(waiver())
    }
    if (isTRUE(extremes)) {
      c(0.5, seq_along(limits) + 0.5)
    } else {
      c(seq_len(pmax(length(limits) - 1, 0))) + 0.5
    }
  }
}
