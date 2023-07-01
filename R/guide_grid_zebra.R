# Constructor -------------------------------------------------------------

#' Zebra grid
#'
#' This guide will draw the panel grid as alternating stripes, resembling the
#' stripes of a zebra, like the pedestrian crosswalk.
#'
#' @param direction Either `"x"` or `"y"` or `"both"` indicating in which
#'   direction the zebra stripes should be drawn. If not `"both"`, the
#'   orthogonal direction will have regular grid lines.
#' @param rect An `element_rect` or `element_blank` object to display the zebra
#'   stripes. Internally inherits from the `panel.grid.major` theme setting,
#'   where the line's `colour` field becomes the rectangle's `fill` field.
#' @param rect_x,rect_y Inherits from the `rect` argument, but for the x and y
#'   directions respectively.
#' @param at One of `"major"` (default), `"minor"` or `"both"` indicating
#'   between which breaks to display stripes.
#' @param at_x,at_y Inherits from the `at` argument, but for the x and y
#'   directions respectively.
#' @param odd A `logical(1)` whether to fill the odd stripes (`TRUE`, default)
#'   or the even stripes (`FALSE`).
#' @param odd_x,odd_y Inherits from the `odd` argument, but for the x and y
#'   directions respectively.
#' @inheritParams guide_grid
#' @inheritDotParams guide_grid -x_breaks -y_breaks
#'
#' @return A `<Guide>` ggproto object that can be given to the
#'   [`guides()`][ggplot2::guides()] function, or set as the `guide` argument
#'   in [`coord_guided()`].
#' @export
#' @details
#' For discrete scales, the function internally default to placing the major
#' breaks in the `direction` dimension in between levels. To restore classic
#' major breaks at discrete scales, one can set the relevant
#' `breaks_{x/y} = seq_along`.
#' @family grid guide variants
#'
#' @examples
#' # A standard plot
#' p <- ggplot(economics, aes(date, unemploy)) +
#'   geom_line()
#'
#' # Adding a zebra grid
#' p + coord_guided(guide = "grid_zebra")
#'
#' # Filling the even stripes
#' p + coord_guided(guide = guide_grid_zebra(odd = FALSE))
#'
#' # Including the minor breaks for zebra stripes
#' p + coord_guided(guide = guide_grid_zebra(at = "minor"))
#'
#' # Customising the look of the stripes
#' p + coord_guided(guide = guide_grid_zebra(
#'   rect = element_rect(fill = "grey95", colour = "white", linetype = "dotted")
#' ))
#'
#' # Using zebra stripes as a gingham pattern
#' p + coord_guided(guide = guide_grid_zebra(direction = 'both', at = "minor")) +
#'   theme(panel.grid.major = element_line(colour = alpha("white", 0.5)))
guide_grid_zebra <- function(
  direction = "x",
  rect      = element_rect(),
  rect_x    = NULL,
  rect_y    = NULL,
  at        = "major",
  at_x      = NULL,
  at_y      = NULL,
  odd       = TRUE,
  odd_x     = NULL,
  odd_y     = NULL,
  breaks    = waiver(),
  x_breaks  = waiver(),
  y_breaks  = waiver(),
  ...
) {

  # Resolve odd/even stripes
  odd_x <- odd_x %||% odd
  odd_y <- odd_y %||% odd
  check_bool(odd_x)
  check_bool(odd_y)

  # Resolve direction
  direction   <- arg_match0(direction, c("x", "y", "both"))
  x_direction <- direction %in% c("x", "both")
  y_direction <- direction %in% c("y", "both")

  # Override default breaks
  # Note that `breaks_between()` will only act on discrete limits, not
  # continuous ones.
  if (x_direction) {
    x_breaks <- x_breaks %|W|% breaks %|W|% breaks_between()
  }
  if (y_direction) {
    y_breaks <- y_breaks %|W|% breaks %|W|% breaks_between()
  }

  # Inherit from `rect`.
  rect_x <- combine_elements(rect_x, rect)
  rect_y <- combine_elements(rect_y, rect)
  arg_class(rect_x, .rect_or_blank)
  arg_class(rect_y, .rect_or_blank)

  # Resolve which breaks
  at_x <- at_x %||% at
  at_y <- at_y %||% at
  at_x <- arg_match(at_x, c("major", "minor", "both"))
  at_y <- arg_match(at_y, c("major", "minor", "both"))

  guide_grid(
    x_direction = x_direction,
    y_direction = y_direction,
    odd_x       = odd_x,
    odd_y       = odd_y,
    rect_x      = rect_x,
    rect_y      = rect_y,
    at_x        = at_x,
    at_y        = at_y,
    breaks      = breaks,
    x_breaks    = x_breaks,
    y_breaks    = y_breaks,
    ...,
    super       = GuideGridZebra
  )

}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideGridZebra <- ggproto(
  "GuideGridZebra", GuideGrid,

  params = c(GuideGrid$params, list(
    x_direction = TRUE,
    y_direction = FALSE,
    rect_x = element_rect(),
    rect_y = element_rect(),
    odd_x  = TRUE,
    odd_y  = TRUE,
    at_x   = "major",
    at_y   = "major"
  )),

  override_elements = function(params, elements, theme) {
    # Since `element_rect` can't inherit from `element_line` directly,
    # we're simply going to fill in `NULL`s from the line.
    major <- elements$x_major
    elements$rect_x <- update_element(
      params$rect_x,
      fill      = major$colour,
      linewidth = major$linewidth,
      linetype  = major$linetype,
      colour    = NA
    )

    major <- elements$y_major
    elements$rect_y <- update_element(
      params$rect_y,
      fill      = major$colour,
      linewidth = major$linewidth,
      linetype  = major$linetype,
      colour    = NA
    )
    elements
  },

  build_ticks = function(key, elements, params, position = params$position) {

    # If we're not drawing rectangles, forward to regular gridlines
    no_zebra <- list()
    if (!params$x_direction) {
      no_zebra <- update_list(no_zebra, key[c("x_major", "x_minor")])
    }
    if (!params$y_direction) {
      no_zebra <- update_list(no_zebra, key[c("y_major", "y_minor")])
    }
    no_zebra  <- no_zebra[list_sizes(no_zebra) > 0]
    gridlines <- GuideGrid$build_ticks(no_zebra, elements, params)

    if (params$x_direction) {

      x <- switch(
        params$at_x,
        major = sort(key$x_major$x),
        minor = sort(key$x_minor$x),
        sort(c(key$x_major$x, key$x_minor$x))
      )

      width <- c(x[2] - x[1], diff(x))
      min   <- oob_squish(x - width * 0, c(0, 1))
      max   <- oob_squish(x + width * 1, c(0, 1))
      mid   <- (min + max) / 2
      width <- (max - min)

      keep <- seq_along(x) %% 2 == as.numeric(params$odd_x)

      zebra_x  <- element_grob(
        elements$rect_x,
        x = mid[keep], width = width[keep],
        y = 0.5, height = 1
      )
    } else {
      zebra_x <- NULL
    }

    if (params$y_direction) {

      y <- switch(
        params$at_y,
        major = sort(key$y_major$y),
        minor = sort(key$y_minor$y),
        sort(c(key$y_major$y, key$y_minor$y))
      )

      height <- c(y[2] - y[1], diff(y))
      min    <- oob_squish(y - height * 0, c(0, 1))
      max    <- oob_squish(y + height * 1, c(0, 1))
      mid    <- (min + max) / 2
      height <- (max - min)

      keep <- seq_along(y) %% 2 == as.numeric(params$odd_y)

      zebra_y <- element_grob(
        elements$rect_y,
        y = mid[keep], height = height[keep],
        x = 0.5, width = 1
      )
    } else {
      zebra_y <- NULL
    }

    grobTree(zebra_x, zebra_y, gridlines)
  }
)
