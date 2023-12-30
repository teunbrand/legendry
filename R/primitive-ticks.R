# Constructor -------------------------------------------------------------

#' Guide primitive: line
#'
#' This function contructs a ticks [guide primitive][guide-primitives].
#'
#' @inheritParams ggplot2::guide_legend
#'
#' @return A `GuideTicks` primitive guide that can be used inside other guides.
#' @export
#' @family primitives
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Adding as secondary guides
#' p + guides(x.sec = "ticks", y.sec = guide_ticks())
guide_ticks <- function(theme = NULL, position = waiver()) {
  new_guide(
    theme = theme,
    position = position,
    available_aes = c("any", "x", "y", "r", "theta"),
    super = GuideTicks
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideTicks <- ggproto(
  "GuideTicks", Guide,

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
    elements <- paste0(prefix, c("ticks", "ticks.length"), suffix)
    elements <- list(ticks = elements[1], ticks_length = elements[2])
    Guide$setup_elements(params, elements, theme)
  },

  override_elements = function(params, elements, theme) {
    if (is_blank(elements$ticks) || nrow(params$key) < 1) {
      elements$ticks_length <- 0
    } else {
      elements$ticks_length <- cm(elements$ticks_length)
    }
    elements
  },

  build_ticks = function(key, elements, params, position = params$position) {
    n_breaks <- nrow(key)
    element <- elements$ticks
    if (n_breaks < 1 || is_blank(element)) {
      return(zeroGrob())
    }
    length  <- elements$ticks_length
    if (params$position %in% .trbl) {
      Guide$build_ticks(
        key, element, params, opposite_position(position), length
      )
    } else {
      angle  <- rep(key$theta, each = 2)
      x      <- rep(key$x,     each = 2)
      y      <- rep(key$y,     each = 2)

      length <- rep(length, length.out = n_breaks * 2)
      length <- rep(c(0, 1), times = n_breaks) * length
      if (!is.null(params$stack_offset)) {
        length <- length + params$stack_offset
      }
      element_grob(
        element,
        x = unit(x, "npc") + sin(angle) * length,
        y = unit(y, "npc") + cos(angle) * length,
        id.lengths = rep(2, n_breaks)
      )
    }
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    params <- replace_null(params, position = position, direction = direction)

    elems <- self$setup_elements(params, self$elements, theme)
    elems <- self$override_elements(params, elems, theme)
    ticks <- self$build_ticks(params$key, elems, params)

    primitive_grob(
      grob = ticks,
      size = max(elems$ticks_length, unit(0, "cm")),
      position = params$position,
      name = "ticks"
    )
  }
)
