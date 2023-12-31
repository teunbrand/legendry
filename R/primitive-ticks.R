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
guide_ticks <- function(key = NULL, theme = NULL, position = waiver()) {
  new_guide(
    key = key,
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

  params = new_params(Guide$params, key = NULL),

  extract_key = standard_extract_key,

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
    elements <- c("ticks", "ticks.length")
    if (params$aesthetic %in% c("x", "y")) {
      suffix <- switch(
        params$position,
        theta = ".x.bottom", theta.sec = ".x.top",
        paste0(".", params$aesthetic, ".", params$position)
      )
      prefix <- "axis."
      elements <- c(elements, "minor.ticks", "minor.ticks.length")
      elements <- paste0(prefix, elements, suffix)
    } else {
      prefix <- "legend."
      elements <- paste0(prefix, elements, suffix)
      elements <- c(
        elements,
        "gguidance.legend.minor.ticks",
        "gguidance.legend.minor.ticks.length"
      )
    }
    elements <- list(
      ticks = elements[1], ticks_length = elements[2],
      minor = elements[3], minor_length = elements[4]
    )
    Guide$setup_elements(params, elements, theme)
  },

  override_elements = function(params, elements, theme) {
    type <- params$key$.type %||% "major"
    n_major <- sum(type == "major")
    n_minor <- sum(type == "minor")
    n_mini  <- sum(type == "mini")
    if (is_blank(elements$ticks) || n_major < 1) {
      elements$ticks_length <- 0
    } else {
      elements$ticks_length <- cm(elements$ticks_length)
    }
    if (is_blank(elements$minor) || n_minor < 1) {
      elements$minor_length <- 0
    } else {
      elements$minor_length <- cm(elements$minor_length)
    }
    if (n_mini > 0) {
      if (params$aesthetic %in% c("x", "y")) {
        mini <- combine_elements(
          theme$gguidance.axis.mini.ticks,
          elements$minor
        )
        mini_length <- combine_elements(
          theme$gguidance.axis.mini.ticks.length,
          elements$minor_length
        )
      } else {
        mini <- combine_elements(
          theme$gguidance.legend.mini.ticks,
          elements$minor
        )
        mini_length <- combine_elements(
          theme$gguidance.legend.mini.ticks.length,
          elements$minor
        )
      }
      elements$mini <- mini
      if (is_blank(mini)) {
        elements$mini_length <- 0
      } else {
        elements$mini_length <- cm(mini_length)
      }
    } else {
      elements$mini_length <- 0
    }
    lengths <- c("ticks_length", "minor_length", "mini_length")
    elements$size <- inject(range(!!!elements[lengths], 0))
    elements
  },

  build_ticks = function(key, elements, params, position = params$position) {
    type <- key$.type %||% "major"
    major <- draw_ticks(
      vec_slice(key, type == "major"),
      elements$ticks, params, position, elements$ticks_length
    )
    minor <- draw_ticks(
      vec_slice(key, type == "minor"),
      elements$minor, params, position, elements$minor_length
    )
    mini <- draw_ticks(
      vec_slice(key, type == "mini"),
      elements$mini, params, position, elements$mini_length
    )
    grob <- list(major, minor, mini)
    grob <- grob[!vapply(grob, is.zero, logical(1))]
    if (length(grob) == 0) {
      return(zeroGrob())
    }
    gTree(children = inject(gList(!!!grob)))
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    params <- replace_null(params, position = position, direction = direction)

    elems <- self$setup_elements(params, self$elements, theme)
    elems <- self$override_elements(params, elems, theme)

    ticks <- self$build_ticks(params$key, elems, params)
    ticks <- list(ticks, zeroGrob())

    size <- unit(c(elems$size[2], max(0, -1 * diff(elems$size))), "cm")

    primitive_grob(
      grob = ticks,
      size = size,
      position = params$position,
      name = "ticks"
    )
  }
)

draw_ticks = function(key, element, params, position, length) {
  n_breaks <- nrow(key)
  if (n_breaks < 1 || is_blank(element) || length == 0) {
    return(zeroGrob())
  }
  if (params$position %in% .trbl) {
    ticks <- Guide$build_ticks(
      key, element, params, opposite_position(position), unit(length, "cm")
    )
    return(ticks)
  }
  angle  <- rep(key$theta, each = 2)
  x      <- rep(key$x,     each = 2)
  y      <- rep(key$y,     each = 2)

  length <- rep(length, length.out = n_breaks * 2)
  length <- unit(rep(c(0, 1), times = n_breaks) * length, "cm")
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
