# Constructor -------------------------------------------------------------

#' Guide primitive: line
#'
#' This function constructs a ticks [guide primitive][guide-primitives].
#'
#' @param bidi A `<logical[1]>`: whether ticks should be drawn bidirectionally
#'   (`TRUE`) or in a single direction (`FALSE`, default).
#' @inheritParams primitive_labels
#'
#' @return A `PrimitiveTicks` primitive guide that can be used inside other
#'   guides.
#' @export
#' @family primitives
#'
#' @details
#' ## Styling options
#'
#' Below are the [theme][ggplot2::theme] options that determine the styling of
#' this guide, which may differ depending on whether the guide is used in
#' an axis or in a legend context.
#'
#' The ticks can come in three variants: major, minor and minimal.
#' Which variants are drawn depends on the keys: [`key_minor()`] draws major
#' and minor ticks, whereas [`key_log()`] also has minimal ticks.
#' Each variant has a corresponding length setting.
#'
#' The possible `{position}` suffixes mentioned below are `x`, `x.top`,
#' `x.bottom`, `y`, `y.left`, `y.right`. The `theta` and `r` position suffixes
#' in \pkg{ggplot2} are *not* obeyed in \pkg{legendry}.
#'
#' | **Theme setting** | **Context** | **Type** | **Description** |
#' | ----------------- | ----------- | -------- | --------------- |
#' | `axis.ticks.{position}` | Axis | [`element_line()`] | Major tick lines |
#' | `axis.ticks.length.{position}` | Axis | [`unit()`] | Major tick length |
#' | `axis.minor.ticks.{position}` | Axis | [`element_line()`] | Minor tick lines |
#' | `axis.minor.ticks.length.{position}` | Axis | [`unit()`] | Minor tick length |
#' | `legendry.axis.mini.ticks` | Axis | [`element_line()`] | Minimal tick lines |
#' | `legendry.axis.mini.ticks.length` | Axis | [`unit()`] | Minimal tick length |
#' | `legend.ticks` | Legend | [`element_line()`] | Major tick lines |
#' | `legend.ticks.length` | Legend | [`unit()`] | Major ticks length |
#' | `legendry.legend.minor.ticks` | Legend | [`element_line()`] | Minor tick lines |
#' | `legendry.legend.minor.ticks.length` | Legend | [`unit()`] | Minor ticks length |
#' | `legendry.legend.mini.ticks` | Legend | [`element_line()`] | Minimal tick lines |
#' | `legendry.legend.mini.ticks.length` | Legend | [`unit()`] | Minimal tick length |
#'
#' Styling options *per break* can be set in the [key][key_standard].
#' The `line` and prefixed properties are prioritised for the tick lines.
#' These override theme settings.
#'
#' The context-agnostic alternative to using `theme()` is to use
#' [`theme_guide()`]:
#'
#' ```r
#' primitive_ticks(theme = theme_guide(
#'   ticks = element_line(),
#'   ticks.length = unit(5, "mm"),
#'   minor.ticks = element_line(),
#'   minor.ticks.length = unit(4, "mm"),
#'   mini.ticks = element_line(),
#'   mini.ticks.length = unit(3, "mm")
#' ))
#' ```
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Adding as secondary guides
#' p + guides(x.sec = primitive_ticks(), y.sec = primitive_ticks())
primitive_ticks <- function(key = NULL, bidi = FALSE, theme = NULL,
                            position = waiver()) {
  check_bool(bidi)
  new_guide(
    key = key,
    bidi = bidi,
    theme = theme,
    position = position,
    available_aes = c("any", "x", "y", "r", "theta"),
    super = PrimitiveTicks
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
PrimitiveTicks <- ggproto(
  "PrimitiveTicks", Guide,

  params = new_params(key = NULL, bidi = FALSE),

  hashables = exprs(key$.value),

  elements = list(
    position = list(
      ticks = "axis.ticks",       ticks_length = "axis.ticks.length",
      minor = "axis.minor.ticks", minor_length = "axis.minor.ticks.length"
    ),
    legend = list(
      ticks        = "legend.ticks",
      ticks_length = "legend.ticks.length",
      minor        = "legendry.legend.minor.ticks",
      minor_length = "legendry.legend.minor.ticks.length",
      # Mini ticks for legends don't have complicated inheritance
      mini         = "legendry.legend.mini.ticks",
      mini_length  = "legendry.legend.mini.ticks.length"
    )
  ),

  extract_key = standard_extract_key,

  extract_params = primitive_extract_params,

  transform = function(self, params, coord, panel_params) {
    params$key <-
      transform_key(params$key, params$position, coord, panel_params)
    params
  },

  setup_params = primitive_setup_params,

  setup_elements = primitive_setup_elements,

  override_elements = function(params, elements, theme) {

    # Count how many ticks of each type we need
    type <- params$key$.type %||% "major"
    n_major <- sum(type == "major")
    n_minor <- sum(type == "minor")
    n_mini  <- sum(type == "mini")

    # We need to setup mini ticks for axes, as the inheritance tree isn't
    # mirrored for every aesthetic/position combination.
    if (n_mini > 0L && params$aesthetic %in% c("x", "y")) {
      elements$mini <- combine_elements(
        theme$legendry.axis.mini.ticks,
        elements$minor
      )
      elements$mini_length <- combine_elements(
        theme$legendry.axis.mini.ticks.length,
        elements$minor_length
      )
    }

    # Set absent ticks to empty
    elements <- zap_tick(elements, "ticks", n_major)
    elements <- zap_tick(elements, "minor", n_minor)
    elements <- zap_tick(elements, "mini",  n_mini)

    lengths <- c("ticks_length", "minor_length", "mini_length")
    elements$size <- inject(range(!!!elements[lengths], 0.0))
    elements
  },

  build_ticks = function(key, elements, params, position = params$position) {
    type <- key$.type %||% "major"
    offset <- elements$offset
    major <- draw_ticks(
      vec_slice(key, type == "major"),
      elements$ticks, params, position, elements$ticks_length, offset
    )
    minor <- draw_ticks(
      vec_slice(key, type == "minor"),
      elements$minor, params, position, elements$minor_length, offset
    )
    mini <- draw_ticks(
      vec_slice(key, type == "mini"),
      elements$mini, params, position, elements$mini_length, offset
    )
    # Discard zeroGrobs
    grob <- list(major, minor, mini)
    grob <- grob[!map_lgl(grob, is_zero)]
    if (length(grob) == 0L) {
      return(zeroGrob())
    }
    gTree(children = inject(gList(!!!grob)))
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    params <- replace_null(params, position = position, direction = direction)
    params <- self$setup_params(params)

    elems <- self$setup_elements(params, self$elements, theme)
    elems <- self$override_elements(params, elems, theme)
    ticks <- self$build_ticks(params$key, elems, params)

    # If ticks have negative length, we want to preserve reasonable spacing
    # to text labels.
    ticks <- list(ticks, zeroGrob())
    size <- unit(c(elems$size[2L], max(0.0, -1.0 * diff(elems$size))), "cm")

    primitive_grob(
      grob = ticks,
      size = size,
      position = params$position,
      name = "ticks"
    )
  }
)

# Helpers -----------------------------------------------------------------

draw_ticks <- function(key, element, params, position, length, offset = 0.0) {
  n_breaks <- nrow(key)
  if (n_breaks < 1L || is_blank(element) || all(length == 0L)) {
    return(zeroGrob())
  }
  props  <- element_key_properties(key, "line")
  length <- rep_len(length, n_breaks)
  bidi   <- c(1.0, -as.numeric(params$bidi %||% FALSE))

  # Every tick has two vertices
  length <- rep(length, each = 2L)
  key <- vec_rep_each(key, 2L)
  id <- rep(2L, n_breaks)

  # Set anchor positions
  switch(
    position,
    top    = {key$y <- 0.0},
    right  = {key$x <- 0.0},
    bottom = {key$y <- 1.0},
    left   = {key$x <- 1.0}
    # theta(.sec) already has appropriate x/y values
  )

  length <- rep(bidi, times = n_breaks) * length
  length <- unit(length + offset, "cm")
  theta  <- get_theta(key, position)

  args <- list(
    x = unit(key$x, "npc") + sin(theta) * length,
    y = unit(key$y, "npc") + cos(theta) * length
  )

  inject(element_grob(element, id.lengths = id, !!!args, !!!props))
}

zap_tick <- function(elements, name, n) {
  length <- paste0(name, "_length")
  # If there are no ticks, set the element to NULL
  if (n < 1L) {
    elements[[name]] <- NULL
  }
  # If there is no element, set the length to 0
  if (is_blank(elements[[name]])) {
    elements[[length]] <- 0.0
  }
  # Ensure tick lengths are in centimetres
  elements[[length]] <- cm(elements[[length]])
  elements
}
