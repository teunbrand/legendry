# Constructors -------------------------------------------------------------

#' Colour rings and arcs
#'
#' Similar to [`guide_colourbar()`][ggplot2::guide_colourbar], this guide
#' displays continuous `colour` or `fill` aesthetics. Instead of a bar, the
#' gradient in shown in a ring or arc, which can be convenient for cyclical
#' palettes such as some provided in the \pkg{scico} package.
#'
#' @param key A [standard key][key_standard] specification. Defaults to
#'   [`key_auto()`].
#' @param start,end A `<numeric[1]>` in radians specifying the offset of the
#'   starting and end points from 12 o'clock. The `NULL` default for `end`,
#'   internally defaults to `start + 2 * pi`.
#' @param outer_guide,inner_guide Guides to display on the outside and inside
#'   of the colour ring. Each guide can be specified using one of the following:
#'   * A `<Guide>` class object.
#'   * A `<function>` that returns a `<Guide>` class object.
#'   * A `<character>` naming such function, without the `guide_` or
#'     `primitive_` prefix.
#' @param
#' nbin A positive `<integer[1]>` determining how many colours to display.
#' @param reverse A `<logical[1]>` whether to reverse continuous guides.
#'   If `TRUE`, guides like colour bars are flipped. If `FALSE` (default),
#'   the original order is maintained.
#' @param show_labels A `<character[1]>` indicating for which guide labels
#'   should be shown. Can be one of `"outer"` (default), `"inner"`, `"both"` or
#'   `"none"`. Note that labels can only be omitted if the related guide
#'   has a label suppression mechanism.
#' @param vanilla A `<logical[1]>` whether to have the default style match
#'   the vanilla `guide_colourbar()` (`TRUE`) or take the theme
#'   verbatim (`FALSE`).
#' @param ... Arguments forwarded to the `outer_guide` and `inner_guide` if
#'   provided as functions or strings.
#' @inheritParams common_parameters
#'
#' @details
#' ## Styling options
#'
#' This guide is a hybrid composition guide, where the [theme][ggplot2::theme]
#' settings apply both this guide itself and the constituents it manages.
#' The constituents are linked below so you can find their 'Styling options'
#' sections. Note that `guide_axis_base()` is just a default that can be
#' swapped out.
#'
#' | **Constituent** | **Description** |
#' | ------------- | --------------- |
#' | [`guide_axis_base()`] | Makes up the tick marks and labels at inner and outer rings. |
#'
#' In addition to the constituent's theme setting, it also has the following
#' settings:
#'
#' | **Theme setting** | **Type** | **Description** |
#' | ----------------- | -------- | --------------- |
#' | `legend.background`| [`element_rect()`] | Background of the legend. |
#' | `legend.margin` | [`margin()`] | Padding around the legend. |
#' | `legend.key.width` | [`unit()`] | Radial thickness of the donut ring. |
#' | `legend.key.size` | [`unit()`] | Size of the legend. Multiplied by 5 to roughly match dimensions of colour bar guides. |
#' | `legend.frame` | [`element_rect()`] | Outline of the donut. The `fill` setting is ignored. |
#' | `legend.title` | [`element_text()`] | Title of the legend. |
#' | `legend.title.position` | `<character[1]>` | One of `"top"`, `"right"`, `"bottom"` or `"left"`.
#'
#' Styling options *per break* can be set in the [standard key][key_standard].
#' These override theme settings.
#'
#' The context-agnostic alternative to using `theme()` is to use
#' [`theme_guide()`]:
#'
#' ```r
#' guide_colring(theme = theme_guide(
#'   # Ring settings
#'   title = element_text(),
#'   title.position = "top",
#'   margin = margin(5),
#'   background = element_rect(),
#'   frame = element_rect(),
#'   key.size = unit(1, "cm"),
#'   key.width = unit(5, "mm"),
#'
#'   # Common options for `guide_axis_base()`
#'   line = element_line(),
#'   text = element_text(),
#'   ticks = element_line(),
#'   ticks.length = unit(5, "mm"),
#' ))
#' ```
#'
#' @return A `<Guide>` object.
#' @export
#' @family standalone guides
#'
#' @examples
#' # Rings works best with a cyclical palette
#' my_pal <- c("black", "tomato", "white", "dodgerblue", "black")
#'
#' p <- ggplot(mpg, aes(displ, hwy, colour = cty)) +
#'   geom_point() +
#'   scale_colour_gradientn(colours = my_pal)
#'
#' # Standard colour ring
#' p + guides(colour = "colring")
#'
#' # As an arc
#' p + guides(colour = guide_colring(
#'   start = 1.25 * pi, end = 2.75 * pi
#' ))
#'
#' # Removing the inner tick marks
#' p + guides(colour = guide_colring(inner_guide = "none"))
#'
#' # Include labels on the inner axis
#' p + guides(colour = guide_colring(show_labels = "both"))
#'
#' # Passing an argument to inner/outer guides
#' p + guides(colour = guide_colring(angle = 0))
guide_colring <- function(
  title = waiver(),
  key = "auto",
  start = 0.0,
  end = NULL,
  outer_guide = "axis_base",
  inner_guide = "axis_base",
  nbin = 300L,
  reverse = FALSE,
  show_labels = "outer",
  theme = NULL,
  vanilla = TRUE,
  position = waiver(),
  available_aes = c("colour", "fill"),
  ...
) {
  show_labels <- arg_match0(show_labels, c("both", "inner", "outer", "none"))
  show_labels <- list(
    inner = show_labels %in% c("inner", "both"),
    outer = show_labels %in% c("outer", "both")
  )

  defaults <- if (isTRUE(vanilla)) vanilla_colourbar_theme() else NULL

  coord <- coord_radial(
    start = start, end = end,
    expand = FALSE, inner.radius = 0.9
  )

  new_compose(
    list(inner = inner_guide, outer = outer_guide),
    title = title,
    coord = coord,
    args = list(...),
    key = key,
    nbin = nbin,
    reverse = reverse,
    theme = theme,
    theme_defaults = defaults,
    show_labels = show_labels,
    position = position,
    available_aes = available_aes,
    super = GuideColring
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
GuideColring <- ggproto(
  "GuideColring", Compose,

  params = new_params(
    guides = list(), guide_params = list(),
    key = NULL, angle = waiver(), coord = NULL,
    nbin = 300L, alpha = NA, reverse = FALSE,
    show_labels = list(inner = FALSE, outer = TRUE),
    theme_defaults = NULL
  ),

  elements = list(
    frame = "legend.frame",
    size  = "legend.key.size",
    width = "legend.key.width",
    background = "legend.background",
    title = "legend.title",
    title_position = "legend.title.position",
    margin = "legend.margin"
  ),

  train = function(self, params = self$params, scale, aesthetic = NULL,
                   title = waiver(), ...) {
    params$guide_params$inner$position <- "theta.sec"
    params$guide_params$outer$position <- "theta"
    params <- Compose$train(params, scale, aesthetic, title, ...)
    params <- ggproto_parent(Guide, self)$train(params, scale, aesthetic)
    params
  },

  extract_key = function(scale, aesthetic, nbin, ...) {
    standard_extract_key(scale, aesthetic, key_sequence(nbin))
  },

  extract_decor = function(scale, aesthetic, ...) {
    data_frame(!!aesthetic := scale$get_limits(), group = 1L)
  },

  get_layer_key = function(params, layers, data = NULL, ...) {
    params <- ring_xy(params, params$aesthetic)

    params <- Compose$get_layer_key(params, layers, data, ...)
    if (isTRUE(params$reverse)) {
      params <- set_limits(params, rev(params$limits))
    }

    panels <- params$coord$setup_panel_params(
      scale_x_continuous(limits = params$limits),
      scale_y_continuous(limits = c(0.0, 1.0))
    )
    # Override parameters
    panels$inner_radius <- c(0.5, 0.5)
    panels$bbox <- params$bbox <-
      polar_bbox(panels$arc, 0.0, panels$inner_radius)

    params <- Compose$transform(params, params$coord, panels)
    params$arc  <- panels$arc %% (2.0 * pi)
    params$decor <- coord_munch(
      params$coord, params$decor,
      panels, segment_length = 0.04
    )
    params
  },

  build_frame = function(params, elements) {
    frame <- elements$frame
    if (is_blank(frame)) {
      frame <- list(colour = NA, linewidth = 0.0)
    }
    decor <- params$decor

    x <- unit(decor$x, "npc")
    y <- unit(decor$y, "npc")

    x <- unit.c(x, rev(x) - unit(sin(rev(decor$theta)) * elements$width, "cm"))
    y <- unit.c(y, rev(y) - unit(cos(rev(decor$theta)) * elements$width, "cm"))

    if (abs(diff(params$coord$arc %% (2.0 * pi))) < 1e-2) {
      id <- nrow(decor)[c(1L, 1L)]
    } else {
      id <- nrow(decor) * 2L
    }

    polygonGrob(
      x = x, y = y, id.lengths = id,
      gp = gpar(
        fill = NA,
        col = frame$colour,
        lwd = frame$linewidth * .pt,
        lty = frame$linetype
      )
    )
  },

  build_arc = function(params, elements, frame) {
    check_device("clippingPaths")
    arc <- params$coord$arc
    limits <- params$limits %||% range(params$key$.value)
    theta <- rescale(params$key$.value, to = arc, from = limits)
    difftheta <- diff(theta)
    start <- c(arc[1L], theta[-1L] - 0.5 * difftheta)
    end   <- c(theta[-length(theta)] + 0.5 * difftheta, arc[2L])

    theta <- vec_interleave(start, start, end, end)
    r <- rep(c(0.0, 1.0, 1.0, 0.0), length(start)) * elements$width

    x <- rescale(0.5 * sin(theta) + 0.5, from = params$bbox$x)
    y <- rescale(0.5 * cos(theta) + 0.5, from = params$bbox$y)

    x <- unit(x, "npc") - unit(sin(theta) * r, "cm")
    y <- unit(y, "npc") - unit(cos(theta) * r, "cm")

    colour <- params$key[[params$aesthetic]]
    ring <- polygonGrob(
      x = x, y = y, id.lengths = rep(4L, nrow(params$key)),
      gp = gpar(fill = colour, col = colour, lwd = 1.0),
      vp = viewport(clip = frame)
    )
    gTree(children = gList(ring, frame))
  },

  setup_elements = function(params, elements, theme) {
    elements$title <- setup_legend_title(theme, direction = params$direction)
    theme$legend.frame <- theme$legend.frame %||% element_blank()
    Guide$setup_elements(params, elements, theme)
  },

  override_elements = function(params, elements, theme) {
    elements$title_position <- elements$title_position %||%
      switch(params$direction, horizontal = "left", vertical = "top")
    check_position(
      elements$title_position,
      theta = FALSE,
      arg = "legend.title.position"
    )
    elements$width <- cm(elements$width)
    elements$size  <- cm(elements$size) * 5.0
    elements$margin <- elements$margin %||% margin()
    elements$background <- element_grob(elements$background)
    elements
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    position  <- params$position  <- params$position  %||% position
    direction <- params$direction <- params$direction %||% direction
    position  <- switch(position, inside = "right", position)
    check_position(position, theta = FALSE)
    check_argmatch(direction, c("horizontal", "vertical"))

    theme <- theme + params$theme
    theme <- apply_theme_defaults(theme, params$theme_defaults)
    theme$legend.text.position <- "theta"

    elems <- self$setup_elements(params, self$elements, theme)
    elems <- self$override_elements(params, elems, theme)

    # Draw inner guide
    inner <- params$guide_params$inner
    inner$stack_offset <- unit(elems$width, "cm")
    inner$draw_label <- isTRUE(params$show_labels$inner)
    inner <-
      params$guides$inner$draw(theme, position, direction, params = inner)

    # Draw outer guide
    outer <- params$guide_params$outer
    outer$draw_label <- isTRUE(params$show_labels$outer)
    outer <-
      params$guides$outer$draw(theme, position, direction, params = outer)

    # Draw ring
    frame <- self$build_frame(params, elems)
    ring  <- self$build_arc(params, elems, frame)

    # Setup gtable
    asp <- with(params$bbox, diff(y) / diff(x))
    gt <- gtable(
      widths  = unit(elems$size * pmin(1.0 / asp, 1.0), "cm"),
      heights = unit(elems$size * pmin(asp, 1.0), "cm")
    ) |>
      gtable_add_grob(ring,  1L, 1L, name = "ring",  clip = "off") |>
      gtable_add_grob(inner, 1L, 1L, name = "inner", clip = "off") |>
      gtable_add_grob(outer, 1L, 1L, name = "outer", clip = "off")

    # Add padding, title, margin and background
    margin <-
      ring_margin(params$arc, outer$offset, elems$width + cm(inner$offset))
    title  <- self$build_title(params$title, elems)
    gt <- gtable_add_padding(gt, margin) |>
      self$add_title(
        title, elems$title_position,
        get_just(elems$title)
      ) |>
      gtable_add_padding(elems$margin)

    if (!is_zero(elems$background)) {
      gt <- gtable_add_grob(
        gt, elems$background,
        name = "background", clip = "off",
        t = 1L, r = -1L, b = -1L, l = 1L, z = -Inf
      )
    }

    gt
  }
)

# Helpers -----------------------------------------------------------------

ring_xy <- function(params, aesthetic) {
  if ("guide_params" %in% names(params)) {
    params$guide_params <-
      lapply(params$guide_params, ring_xy, aesthetic = aesthetic)
  }
  key <- params$key
  if (!is.null(key)) {
    key$x <- key$x %||% key$.value %||% key[[aesthetic]]
    key$y <- key$y %||% 1.0
    params$key <- key
  }
  decor <- params$decor
  if (!is.null(decor)) {
    decor$x <- decor$x %||% decor$.value %||% decor[[aesthetic]]
    decor$y <- decor$y %||% 1.0
    params$decor <- decor
  }
  params
}

ring_margin <- function(arc, outer = NULL, inner = NULL) {

  outer <- cm(outer %||% unit(0.0, "cm"))

  # If we have a full circle, apply outer padding to every margin
  if (abs(diff(arc)) >= 2.0 * pi) {
    return(unit(rep(outer, 4L), "cm"))
  }

  tol <- c(1.0, -1.0) * sqrt(.Machine$double.eps)
  margin <- rep(0.0, 4L)
  inner  <- cm(inner %||% unit(0.0, "cm"))

  # Left margin
  keep <- in_range(arc, c(1.0, 2.0) * pi + tol)
  if (in_arc(1.5 * pi, arc)) {
    margin[4L] <- outer
  } else if (any(keep)) {
    theta <- arc[keep]
    margin[4L] <- max(abs(cos(theta))) * outer
  } else if (all(in_range(arc, c(0.0, 1.0) * pi + tol))) {
    margin[4L] <- max(cos(arc)) * inner
  }

  # Right margin
  keep <- in_range(arc, c(0.0, 1.0) * pi + tol)
  if (in_arc(0.5 * pi, arc)) {
    margin[2L] <- outer
  } else if (any(keep)) {
    theta <- arc[keep]
    margin[2L] <- max(abs(cos(theta))) * outer
  } else if (all(in_range(arc, c(1.0, 2.0) * pi + tol))) {
    margin[2L] <- max(cos(arc)) * inner
  }

  # Bottom margin
  keep <- in_range(arc, c(0.5, 1.5) * pi + tol)
  if (in_arc(pi, arc)) {
    margin[3L] <- outer
  }  else if (any(keep)) {
    theta <- arc[keep]
    margin[3L] <- max(abs(sin(theta))) * outer
  } else if (all(in_arc(arc, c(1.5, 2.5) * pi + tol))) {
    margin[3L] <- max(sin(arc)) * inner
  }

  # Top margin
  keep <-
    in_range(arc, c(0.0, 0.5) * pi + c(0.0, tol[2L])) |
    in_range(arc, c(1.5, 2.0) * pi + c(tol[1L], 0.0))
  if (in_arc(0.0, arc)) {
    margin[1L] <- outer
  } else if (any(keep)) {
    theta <- arc[keep]
    margin[1L] <- max(abs(sin(theta))) * outer
  } else if (all(in_arc(arc, c(0.5, 1.5) * pi + tol))) {
    margin[1L] <- max(-sin(arc)) * inner
  }

  unit(margin, "cm")
}
