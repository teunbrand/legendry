# Constructor -------------------------------------------------------------

#' Guide gizmo: capped colour bar
#'
#' This guide displays a colour bar with optional caps at either ends of
#' the bar.
#'
#' @param key A [sequence key][key_sequence] specification. Defaults to
#'   [`key_sequence(n = 15)`][key_sequence]. Changing the argument to
#'   `key_sequence()` is fine, but changing the key type is not advised.
#' @param shape A [cap][cap_options] specification by providing one of the
#'   following:
#'   * A cap `<function>`, such as `cap_triangle()`.
#'   * A `<character[1]>` naming a cap function without the '`cap_`'-prefix,
#'   e.g. `"round"`.
#'   * A two column `<matrix[n, 2]>` giving coordinates for a cap, like those
#'   created by cap functions such as `cap_arch()`.
#' @param size A [`<unit>`][grid::unit] setting the size of the cap. When
#'   `NULL` (default), cap size will be proportional to the `shape` coordinates
#'   and the `legend.key.size` theme setting.
#'
#' @param show A `<logical>` to control how caps are displayed at the ends
#'   of the bar. When `TRUE`, caps are always displayed. When `FALSE`, caps
#'   are never displayed. When `NA` (default), caps are displayed when the
#'   data range exceed the limits. When given as `<logical[2]>`, `show[1]`
#'   controls the display at the lower end and `show[2]` at the upper end.
#' @param alpha A `<numeric[1]>` between 0 and 1 setting the colour transparency
#'   of the bar. Use `NA` to preserve the alpha encoded in the colour itself.
#' @param oob An out-of-bounds handling function that affects the cap colour.
#'   Can be one of the following:
#'   * A `<function>` like [`oob_squish`][scales::oob_squish].
#'   * A `<character[1]>` naming such a function without the '`oob`'-prefix,
#'   such as `"keep"`.
#' @inheritParams common_parameters
#'
#' @details
#' ## Styling options
#'
#' Below are the [theme][ggplot2::theme] options that determine the styling of
#' this guide. Note that the width or height (depending on the `direction`
#' argument) *includes* the cap.
#'
#' | **Theme setting** | **Type** | **Description** |
#' | ----------------- | -------- | --------------- |
#' | `legend.frame` | [`element_rect()`] | Frame drawn around the bar and caps. The `fill` setting is ignored. |
#' | `legend.key.width` | [`unit()`] | Width of the bar |
#' | `legend.key.height` | [`unit()`] | Height of the bar |
#'
#' Please note that depending on the `direction` argument, the
#' `legend.key.width`/`legend.key.height` setting are expanded 5-fold if
#' originating from the global theme. To set these directly, you can use the
#' local `theme` argument in the guide.
#' These settings have shorthands in [`theme_guide()`]:
#'
#' ```r
#' gizmo_barcap(theme = theme_guide(
#'   frame = element_rect(),
#'   key.width = unit(5, "mm")
#'   key.height = unit(5, "cm")
#' ))
#' ```
#'
#' @return A `<GizmoBarcap>` object.
#' @family gizmos
#' @export
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy, colour = cty)) +
#'   geom_point()
#'
#' # Just a bar
#' p + scale_colour_viridis_c(guide = gizmo_barcap())
#'
#' # Caps show up when there is data outside the limits
#' p + scale_colour_viridis_c(
#'   limits = c(10, 30),
#'   guide  = gizmo_barcap()
#' )
#'
#' # The scale's out-of-bounds handler determines cap colour
#' p + scale_colour_viridis_c(
#'   limits = c(10, 30), oob = scales::oob_squish,
#'   guide = gizmo_barcap()
#' )
#'
#' # Customising display of the guide
#' p +
#'   scale_colour_viridis_c(
#'     oob = scales::oob_squish,
#'     guide = gizmo_barcap(
#'       shape = "arch", show = c(FALSE, TRUE),
#'       size = unit(2, "cm"),
#'       theme = theme(legend.key.height = unit(4, "cm"))
#'     )
#'   ) +
#'   theme(
#'     legend.frame = element_rect(colour = "black"),
#'     legend.key.width = unit(0.5, "cm")
#'   )
gizmo_barcap <- function(key = "sequence", shape = "triangle", size = NULL,
                         show = NA, alpha = NA, oob = "keep", theme = NULL,
                         position = waiver(), direction = NULL) {
  check_number_decimal(
    alpha, min = 0.0, max = 1.0,
    allow_infinite = FALSE, allow_na = TRUE
  )
  check_logical(show)
  check_length(show, exact = 1L:2L)
  show <- rep_len(show, 2L)

  check_unit(size, allow_null = TRUE)
  shape <- resolve_cap_shape(shape)
  oob <- resolve_oob(oob)

  new_guide(
    key = key,
    shape = shape,
    size  = size,
    show  = show,
    alpha = alpha,
    oob = oob,
    theme = theme,
    position = position,
    direction = direction,
    available_aes = c("colour", "fill"),
    super = GizmoBarcap
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
GizmoBarcap <- ggproto(
  "GizmoBarcap", Guide,

  elements = list(
    frame  = "legend.frame",
    width  = "legend.key.width",
    height = "legend.key.height"
  ),

  params = new_params(alpha = NA, shape = NULL, key = "sequence",
                      show = NA, size = NULL, oob = oob_keep),

  extract_key = function(scale, aesthetic, key, ...) {
    key <- resolve_key(key %||% "sequence")
    if (is.function(key)) {
      key <- key(scale, aesthetic)
    }
    key
  },

  extract_params = function(scale, params, ...) {
    params$position <- params$position %|W|% NULL
    key <- rename(params$key, params$aesthetic, "colour")
    limits <- scale$get_limits()
    range <- scale$range$range

    lower_oob <- range[1L] < limits[1L]
    upper_oob <- range[2L] > limits[2L]

    params$show[1L] <- !isFALSE(params$show[1L] %|NA|% lower_oob)
    params$show[2L] <- !isFALSE(params$show[2L] %|NA|% upper_oob)

    add <- abs(diff(limits)) / 1000.0
    if (params$show[1L]) {
      val <- params$oob(limits[1L] - add, limits)
      limits <- range(limits, val)
      key <- data_frame0(
        colour = c(scale$map(val), key$colour),
        .value = c(val, key$.value)
      )
    }
    if (params$show[2L]) {
      val <- params$oob(limits[2L] + add, limits)
      limits <- range(limits, val)
      key <- data_frame0(
        colour = c(key$colour, scale$map(val)),
        .value = c(key$.value, val)
      )
    }
    if ("colour" %in% names(key)) {
      key$colour <- alpha(key$colour, alpha = params$alpha)
    }
    params$limits <- limits
    params$key <- key
    params
  },

  setup_params = function(params) {
    key <- params$key
    key$.value <- guide_rescale(key$.value, params$limits)
    key$x <- switch(params$position, left = , right = 0.5, key$.value)
    key$y <- switch(params$position, left = , right = key$.value, 0.5)
    params$key <- key
    if (params$limits[1L] > params$limits[2L]) {
      params$show <- rev(params$show)
    }
    params
  },

  setup_elements = function(params, elements, theme) {
    theme$legend.frame <- theme$legend.frame %||% element_blank()
    if (params$direction == "horizontal") {
      theme$legend.key.width <- (theme$legend.key.width %||% rel(1.0)) * 5.0
    } else {
      theme$legend.key.height <- (theme$legend.key.height %||% rel(1.0)) * 5.0
    }
    Guide$setup_elements(params, elements, theme)
  },

  build_frame = function(key, elements, params) {

    short_side <-
      switch(params$direction, vertical = elements$width, elements$height)

    shape <- params$shape
    max   <- max(shape[, 2L])
    if (max != 0.0) {
      shape[, 2L] <- rescale_max(shape[, 2L], from = c(0.0, max))
    }

    size_lower <- size_upper <- params$size %||% (max * short_side)
    if (isFALSE(params$show[1L])) {
      lower <- cbind(c(0.0, 1.0), c(1.0, 1.0))
      size_lower <- unit(0.0, "cm")
    } else {
      lower <- cbind(shape[, 1L], 1.0 - shape[, 2L])
    }
    if (isFALSE(params$show[2L])) {
      upper <- cbind(c(1.0, 0.0), c(1.0, 1.0))
      size_upper <- unit(0.0, "cm")
    } else {
      upper <- cbind(rev(shape[, 1L]), 1.0 - rev(shape[, 2L]))
    }
    poly_args <- list(
      x = unit.c(
        unit(0.0, "npc") + lower[, 2L] * size_lower,
        unit(1.0, "npc") - upper[, 2L] * size_upper
      ),
      y = unit(c(lower[, 1L], upper[, 1L]), "npc")
    )
    if (params$direction == "vertical") {
      poly_args <- rename(poly_args, c("x", "y"), c("y", "x"))
    }
    frame <- element_grob(elements$frame)
    gp <- frame$gp %||% gpar(col = NA)
    frame <- polygonGrob(poly_args$x, poly_args$y, gp = gp)
    list(grob = frame, upper = size_upper, lower = size_lower)
  },

  fill_frame = function(key, elements, params, grobs = NULL) {

    check_device("gradients", call = expr(gizmo_barcap()))

    grad_args <- list(
      x1 = unit(0.0, "npc") + grobs$lower,
      x2 = unit(1.0, "npc") - grobs$upper,
      y1 = 0.5, y2 = 0.5, colours = key$colour, stops = key$.value
    )
    if (params$direction == "vertical") {
      grad_args <- flip_names(grad_args)
    }

    gradient   <- inject(linearGradient(!!!grad_args))
    grobs$grob <- editGrob(grobs$grob, gp = gpar(fill = gradient))
    grobs
  },

  assemble_drawing = function(grobs, layout, sizes, params, elements) {

    middle <- switch(layout, horizontal = elements$width, elements$height)
    if (unitType(middle) != "null") {
      middle <- max(middle - sizes$lower - sizes$upper, unit(0.0, "cm"))
    }
    sizes <- unit.c(sizes$lower, middle, sizes$upper)

    if (layout == "horizontal") {
      gt <- gtable(widths = sizes, heights = elements$height) |>
        gtable_add_grob(
          grobs, t = 1L, l = 1L, r = -1L,
          clip = "off", name = "barcap"
        )
      gt$align <- list(horizontal = c(2.0, -2.0))
    } else {
      gt <- gtable(widths = elements$width, heights = rev(sizes)) |>
        gtable_add_grob(
          grobs, t = 1L, b = -1L, l = 1L,
          clip = "off", name = "barcap"
        )
      gt$align <- list(vertical = c(2.0, -2.0))
    }
    gt
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    params <- replace_null(params, position = position, direction = direction)
    params <- self$setup_params(params)
    elems  <- self$setup_elements(params, self$elements, theme)

    frame <- self$build_frame(params$key, elems, params)
    bar   <- self$fill_frame(params$key, elems, params, grobs = frame)

    self$assemble_drawing(
      grobs = bar$grob, layout = params$direction,
      sizes = list(lower = bar$lower, upper = bar$upper),
      elements = elems
    )
  }
)

# Helpers -----------------------------------------------------------------

resolve_oob <- function(x, call = caller_call(), arg = caller_arg(x)) {
  if (is.function(x)) {
    return(x)
  }
  if (is.character(x)) {
    check_string(x, arg = arg, call = call)
    x <- paste0("oob_", x)
    fun <- find_global(x, env = global_env(), mode = "function")
    if (is.function(fun)) {
      return(fun)
    }
    # Try the {scales} package
    fun <- find_global(x, env = asNamespace("scales"), mode = "function")
    if (is.function(fun)) {
      return(fun)
    }
  }
  cli::cli_abort("Unknown {.arg {arg}} handler: {x}.", call = call)
}
