# Constructor -------------------------------------------------------------

#' Guide gizmo: capped colour bar
#'
#' This guide displays a colour bar with optional caps at either ends of
#' the bar.
#'
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
#' @param nbin An `<integer[1]>` giving how many colours will be used to
#'   display the colour gradient.
#' @param alpha A `<numeric[1]>` between 0 and 1 setting the colour transparency
#'   of the bar. Use `NA` to preserve the alpha encoded in the colour itself.
#' @param oob An out-of-bounds handling function that affects the cap colour.
#'   Can be one of the following:
#'   * A `<function>` like [`oob_squish`][scales::oob_squish].
#'   * A `<character[1]>` naming such a function without the '`oob`'-prefix,
#'   such as `"keep"`.
#' @inheritParams common_parameters
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
gizmo_barcap <- function(shape = "triangle", size = NULL, show = NA,
                         nbin = 15, alpha = NA, oob = "keep", theme = NULL,
                         position = waiver(), direction = NULL) {
  check_number_whole(nbin, min = 2)
  check_number_decimal(
    alpha, min = 0, max = 1,
    allow_infinite = FALSE, allow_na = TRUE
  )
  check_logical(show)
  check_length(show, exact = 1:2)
  show <- rep(show, length.out = 2)

  check_unit(size, allow_null = TRUE)
  shape <- resolve_cap_shape(shape)
  oob <- resolve_oob(oob)

  new_guide(
    shape = shape,
    size  = size,
    show  = show,
    nbin  = nbin,
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
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GizmoBarcap <- ggproto(
  "GizmoBarcap", Guide,

  elements = list(
    frame  = "legend.frame",
    width  = "legend.key.width",
    height = "legend.key.height"
  ),

  params = new_params(nbin = 15, alpha = NA, shape = NULL,
                      show = NA, size = NULL, oob = oob_keep),

  extract_key = extract_colourbar,

  extract_params = function(scale, params, ...) {
    params$position <- params$position %|W|% NULL
    limits <- scale$get_limits()
    range <- scale$range$range

    lower_oob <- range[1] < limits[1]
    upper_oob <- range[2] > limits[2]

    params$show[1] <- !isFALSE(params$show[1] %|NA|% lower_oob)
    params$show[2] <- !isFALSE(params$show[2] %|NA|% upper_oob)

    add <- diff(limits) / 1000
    if (params$show[1]) {
      val <- params$oob(limits[1] - add, limits)
      limits <- range(limits, val)
      params$key <- data_frame0(
        colour = c(scale$map(val), params$key$colour),
        value  = c(val, params$key$value)
      )
    }
    if (params$show[2]) {
      val <- params$oob(limits[2] + add, limits)
      limits <- range(limits, val)
      params$key <- data_frame0(
        colour = c(params$key$colour, scale$map(val)),
        value  = c(params$key$value, val)
      )
    }
    params$limits <- limits
    params
  },

  setup_params = function(params) {
    key <- params$key
    key$value <- guide_rescale(key$value, params$limits)
    key$x <- switch(params$position, left = , right = 0.5, key$value)
    key$y <- switch(params$position, left = , right = key$value, 0.5)
    params$key <- key
    params
  },

  setup_elements = function(params, elements, theme) {
    theme$legend.frame <- theme$legend.frame %||% element_blank()
    if (params$direction == "horizontal") {
      theme$legend.key.width <- theme$legend.key.width * 5
    } else {
      theme$legend.key.height <- theme$legend.key.height * 5
    }
    Guide$setup_elements(params, elements, theme)
  },

  build_decor = function(key, grobs = NULL, elements, params) {

    check_device("gradients", call = expr(gizmo_barcap()))
    if (params$direction == "horizontal") {
      dir <- elements$width
      ort <- elements$height
    } else {
      dir <- elements$height
      ort <- elements$width
    }

    shape <- params$shape
    max <- max(shape[, 2])
    shape[, 2] <- rescale(shape[, 2], to = c(0, 1), from = c(0, max))
    size_upper <- size_lower <- params$size %||% (max * ort)
    if (params$limits[1] > params$limits[2]) {
      params$show <- rev(params$show)
    }

    if (!isFALSE(params$show[2])) {
      upper <- cbind(rev(shape[, 1]), 1 - rev(shape[, 2]))
    } else {
      upper <- cbind(c(1, 0), c(1, 1))
      size_upper <- unit(0, "cm")
    }
    if (!isFALSE(params$show[1])) {
      lower <- cbind(shape[, 1], 1 - shape[, 2])
    } else {
      lower <- cbind(c(0, 1), c(1, 1))
      size_lower <- unit(0, "cm")
    }
    grad_args <- list(
      x1 = unit(0, "npc") + size_lower, x2 = unit(1, "npc") - size_upper,
      y1 = 0.5, y2 = 0.5, colours = key$colour, stops = key$value
    )
    poly_args <- list(
      x = unit.c(
        unit(0, "npc") + lower[, 2] * size_lower,
        unit(1, "npc") - upper[, 2] * size_upper
      ),
      y = unit(c(lower[, 1], upper[, 1]), "npc")
    )
    if (params$direction == "vertical") {
      grad_args <- rename(
        grad_args, c("x1", "x2", "y1", "y2"), c("y1", "y2", "x1", "x2")
      )
      poly_args <- rename(poly_args, c("x", "y"), c("y", "x"))
    }

    gradient <- inject(linearGradient(!!!grad_args))
    frame <- element_grob(elements$frame)
    if (!is.zero(frame)) {
      gp <- frame$gp
      gp$fill <- gradient
      gp <- inject(gpar(!!!gp))
    } else {
      gp <- gpar(fill = gradient, col = NA)
    }
    poly <- inject(polygonGrob(!!!poly_args, gp = gp))
    list(grob = poly, upper = size_upper, lower = size_lower)
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    params <- replace_null(params, position = position, direction = direction)
    params <- self$setup_params(params)
    elems  <- self$setup_elements(params, self$elements, theme)

    bar <- self$build_decor(params$key, elements = elems, params = params)

    if (params$direction == "horizontal") {
      middle <- elems$width
      if (unitType(middle) != "null") {
        middle <- middle - bar$lower - bar$upper
      }
      widths  <- unit.c(bar$lower, middle, bar$upper)
      gt <- gtable(widths = widths, heights = elems$height)
      gt <- gtable_add_grob(gt, bar$grob, 1, l = 1, r = -1, clip = "off", name = "barcap")
      gt$align <- list(horizontal = c(2, -2))
    } else {
      middle <- elems$height
      if (unitType(elems$height) != "null") {
        middle <- middle - bar$lower - bar$upper
      }
      heights <- unit.c(bar$upper, middle, bar$lower)
      gt <- gtable(widths = elems$width, heights = heights)
      gt <- gtable_add_grob(gt, bar$grob, t = 1, b = -1, l = 1, clip = "off", name = "barcap")
      gt$align <- list(vertical = c(2, -2))
    }
    gt
  }
)

# Helpers -----------------------------------------------------------------

resolve_oob = function(x, call = caller_call(), arg = caller_arg(x)) {
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

