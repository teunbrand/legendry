# Constructor -------------------------------------------------------------

#' Guide gizmo: kernel density estimate
#'
#' This guide displays a kernel density estimate (KDE) of the aesthetic. If the
#' aesthetic is `colour` or `fill`, the shape will reflect this.
#'
#' @param key A [sequence key][key_sequence] or [binned key][key_bins]
#'   specification.
#' @param density One of the following:
#'  * `NULL` for using kernel density estimation on the data values (default).
#'  * a `<numeric>` vector to feed to the `density.fun` function.
#'  * A named `<list>` with `x` and `y` numeric elements of equal length, such
#'    as one returned by using the [`density()`][stats::density] function.
#'    Please note that `<list>` input is expected in scale-transformed space,
#'    not original data space.
#' @param density.args A `<list>` with additional arguments to the
#'   `density.fun` argument. Only applies when `density` is not provided as a
#'   `<list>`. already.
#' @param density.fun A `<function>` to use for kernel density estimation when
#'   the `density` argument is not provided as a list already.
#' @param just A `<numeric[1]>` between 0 and 1. Use 0 for bottom- or
#'   left-aligned densities, use 1 for top- or right-aligned densities and 0.5
#'   for violin shapes.
#' @inheritParams gizmo_barcap
#'
#' @details
#' Non-finite values such as `NA` and `NaN` are ignored while infinite values
#' such as `-Inf` and `Inf` are [squished][scales::oob_squish] to the limits.
#'
#' @return A `<GizmoDensity>` object.
#' @family gizmos
#' @export
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy, colour = cty)) +
#'   geom_point() +
#'   scale_colour_viridis_c()
#'
#' # Density from plot data
#' p + guides(colour = gizmo_density())
#'
#' # Using bins instead of gradient
#' p + guides(colour = gizmo_density("bins"))
#'
#' # Providing custom values to compute density of
#' p + guides(colour = gizmo_density(density = runif(1000, min = 5, max = 35)))
#'
#' # Providing a precomputed density
#' p + guides(colour = gizmo_density(density = density(mpg$cty, adjust = 0.5)))
#'
#' # Alternatively, parameters may be passed through density.args
#' p + guides(colour = gizmo_density(density.args = list(adjust = 0.5)))
gizmo_density <- function(
  key = "sequence",
  density = NULL, density.args = list(), density.fun = stats::density,
  just = 0.5, oob = "keep", alpha = NA,
  # standard arguments
  theme = NULL, position = waiver(), direction = NULL
) {

  check_number_decimal(just, min = 0, max = 1, allow_infinite = FALSE)

  new_guide(
    key = key,
    density      = density,
    density_args = density.args,
    density_fun  = density.fun,
    just = just, oob = oob, alpha = alpha,
    theme = theme, position = position, direction = direction,
    super = GizmoDensity
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GizmoDensity <- ggproto(
  "GizmoDensity", Guide,

  params = new_params(
    density = NULL, density_args = list(), density_fun = stats::density,
    just = 0.5, nbin = 15, oob = oob_keep, alpha = NA, key = "sequence"
  ),

  elements = list(
    frame  = "legend.frame",
    key    = "legend.key",
    width  = "legend.key.width",
    height = "legend.key.height"
  ),

  extract_key = function(scale, aesthetic, key, ...) {
    key <-  resolve_key(key %||% "sequence")
    if (is.function(key)) {
      key <- disallow_even_steps(key)
      key <- key(scale, aesthetic)
    }
    key
  },

  extract_decor = function(scale, density, density_args, density_fun, ...) {
    if (is.null(density)) {
      return(NULL)
    }
    if (is.atomic(density)) {
      density <- filter_finite(scale$transform(density))
      density <- inject(density_fun(density, !!!density_args))
    }
    check_density(density)
    density
  },

  extract_params = function(scale, params, ...) {

    params$position <- params$position %|W|% NULL

    key    <- params$key
    aes    <- params$aesthetic
    limits <- scale$get_limits()
    range  <- scale$range$range

    if ((range[1] < limits[1]) || (range[2] > limits[2])) {
      # Collect out-of-bounds values
      add <- abs(diff(limits)) / 1000
      oob <- c(-1, 1) * add + limits
      map <- scale$map(oob)
      aes <- params$aesthetic

      limits <- range(limits, oob)
      if (all(c("min", "max") %in% names(params$key))) {
        n <- max(which(!is.na(key[[aes]])))
        key <- data_frame0(
          !!aes := c(map[1], key[[aes]], map[2]),
          min = c(-Inf, key$min, key$max[n]),
          max = c(key$min[1], key$max, Inf)
        )
      } else {
        key <- data_frame0(
          !!aes := c(map[1], key[[aes]],  map[2]),
          .value = c(oob[1], key$.value,  oob[2])
        )
      }
    }
    if (grepl("colour|color|fill", aes)) {
      key[[aes]] <- alpha(key[[aes]], params$alpha)
    }

    params$limits <- limits
    params$key <- key
    params
  },

  get_layer_key = function(params, layers, data = NULL) {
    density <- params$decor %||% params$density
    if (length(density) == 0) {
      values  <- filter_finite(vec_c(!!!lapply(data, .subset2, params$aesthetic)))
      density <- inject(params$density_fun(values, !!!params$density_args))
      check_density(density)
    }
    params$decor  <- normalise_density(density)
    params$limits <- range(params$limits, params$decor$x)
    params
  },

  setup_params = function(params) {
    limits <- params$limits
    key <- params$key
    key <- vec_slice(key, !is.na(key[[params$aesthetic]]))
    if (all(c("min", "max") %in% names(key))) {
      min <- key$min
      min[which.min(min)] <- -Inf
      min <- guide_rescale(min, limits)

      max <- key$max
      max[which.max(max)] <- Inf
      max <- guide_rescale(key$max, limits)
      key$mid <- (max + min) / 2
      key$height <- abs(max - min)
    } else {
      key$.value <- guide_rescale(key$.value, limits)
    }
    params$key <- key
    params$decor$x <- guide_rescale(params$decor$x, params$limits)
    params
  },

  setup_elements = function(params, elements, theme) {
    theme$legend.frame <- theme$legend.frame %||% element_blank()
    if (params$direction == "horizontal") {
      theme$legend.key.width  <- theme$legend.key.width * 5
    } else {
      theme$legend.key.height <- theme$legend.key.height * 5
    }
    Guide$setup_elements(params, elements, theme)
  },

  build_frame = function(params, elems) {

    decor <- params$decor
    just <- c(params$just, 1 - params$just)

    poly_args <- list(
      x = c(decor$x, rev(decor$x)),
      y = c((1 - decor$y) * just[1], 1 - (1 - rev(decor$y)) * just[2])
    )

    if (params$direction == "vertical") {
      poly_args <- rename(poly_args, c("x", "y"), c("y", "x"))
    }

    frame <- element_grob(elems$frame)
    gp <- frame$gp %||% gpar(col = NA, fill = NA)

    polygonGrob(x = poly_args$x, y = poly_args$y, gp = gp)
  },

  fill_frame = function(frame, params, elems) {
    key <- params$key
    if (!any(c("colour", "fill") %in% names(key))) {
      return(frame)
    }

    if (all(c("min", "max") %in% names(key))) {
      check_device("clippingPaths")

      args <- list(
        x = 0.5, y = key$mid, height = key$height,
        vjust = 0.5, hjust = 0.5,
        gp = gpar(fill = key[[params$aesthetic]], col = NA)
      )
      if (params$direction == "horizontal") {
        args <- flip_names(args)
        args$height <- grobHeight(frame)
      } else {
        args$width  <- grobWidth(frame)
      }
      args$vp <- viewport(clip = frame)
      grob <- inject(rectGrob(!!!args))
      return(grob)
    }

    check_device("gradients", call = expr(gizmo_density()))
    grad_args <- list(
      x1 = 0, x2 = 1, y1 = 0.5, y2 = 0.5,
      colours = key$colour %||% key$fill, stops = key$.value
    )
    if (params$direction == "vertical") {
      grad_args <- flip_names(grad_args)
    }
    gradient <- inject(linearGradient(!!!grad_args))
    gradient <- editGrob(frame, gp = gpar(fill = gradient, col = NA))
    gradient
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    params <- replace_null(params, position = position, direction = direction)
    params <- self$setup_params(params)

    elems <- self$setup_elements(params, self$elements, theme)

    frame    <- self$build_frame(params, elems)
    gradient <- self$fill_frame(frame, params, elems)
    key <- element_grob(elems$key)

    gt <- gtable(widths = elems$width, heights = elems$height)
    gt <- gtable_add_grob(
      gt, list(key, frame, gradient), t = 1, l = 1, clip = "off",
      name = c("background", "frame", "density")
    )
  }
)

# Helpers -----------------------------------------------------------------

normalise_density <- function(density) {

  xlim <- range(filter_finite(density$x), na.rm = TRUE)
  density$x <- oob_squish_infinite(density$x, xlim)

  ylim <- range(filter_finite(density$y), na.rm = TRUE)
  density$y <- oob_squish_infinite(density$y, ylim)

  density$y <- rescale_max(density$y, to = c(0, 0.9), from = ylim)
  density
}

check_density <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is_missing(x)) {
    cli::cli_abort("{.arg {arg}} cannot be missing.", call = call)
  }
  if (inherits(x, "density")) {
    # We'll trust this class
    return(x)
  }
  check_list_names(x, c("x", "y"), arg = arg, call = call)
  if (length(x$x) != length(x$y) || length(x$x) < 2) {
    if (length(x$x) < 2 || length(x$y) < 2) {
      extra <- " and at least length 2."
    } else {
      extra <- "."
    }
    cli::cli_abort(c(
      paste0("The {.field x} and {.field y} elements in the {.arg {arg}} ",
             "argument must be of equal length{extra}"),
      i = "{.code {arg}$x} has length {length(x$x)}.",
      i = "{.code {arg}$y} has length {length(x$y)}."
    ), call = call)
  }
  check_bare_numeric(x$x, arg = as_cli("{arg}$x"), call = call)
  check_bare_numeric(x$y, arg = as_cli("{arg}$y"), call = call)
}

disallow_even_steps <- function(fun, call = caller_env()) {
  even_steps <- environment(fun)$even.steps
  if (isTRUE(even_steps)) {
    cli::cli_warn(
      "Cannot use {.code even.steps = TRUE} in this guide.",
      call = call
    )
    environment(fun)$even.steps <- FALSE
  }
  fun
}

