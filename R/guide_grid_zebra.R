# Public constructor ------------------------------------------------------

#' Zebra grid guide
#'
#' This grid guide draws alternating stripes along one of the axes, resembling
#' a zebra crossing (or indeed, the animal itself).
#'
#' @param along A `character(1)`, one of `"x"` or `"y"` indicating along which
#'   direction the stripes should occur.
#' @param just A `numeric(1)` between 0 and 1, setting the justification of the
#'   stripes relative to the major ticks.
#' @param at A `character(1)`, one of `"major"` or `"minor"` indicating what
#'   breaks to consider for making stripes. Note that `"minor"` automatically
#'   includes the major breaks.
#' @param on_top A `logical(1)`. If `TRUE`, stripes are drawn on top of the
#'   grid lines of the orthogonal direction. The default is `FALSE`, in which
#'   case stripes are drawn underneath those grid lines.
#' @param evenodd A `character(1)`, one of `"even"` or `"odd"` indicating which
#'   breaks to take for stripes.
#'
#' @inherit guide_grid_vanilla return
#' @export
#' @family grid variants
#'
#' @examples
#' # A standard plot
#' p <- ggplot(economics, aes(date, unemploy)) +
#'   geom_line()
#'
#' # By default, stripes are centered at major breaks
#' p + coord_guided("grid_zebra")
#'
#' # To set stripes in between breaks, use the `just` argument
#' p + coord_guided(guide_grid_zebra(just = 0))
#'
#' # Alternatively, also use minor breaks
#' p + coord_guided(guide_grid_zebra(at = "minor", just = 1))
#'
#' # Stripe colour is controlled by the major panel grid
#' p + coord_guided("grid_zebra") +
#'   theme(panel.grid.major.x = element_line(colour = "grey80"))
guide_grid_zebra <- function(
  along = "x", just = 0.5, at = "major", on_top = FALSE, evenodd = "odd"
) {
  just    <- oob_squish(just, c(0, 1))
  at      <- arg_match0(at, c("major", "minor"))
  evenodd <- arg_match0(evenodd, c("even", "odd"))
  along   <- arg_match0(along, c("x", "y"))

  construct_grid(
    along   = along,
    just    = just,
    at      = at,
    on_top  = isTRUE(on_top),
    evenodd = evenodd,
    super   = GuideGridZebra
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideGridZebra <- ggproto(
  "GuideGridZebra", GuideGrid,

  build_xgrid = function(params, elements) {
    if (!(params$along == "x")) {
      ans <- GuideGrid$build_xgrid(params, elements)
      return(ans)
    }
    if (is_blank(elements$x_major)) {
      return(list())
    }
    elem <- elements$x_major
    elem <- element_rect(
      fill   = elem$colour,
      colour = NA
    )
    if (params$at == "major") {
      breaks <- params$x_major
    } else {
      breaks <- union(params$x_major, params$x_minor)
    }
    breaks <- sort(breaks[is.finite(breaks)])
    n      <- length(breaks)
    width  <- c(breaks[2] - breaks[1], diff(breaks))

    min <- breaks - params$just * width
    max <- breaks + (1 - params$just) * width
    min <- oob_squish(min, c(0, 1))
    max <- oob_squish(max, c(0, 1))

    center <- (min + max) / 2
    width  <- (max - min)
    if (params$evenodd == "odd") {
      keep <- seq_along(breaks) %% 2 == 1
    } else {
      keep <- seq_along(breaks) %% 2 == 0
    }

    element_grob(
      elem,
      x = center[keep], y = 0.5,
      width = width[keep], height = 1
    )
  },

  build_ygrid = function(params, elements) {
    if (!(params$along == "y")) {
      ans <- GuideGrid$build_ygrid(params, elements)
      return(ans)
    }
    if (is_blank(elements$y_major)) {
      return(list())
    }
    elem <- elements$y_major
    elem <- element_rect(
      fill   = elem$colour,
      colour = NA
    )
    if (params$at == "major") {
      breaks <- params$y_major
    } else {
      breaks <- union(params$y_major, params$y_minor)
    }
    breaks <- sort(breaks[is.finite(breaks)])
    n      <- length(breaks)
    height  <- c(breaks[2] - breaks[1], diff(breaks))

    min <- breaks - params$just * height
    max <- breaks + (1 - params$just) * height
    min <- oob_squish(min, c(0, 1))
    max <- oob_squish(max, c(0, 1))

    center <- (min + max) / 2
    height <- (max - min)
    if (params$evenodd == "odd") {
      keep <- seq_along(breaks) %% 2 == 1
    } else {
      keep <- seq_along(breaks) %% 2 == 0
    }

    element_grob(
      elem,
      x = 0.5, y = center[keep],
      width = 1, height = height[keep]
    )
  },

  assemble_drawing = function(background, xgrid, ygrid, params) {
    if (params$along == "x") {
      if (isTRUE(params$on_top)) {
        args <- list(background, ygrid$minor, ygrid$major, xgrid)
      } else {
        args <- list(background, xgrid, ygrid$minor, ygrid$major)
      }
    } else {
      if (isTRUE(params$on_top)) {
        args <- list(background, xgrid$minor, xgrid$major, ygrid)
      } else {
        args <- list(background, ygrid, xgrid$minor, xgrid$major)
      }
    }
    args <- args[!vapply(args, is.null, logical(1))]
    grob <- do.call("grobTree", args)
    grob$name <- grobName(grob, "grill")
    grob
  }
)
