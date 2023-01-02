# Public constructor ------------------------------------------------------

#' Capped colour bar guide
#'
#' Like regular colour bar guides, this capped colour bar guide also shows
#' continuous colour scales mapped to values. In addition, this guide allows
#' you to 'cap' the ends of the colour bar, which can serve as visual indicators
#' of scale [squishing][scales::oob_squish()]. The cap itself will be filled
#' with the colours at the extremes.
#'
#' @param cap_shape A `character(1)` indicating the shape of the cap. Can be one
#'   of `"triangle"` (default), `"round"` or `"arched"`.
#' @param cap_size A [`<unit>`][grid::unit()] object setting the size of the
#'   cap. If `NULL` (default), an appropriate size is chosen according to the
#'   `cap_shape` and `bar{width/height}` argument.
#' @param cap_position A `character(1)` indicating on which end of the colour
#'   bar to display the cap(s). Can be one of `"lower"`, `"upper"` or `"both"`.
#' @inheritDotParams ggplot2::guide_colourbar
#'
#' @return A `<Guide>` ggproto object that can be given to the
#'   [`guides()`][ggplot2::guides()] function, or set as the `guide` argument
#'   in a colour or fill scale.
#'
#' @export
#' @name guide_colourbar_cap
#' @family colour bar variants
#'
#' @examples
#' # A standard plot with continuous colour scale
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = cty)) +
#'   scale_colour_viridis_c()
#'
#' # By default, the bar is capped by equilateral triangles
#' p + guides(colour = guide_colourbar_cap())
#'
#' # This can be changed by setting a different cap shape
#' p + guides(colour = guide_colourbar_cap(cap_shape = "arched"))
#'
#' # To cap just one end, use the `cap_position` argument
#' p + guides(colour = guide_colourbar_cap(cap_position = "upper"))
#'
#' # The `cap_size` argument can be used to stretch or squish the cap
#' p + guides(colour = guide_colourbar_cap(cap_size  = unit(1, "cm"),
#'                                         cap_shape = "round"))
guide_colourbar_cap <- function(
  cap_shape    = "triangle",
  cap_size     = NULL,
  cap_position = "both",
  ...
) {

  cap_shape    <- arg_match0(cap_shape, c("triangle", "round", "arched"))
  cap_position <- arg_match0(cap_position, c("lower", "upper", "both"))
  cap_position <- list(
    lower = cap_position %in% c("lower", "both"),
    upper = cap_position %in% c("upper", "both")
  )
  cap_size <- arg_class(cap_size, c("NULL", "unit"))[1]

  guide_colourbar(
    cap_shape    = cap_shape,
    cap_size     = cap_size,
    cap_position = cap_position,
    ...,
    super = GuideColourbarCap
  )
}

#' @rdname guide_colourbar_cap
#' @export
guide_colorbar_cap <- guide_colourbar_cap

# Classes ------------------------------------------------------------------

GuideColourbarCap <- ggproto(
  "GuideColourbarCap", GuideColourbar,
  params = c(
    GuideColourbar$params,
    list(cap_shape = "triangle", cap_size = NULL, cap_position = "both",
         steps = FALSE)
  ),

  override_elements = function(params, elements, theme) {
    elements <- GuideColourbar$override_elements(params, elements, theme)

    cap_size     <- params$cap_size

    # Resolve cap size
    if (is.null(cap_size)) {
      if (params$direction == "horizontal") {
        cap_size <- 0.5 * elements$key.height
      } else {
        cap_size <- 0.5 * elements$key.width
      }
      if (params$cap_shape != "round") {
        cap_size <- sin((2 * pi) / 3) * cap_size * 2
      }
    }

    elements$cap_size <- cap_size
    elements
  },

  build_decor       = function(decor, ticks, elements, params) {
    bargrob <- GuideColourbar$build_decor(decor, ticks, elements, params)
    bar <- bargrob$bar

    direction <- params$direction
    position  <- params$cap_position
    width     <- elements$key.width
    height    <- elements$key.height

    size <- elements$cap_size
    orth <- if (direction == "horizontal") height else width

    switch(
      params$cap_shape,
      triangle = {
        x <- c(0, 0.5, 1)
        y <- c(0,   1, 0)
      },
      round = {
        t <- seq(1, 0, length.out = 100) * pi
        x <- cos(t) * 0.5 + 0.5
        y <- sin(t)
      },
      arched = {
        t <- seq((1 * pi) / 3, 0, length.out = 50)
        x <- c(1 - rev(cos(t)), cos(t))
        y <- c(rev(sin(t)), sin(t)) / sin(head(t, 1))
      }
    )

    major <- unit(y * size, "cm")
    minor <- unit(x * orth, "cm")
    minor <- unit.c(unit(0, "npc"), minor, unit(1, "npc"))
    n <- length(minor)

    if (position$lower && position$upper) {
      major <- unit.c(unit(-1, "pt"), major, unit(-1, "pt"))
      major <- unit.c(major + unit(1, "npc"), unit(0, "npc") - major)
      minor <- unit.c(minor, rev(minor))
      id    <- 1:2
      fill  <- decor$colour[c(nrow(decor), 1)]
    } else {
      major <- unit.c(unit(-1, "pt"), major, unit(-1, "pt"))
      id    <- 1
      if (position$lower) {
        major <- unit(0, "npc") - major
        fill  <- decor$colour[1]
      } else {
        major <- unit(1, "npc") + major
        fill  <- decor$colour[nrow(decor)]
      }
    }

    if (direction == "horizontal") {
      x <- major
      y <- minor
    } else {
      x <- minor
      y <- major
    }

    caps <- polygonGrob(
      x = x, y = y,
      id = rep(id, each = n),
      gp = gpar(fill = fill, col = NA)
    )

    frame <- elements$frame
    if (!is_blank(frame)) {
      frame <- polygonGrob(
        x = x, y = y,
        gp = gpar(
          fill = frame$fill,
          col  = frame$colour,
          lwd  = if (length(frame$size) == 0) NULL else (frame$size * .pt),
          lty  = frame$linetype
        )
      )
    } else {
      frame <- zeroGrob()
    }

    list(
      cap = caps, bar = bargrob$bar, frame = frame, ticks = bargrob$ticks
    )
  },

  assemble_drawing  = function(grobs, layout, sizes, params, elements) {
    gt <- GuideColourbar$assemble_drawing(grobs, layout, sizes, params, elements)
    cap_size <- elements$cap_size
    position <- unlist(params$cap_position, FALSE, FALSE)
    cap_size <- unit(cap_size, "cm") * as.numeric(position)

    if (params$direction == "horizontal") {
      i <- layout$layout$key_col[1] + c(-1, 1)
      gt$widths[i] <- gt$widths[i] + cap_size
    } else {
      i <- layout$layout$key_row[1] + c(-1, 1)
      gt$heights[i] <- gt$heights[i] + rev(cap_size)
    }
    gt
  }
)

