# Public constructor ------------------------------------------------------

#' Capped colour bar guide
#'
#' Like regular colour bar guides, this capped colour bar guide also shows
#' continuous colour scales mapped to values. In addition, this guide allows
#' you to 'cap' the ends of the colour bar, which can serve as visual indicators
#' of scale [squishing][scales::oob_squish()].
#'
#' @param cap_shape A `character(1)` indicating the shape of the cap. Can be one
#'   of `"triangle"` (default), `"round"` or `"arched"`.
#' @param cap_size A [`<unit>`][grid::unit()] object setting the size of the
#'   cap. If `NULL` (default), an appropriate size is chosen according to the
#'   `cap_shape` and `bar{width/height}` argument.
#' @param cap_position A `character(1)` indicating on which end of the colour
#'   bar to display the cap(s). Can be one of `"lower"`, `"upper"` or `"both"`.
#' @inheritDotParams guide_colourbar_vanilla
#'
#' @inherit guide_colourbar_vanilla return
#' @export
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

  guide_colourbar_vanilla(
    cap_shape    = cap_shape,
    cap_size     = cap_size,
    cap_position = cap_position,
    ...,
    super = GuideColourbarCap
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideColourbarCap <- ggproto(
  "GuideColourbarCap", GuideColourbar,

  setup_elements = function(theme, params) {

    elements <- GuideColourbar$setup_elements(theme, params)
    cap_size <- params$cap_size
    position <- params$cap_position

    if (is.null(cap_size)) {
      # Set appropriate cap_size automatically
      if (params$direction == "horizontal") {
        cap_size <- 0.5 * elements$barheight
      } else {
        cap_size <- 0.5 * elements$barwidth
      }
      if (params$cap_shape != "round") {
        cap_size <- sin((2 * pi)/3) * cap_size * 2
      }
    }

    # Make title dodge the cap by adjusting margins
    adjust <- unit(cap_size, "cm")
    mar <- elements$title$margin
    if (params$direction == "horizontal") {
      if (params$title.position == "left" && position$lower) {
        mar[2] <- mar[2] + adjust
      } else if (params$title.position == "right" && position$upper) {
        mar[4] <- mar[4] + adjust
      }
    } else {
      if (params$title.position == "top" && position$upper) {
        mar[3] <- mar[3] + adjust
      } else if (params$title.position == "bottom" && position$lower) {
        mar[1] <- mar[1] + adjust
      }
    }

    elements$title$margin <- mar
    elements$cap_size <- cap_size
    elements
  },

  build_bar = function(elements, bar, params) {
    bargrob <- GuideColourbar$build_bar(elements, bar, params)
    bargrob <- bargrob$children[[1]]

    direct   <- params$direction
    position <- params$cap_position
    width    <- elements$barwidth
    height   <- elements$barheight
    size     <- elements$cap_size
    orth     <- if (direct == "horizontal") height else width

    switch(
      params$cap_shape,
      "triangle" = {
        x <- c(0, 0.5, 1)
        y <- c(0, 1, 0)
      },
      "round" = {
        t <- seq(1, 0, length.out = 100) * pi
        x <- cos(t) * 0.5 + 0.5
        y <- sin(t)
      },
      "arched" = {
        t <- seq((1 * pi) / 3, 0, length.out = 50)
        x <- c(1 - rev(cos(t)), cos(t))
        y <- c(rev(sin(t)), sin(t)) / sin(head(t, 1))
      }
    )

    major <- unit(y * size, "cm")
    minor <- unit(x * orth, "cm")

    minor <- unit.c(unit(0, "npc"), minor, unit(1, 'npc'))
    n <- length(minor)
    if (position$lower && position$upper) {
      major <- unit.c(unit(-1, "pt"), major, unit(-1, "pt"))
      major <- unit.c(major + unit(1, "npc"), unit(0, "npc") - major)
      minor <- unit.c(minor, rev(minor))
      id    <- 1:2
      fill  <- bar$colour[c(nrow(bar), 1)]
    } else {
      major <- unit.c(unit(-1, "npc"), major, unit(-1, "npc"))
      id <- 1
      if (position$lower) {
        major <- unit(0, "npc") - major
        fill  <- bar$colour[1]
      } else {
        major <- unit(1, "npc") + major
        fill  <- bar$colour[nrow(bar)]
      }
    }

    if (direct == "horizontal") {
      x <- major
      y <- minor
    } else {
      x <- minor
      y <- major
    }

    caps <- polygonGrob(
      x = x,
      y = y,
      id = rep(id, each = n),
      gp = gpar(
        fill = fill,
        col  = NA
      )
    )

    frame <- elements$frame
    if (!is_blank(frame)) {
      frame <- polygonGrob(
        x = x,
        y = y,
        gp = gpar(
          fill = frame$fill,
          col  = frame$colour,
          lwd  = prtct_zlen(frame$size * .pt),
          lty  = frame$linetype
        )
      )
    } else {
      frame <- zeroGrob()
    }

    grobTree(
      caps, bargrob, frame
    )
  }

)

prtct_zlen <- function(x) {
  if (length(x) == 0) return(NULL) else x
}

arg_match_some <- function(arg, valid, arg_nm = caller_arg(arg),
                     warn_call = caller_env()) {
  keep     <- arg %in% valid
  dropping <- arg[!keep]
  arg      <- vec_slice(arg, keep)

  if (length(dropping)) {
    dropping <- paste0('"', dropping, '"')
    dropping <- glue_collapse(dropping, sep = ", ", last = " and ")
    valid    <- glue_collapse(valid, sep = ", ", last = " and ")
    imessage <- glue("Valid matches are: {valid}.")
    if (length(arg) > 0) {
      message <- glue(
        "The `{arg_nm}` has invalid matches that will be dropped: {dropping}."
      )
      warn(c(message, "i" = imessage), call = warn_call)
    } else {
      message
    }
  }

  if (length(arg) > 0 && length(dropping)) {


  } else if (length(arg) == 0) {
    dropping <- paste0('"', dropping, '"')
    dropping <- glue_collapse(dropping, sep = ", ", last = " and ")

  }
  arg

}
