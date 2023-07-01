# Constructor -------------------------------------------------------------

#' Extended axis guide
#'
#' Like regular [axis guides][ggplot2::guide_axis()], this extended axis guide
#' is a visual representation of position scales. It offers various additional
#' options for axis capping, display of minor ticks, subtitles and convenient
#' recolouring.
#'
#' @inheritParams ggplot2::guide_axis
#' @param subtitle A `character()` or `expression()` giving a subtitle for the
#'   guide. If `NULL` (default), no subtitle will be shown. If
#'   `length(subtitle) > 1`, text will be distributed over axis and `hjust`
#'   setting will be ignored.
#' @param subtitle.theme An [`<element_text>`][ggplot2::element_text()] object
#'   or `<element_blank>` object controlling the appearance of the `subtitle`
#'   argument. By default, it inherits from the relevant
#'   `axis.text.{x/y}.{position}` theme setting.
#' @param cap_lower,cap_upper A way to set the lower and upper ranges for
#'   axis capping Can be one of the following:
#'   * `NULL` to not perform any axis capping.
#'   * A `function` that takes the break positions as input and returns the
#'     lower or upper boundary. Note that also for discrete scales, the inputs
#'     are the mapped breaks, which are `numeric`. A `function` input can also
#'     be given as [lambda syntax][rlang::as_function()].
#'   * A `numeric` value, in data units, for the lower and upper boundaries.
#'   * A [`<unit>`][grid::unit()] object to set the boundaries independent of
#'     any data.
#' @param colour,color A `character(1)` with a valid colour for simultaneously
#'   changing the colour or the axis text, axis ticks and axis line. If `NULL`
#'   (default), inherit colours directly from the theme. Otherwise, theme
#'   settings are overruled.
#' @param minor_size A `numeric(1)` giving the relative size of minor axis ticks
#'   relative to major axis ticks, as defined by the
#'   `axis.ticks.length.{x/y}.{position}` theme element. If `NULL` (default),
#'   minor axis ticks are not shown.
#' @param major_size A `numeric(1)` giving the relative size of major ticks
#'   relative to the `axis.ticks.length.{x/y}.{position}` theme element. If
#'   `NULL` (default), major ticks are drawn to the theme's length.
#' @param ... Currently not used.
#'
#' @return A `<Guide>` ggproto object that can be given to the
#'   [`guides()`][ggplot2::guides()] function, or set as the `guide` argument
#'   in a position scale.
#' @export
#' @family axis variants
#'
#' @examples
#' # For examples with minor ticks or capping, please see:
#' # ?guide_axis_minor
#' # ?guide_axis_cap
#'
#' # A basic plot with an axis line
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   theme(axis.line.x = element_line())
#'
#' # Recolouring whole axis at once
#' p + guides(x = guide_axis_extend(colour = "red"))
#'
#' # Adding a subtitle
#' p + guides(x = guide_axis_extend(subtitle = "Engine Displacement (L)"))
#'
#' # Controlling subtitle appearance
#' p + guides(x = guide_axis_extend(
#'   title = "Engine Displacement",
#'   subtitle = "Litres",
#'   subtitle.theme = element_text(face = "italic", hjust = 1)
#' ))
#'
#' # Multiple subtitles are distributed over the axis
#' p + guides(x = guide_axis_extend(
#'   subtitle = c("Less", "Middle", "More")
#' ))
guide_axis_extend <- function(
  # Title
  title          = waiver(),

  # Subtitle
  subtitle       = NULL,
  subtitle.theme = element_text(),

  # Capping
  cap_lower      = NULL,
  cap_upper      = NULL,

  # Theming
  colour         = NULL,
  color          = NULL,

  # Ticks
  major_size     = NULL,
  minor_size     = NULL,

  # Customisations
  check.overlap  = FALSE,
  angle          = NULL,
  n.dodge        = 1,

  # General
  order          = 0,
  position       = waiver(),
  ...
) {

  colour    <- color %||% colour
  cap_lower <- simplify_cap(cap_lower, "lower")
  cap_upper <- simplify_cap(cap_upper, "upper")
  check_axis_cap(cap_lower, cap_upper)

  # Trick to re-use this constructor
  args  <- list(...)
  super <- args$super %||% GuideAxisExtend
  args$super <- NULL

  new_guide(
    # Title
    title          = title,

    # Subtitle
    subtitle       = subtitle,
    subtitle.theme = subtitle.theme,

    # Capping
    cap_lower      = cap_lower,
    cap_upper      = cap_upper,

    # Tick sizes
    major_size     = major_size,
    minor_size     = minor_size,

    # Miscellaneous
    colour         = colour,

    # Customisation
    check.overlap  = check.overlap,
    angle          = angle,
    n.dodge        = n.dodge,

    # General
    order          = order,
    position       = position,
    name           = "axis",
    available_aes  = c("x", "y"),
    !!!args,
    super          = super
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideAxisExtend <- ggproto(
  "GuideAxisExtend", GuideAxis,

  params = c(
    GuideAxis$params,
    list(
      subtitle = NULL, subtitle.theme = element_text(),
      cap_lower = unit(0, "npc"), cap_upper = unit(1, "npc"),
      major_size = NULL, minor_size = NULL, colour = NULL
    )
  ),

  extract_key = function(scale, aesthetic, major_size, minor_size, ...) {

    # Get breaks
    breaks_major <- scale$get_breaks()
    if (!is.null(minor_size)) {
      breaks_minor <- scale$get_breaks_minor()
    } else {
      breaks_minor <- NULL
    }

    # Combine breaks
    breaks   <- union(breaks_major, breaks_minor)
    is_major <- breaks %in% breaks_major
    is_minor <- !is_major & breaks %in% breaks_minor

    # Initialise key
    key <- data_frame(scale$map(breaks), .name_repair = ~ aesthetic)
    key$.value <- breaks

    # Set labels
    labels <- scale$get_labels(breaks[is_major])
    key$.label <- if (is.list(labels)) list() else ""
    key$.label[is_major] <- labels

    # Set length
    key$.size           <- 0
    key$.size[is_minor] <- minor_size %||% 0
    key$.size[is_major] <- major_size %||% 1

    if (is.numeric(breaks)) {
      key[is.finite(breaks), , drop = FALSE]
    } else {
      key
    }
  },

  extract_decor = function(scale, aesthetic, key, cap_lower, cap_upper, ...) {
    value <- key[[aesthetic]]

    # Train caps
    cap_lower <- train_cap(cap_lower, scale, value)
    cap_upper <- train_cap(cap_upper, scale, value)
    check_axis_cap(cap_lower, cap_upper)

    # Make data.frame
    cap <- new_data_frame(list(start = cap_lower, end = cap_upper))
    cap <- cap[!is.na(cap_lower) & !is.na(cap_upper), , drop = FALSE]
    colnames(cap) <- paste0(aesthetic, c("", "end"))
    cap
  },

  override_elements = function(params, elements, theme) {
    max_size <- max(params$key$.size, na.rm = TRUE)
    elements$ticks_length <- elements$ticks_length * max_size

    colour <- params$colour
    elements$line$colour  <- colour %||% elements$line$colour
    elements$text$colour  <- colour %||% elements$text$colour
    elements$ticks$colour <- colour %||% elements$ticks$colour
    elements
  },

  transform = function(self, params, coord, panel_params) {
    cap <- params$decor
    params$decor <- data_frame0()

    params <- GuideAxis$transform(params, coord, panel_params)

    do_trans <- !c(is.unit(cap[[1]]), is.unit(cap[[2]]))
    if (any(do_trans)) {
      cap[, do_trans] <- coord$transform(
        cap[, do_trans, drop = FALSE], panel_params
      )
      cap[, do_trans] <- lapply(
        cap[, do_trans, drop = FALSE], unit, units = "npc"
      )
    }
    params$decor <- cap
    params
  },

  build_ticks = function(key, elements, params, position = params$position) {
    key <- key[key$.size != 0, ]
    breaks   <- key[[params$aes]]
    n_breaks <- length(breaks)

    if (n_breaks < 1) {
      return(zeroGrob())
    }

    size <- key$.size / max(key$.size)

    tick_dir <- -2 * params$orth_side + 1
    pos <- params$orth_side + (tick_dir * (size %||% 1))
    tick <- vec_interleave(params$orth_side, pos) * elements$ticks_length
    mark <- unit(rep(breaks, each = 2), "npc")

    flip_element_grob(
      elements$ticks,
      x = tick, y = mark,
      id.lengths = rep(2, n_breaks),
      flip = position %in% c("top", "bottom")
    )
  },

  build_decor = function(decor, grobs, elements, params) {

    pos <- unit.c(decor[[1]], decor[[2]])
    pos[c(TRUE, FALSE)] <- decor[[1]]
    pos[c(FALSE, TRUE)] <- decor[[2]]

    exec(
      element_grob,
      element = elements$line,
      !!params$aes      := pos,
      !!params$orth_aes := unit(rep(params$orth_side, length(pos)), "npc")
    )
  },

  build_title = function(label, elements, params) {

    label <- params$subtitle
    if (is.null(label) || is_blank(params$subtitle.theme)) {
      return(zeroGrob())
    }
    theme  <- combine_elements(params$subtitle.theme, elements$text)
    margin <- paste0("margin_", params$orth_aes)
    if (length(label) == 1) {
      grob <- exec(
        element_grob,
        element   = theme,
        label     = label,
        !!margin := TRUE
      )
      return(grob)
    }

    ang <- theme$angle[1] %% 360
    pos <- seq(0, 1, length.out = length(label))
    ver <- (ang > 45 && ang < 135) || (ang > 255 && ang < 315)
    jus <- if (ang >= 180) 1 - pos else pos

    vjust <- theme$vjust
    hjust <- theme$hjust

    if (params$vertical) {
      if (ver) {
        hjust <- jus
      } else {
        vjust <- jus
      }
    } else {
      if (ver) {
        vjust <- 1 - jus
      } else {
        hjust <- jus
      }
    }

    exec(
      element_grob,
      element = theme,
      label   = label,
      hjust   = hjust,
      vjust   = vjust,
      !!params$aes      := unit(pos, "npc"),
      !!params$orth_aes := unit(0.5, "npc"),
      !!margin          := TRUE
    )
  }
)



# Helpers -----------------------------------------------------------------

check_axis_cap <- function(lower, upper) {
  if (is.function(lower) || is.function(upper)) {
    return(invisible())
  }
  lengths <- c(length(lower), length(upper))
  abort_if(
    lengths[1] != lengths[2] & !any(lengths == 0),
    "Axis capping must have an equal number of upper and lower capping points.",
    i = paste0(
      "{.arg cap_lower} has {lengths[1]} values, whereas {.arg cap_upper} has ",
      "{lengths[2]} values."
    )
  )
}

simplify_cap <- function(cap, type) {
  if (is_formula(cap)) {
    cap <- as_function(cap)
  }
  if (is.null(cap)) {
    cap <- unit(switch(type, lower = 0, upper = 1), "npc")
  }
  cap
}

train_cap = function(cap, scale, breaks) {
  if (is.unit(cap)) {
    return(cap)
  }
  trans <- scale$trans %||% scale$scale$trans
  if (is.function(cap)) {
    if (scale$is_discrete()) {
      cap <- cap(breaks)
    } else {
      cap <- cap(trans$inverse(breaks))
    }
  }
  scale_transform(cap, scale)
}

scale_transform <- function(x, scale) {
  # Scale can be either proper scale or viewscale placeholder.
  # Viewscales doesn't have transform method, so try parent as well.
  trans <- scale$trans %||% scale$scale$trans
  # If trans is `NULL` and not identity trans, we have a discrete scale
  # that needs to be mapped.
  if (is.null(trans)) {
    # Again, try parent of placeholder too.
    (scale$scale$map %||% scale$map)(x)
  } else {
    trans$transform(x)
  }
}
