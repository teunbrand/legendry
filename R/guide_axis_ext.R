# Public constructor ------------------------------------------------------

#' Extended axis guide
#'
#' Like regular [axis guides][ggplot2::guide_axis()], this extended axis guide
#' is a visual representation of position scales. It offers various additional
#' options for axis truncation, display of minor ticks, subtitles and convenient
#' recolouring.
#'
#' @inheritParams guide_axis_vanilla
#' @param subtitle A `character(1)` or `expression(1)` giving a subtitle for the
#'   guide. If `NULL` (default), no subtitle will be shown.
#' @param subtitle.theme An [`<element_text>`][ggplot2::element_text()] object
#'   or `<element_blank>` object controlling the appearance of the `subtitle`
#'   argument. By default, it inherits from the relevant
#'   `axis.text.{x/y}.{position}` theme setting.
#' @param trunc_lower,trunc_upper A way to set the lower and upper ranges for
#'   axis truncation. Can be one of the following:
#'   * `NULL` to not perform any axis truncation.
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
#'
#' @inherit guide_axis_vanilla return
#' @export
#' @family axis variants
#'
#' @examples
#' # For examples with minor ticks or truncation, please see:
#' # ?guide_axis_minor
#' # ?guide_axis_trunc
#'
#' # A basic plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Recolouring whole axis at one
#' p + guides(x = guide_axis_ext(colour = "red")) +
#'   theme(axis.line.x = element_line())
#'
#' # Adding a subtitle
#' p + guides(x = guide_axis_ext(subtitle = "Engine Displacement (L)"))
#'
#' # Controlling subtitle appearance
#' p + guides(x = guide_axis_ext(
#'   title = "Engine Displacement",
#'   subtitle = "Litres",
#'   subtitle.theme = element_text(face = "italic")
#' ))
guide_axis_ext <- function(
  # Title
  title = waiver(),

  # Subtitle
  subtitle       = NULL,
  subtitle.theme = element_text(),

  # General
  check.overlap  = FALSE,
  angle          = NULL,
  n.dodge        = 1L,
  order          = 0,
  position       = waiver(),

  # Truncation
  trunc_lower    = NULL,
  trunc_upper    = NULL,

  # Theming
  colour         = NULL,
  color          = NULL,

  # Minor ticks
  minor_size     = NULL,
  major_size     = NULL,

  ...
  ) {

  colour      <- color %||% colour
  trunc_lower <- simplify_trunc(trunc_lower, "lower")
  trunc_upper <- simplify_trunc(trunc_upper, "upper")
  check_trunc_arg(trunc_lower, trunc_upper)

  # Method to override the default super without exposing as formal argument
  # to user. Passing super should be an internal matter.
  construct <- function(..., super = GuideAxisExt) {
    construct_axis(..., super = super)
  }

  construct(
    title          = title,
    subtitle       = subtitle,
    subtitle.theme = subtitle.theme,
    check.overlap  = check.overlap,
    angle          = angle,
    n.dodge        = n.dodge,
    order          = order,
    position       = position,
    trunc_lower    = trunc_lower,
    trunc_upper    = trunc_upper,
    colour         = colour,
    minor_size     = minor_size,
    major_size     = major_size,
    ...
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideAxisExt <- ggproto(
  "GuideAxisExt", GuideAxis,

  ## Method implementation ------------------------------------------------

  train = function(self, scale, aesthetic) {
    params <- self$params

    # Get all levels of breaks
    breaks_major <- scale$get_breaks()
    breaks_minor <- self$calc_minor(breaks_major, scale, params$minor_size)
    breaks_mini  <- self$calc_mini(breaks_major,  scale, params$mini_size)

    # Combine and assign breaks
    breaks   <- union(union(breaks_major, breaks_minor), breaks_mini)
    is_major <- breaks %in% breaks_major
    is_minor <- !is_major & breaks %in% breaks_minor

    mapped_breaks <- if (scale$is_discrete()) scale$map(breaks) else breaks

    # Set aesthetic and values
    key <- new_data_frame(setNames(list(mapped_breaks), aesthetic))
    key$.value <- breaks

    # Set labels
    labels <- scale$get_labels(breaks[is_major])
    key$.label <- if (is.list(labels)) list() else ""
    key$.label[is_major] <- labels

    # Set length
    key$.length           <- params$mini_size  %||% 0
    key$.length[is_minor] <- params$minor_size %||% 0
    key$.length[is_major] <- params$major_size %||% 1

    key <- vec_slice(key, !is.na(key[[aesthetic]]))

    self$set_trunc(
      self$truncate(params$trunc_lower, scale, key[[aesthetic]]),
      self$truncate(params$trunc_upper, scale, key[[aesthetic]]),
      aesthetic
    )

    key
  },

  transform = function(self, coord, panel_params, aesthetic) {
    ggproto_parent(GuideAxis, self)$transform(coord, panel_params, aesthetic)
    trunc <- self$params$trunc
    is_unit <- c(is.unit(trunc[[1]]), is.unit(trunc[[2]]))
    if (any(!is_unit)) {
      trunc[, !is_unit] <- coord$transform(
        trunc[, !is_unit, drop = FALSE], panel_params
      )
      trunc[, !is_unit] <- lapply(
        trunc[, !is_unit, drop = FALSE], unit, units = "npc"
      )
    }
    self$params$trunc <- trunc
    return(invisible())
  },

  draw_guide = function(self, theme) {
    # Extract relevant info
    key       <- self$key
    aesthetic <- aes_from_key(key)
    position  <- match.arg(substr(self$position, 1, 1), c("t", "b", "l", "r"))
    params    <- self$params

    # Setup stuff
    elements  <- self$setup_elements(position, theme, params)
    params    <- self$setup_params(position, params)

    # Actual drawing stuff
    line_grob <- self$build_line(elements, params)
    if (nrow(key) == 0) {
      return(absGrob(line_grob))
    }

    label_grob <- self$build_labels(elements, key, params)
    tick_grob  <- self$build_ticks(elements, key, params)

    # Do tick adjustment
    elements$tick_length <- elements$tick_length * max(key$.length %||% 1)

    self$assemble_drawing(
      ticks    = tick_grob,
      labels   = label_grob,
      lines    = line_grob,
      elements = elements,
      params   = params
    )
  },

  ## Drawing helpers ------------------------------------------------------

  build_line = function(elements, params) {
    trunc <- params$trunc
    pos <- unit.c(trunc[[1]], trunc[[2]])
    pos[c(TRUE, FALSE)] <- trunc[[1]]
    pos[c(FALSE, TRUE)] <- trunc[[2]]

    args <- list(
      elements$line, pos,
      rep(params$pos, nrow(trunc) * 2),
      rep(2, nrow(trunc))
    )
    names(args) <- c("element", params$aes, params$alt_aes, "id.lengths")
    do.call(element_grob, args)
  },

  build_ticks = function(elements, key, params) {
    breaks   <- key[[params$aes]]
    n_breaks <- length(breaks)

    key$.length <- key$.length / max(key$.length)

    pos <- params$pos + (params$tick_dir * (key$.length %||% 1))
    pos <- vec_interleave(params$pos, pos)

    args <- list(
      elements$ticks,
      unit(rep(breaks, each = 2), "native"),
      unit(pos, "npc"),
      rep(2, times = n_breaks)
    )
    names(args) <- c("element", params$aes, params$alt_aes, "id.lengths")
    do.call(element_grob, args)
  },

  build_labels = function(elements, key, params) {
    labels <- GuideAxis$build_labels(elements, key, params)
    if (is.null(params$subtitle) || is_blank(elements$subtitle)) {
      return(labels)
    }
    subtitle <- element_grob(
      elements$subtitle,
      label = params$subtitle[1]
    )
    c(labels, list(subtitle))
  },

  setup_elements = function(position, theme, params) {
    elements <- GuideAxis$setup_elements(position, theme, params)
    if (is_blank(params$subtitle.theme) || is.null(params$subtitle)) {
      elements$subtitle <- element_blank()
      return(elements)
    }
    aes <- if (position %in% c("t", "b")) "x" else "y"
    pos <- match.arg(position, c("top", "bottom", "left", "right"))

    # We don't directly inherit from computed labels to bypass
    # the angle override. Instead we recompute it
    labels <- calc_element(paste0("axis.text.", aes, ".", pos), theme)
    subtitle <- combine_elements(params$subtitle.theme, labels)
    if (!is.null(params$colour)) {
      subtitle$colour <- params$colour
    }
    if (is.null(params$subtitle.theme$hjust)) {
      subtitle$hjust <- 1
    }
    # We also change the defaults of angles on vertical axes
    if (pos == "left") {
      subtitle$angle <- params$subtitle.theme$angle %||% 90
    }
    if (pos == "right") {
      subtitle$angle <- params$subtitle.theme$angle %||% 270
    }
    elements$subtitle <- subtitle
    elements
  },

  ## Break calculation helpers --------------------------------------------

  calc_minor = function(breaks, scale, size) {
    if (is.null(size)) {
      return(NULL)
    }
    scale$get_breaks_minor()
  },

  calc_mini = function(breaks, scale, size) {
    return(NULL)
  },

  ## Truncation -----------------------------------------------------------

  truncate = function(trunc, scale, breaks) {
    if (is.unit(trunc)) {
      return(trunc)
    }
    trans <- scale$trans %||% scale$scale$trans

    if (is.function(trunc)) {
      if (scale$is_discrete()) {
        trunc <- trunc(breaks)
      } else {
        trunc <- trunc(trans$inverse(breaks))
      }
    }

    if (scale$is_discrete()) {
      (scale$scale$map %||% scale$map)(trunc)
    } else {
      trans$transform(trunc)
    }
  },

  set_trunc = function(self, lower, upper, aesthetic) {
    check_trunc_arg(lower, upper)
    trunc <- new_data_frame(list(start = lower, end = upper))
    trunc <- trunc[!is.na(lower) & !is.na(upper), ]
    colnames(trunc) <- paste0(aesthetic, c("", "end"))
    self$params$trunc <- trunc
    self$params$trunc_lower <- self$params$trunc_upper <- NULL
    return(invisible())
  }
)

# Helpers -----------------------------------------------------------------

check_trunc_arg <- function(lower, upper) {
  if (!is.function(lower) && !is.function(upper)) {
    lens <- c(length(lower), length(upper))
    if (lens[1] != lens[2] & !any(lens == 0)) {
      abort(
        paste0(
          "Axis truncations must have an equal number of ",
          "upper and lower truncation points."
        )
      )
    }
  }
}

simplify_trunc <- function(trunc, type) {
  if (is_formula(trunc)) {
    trunc <- as_function(trunc)
  }
  if (is.null(trunc)) {
    trunc <- unit(switch(type, "lower" = 0, "upper" = 1), "npc")
  }
  trunc
}
