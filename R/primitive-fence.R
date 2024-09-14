# Constructor -------------------------------------------------------------

#' Guide primitive: fence
#'
#' This function constructs a fence [guide primitive][guide-primitives]. The
#' customisation options are easier to understand if we view fence 'post' as the
#' vertical pieces of a real world fence, and the 'rail' as the horizontal
#' pieces.
#'
#' @inheritParams primitive_bracket
#' @param rail A `<character[1]>` giving an option for how to display fence
#'   railing. Can be either `"none"` (default) to display no railings, `"inner"`
#'   to draw one rail closer to the plot panel, `"outer"` to display one rail
#'   farther from the plot panel, or `"both"` to sandwich the labels between
#'   rails.
#' @param levels_post,levels_rail A list of `<element_line>` objects to
#'   customise how fence posts and rails are displayed at every level.
#'
#' @return A `<PrimitiveFence>` primitive guie that can be used inside other
#'   guides.
#' @family primitives
#' @export
#'
#' @details
#' # Styling options
#'
#' Below are the [theme][ggplot2::theme] options that determine the styling of
#' this guide, which may differ depending on whether the guide is used in an
#' axis or legend context.
#'
#' Common to both types is the following:
#'
#' * `gguidance.fence.post` an [`<element_line>`][ggplot2::element_line] for the
#'   line used to draw the pieces orthogonal to the direction of the scale.
#' * `gguidance.fence.rail` an [`<element_line>`][ggplot2::element_line] for the
#'   line used to draw the pieces parallel to the direction of the scale.
#'
#' ## As an axis guide
#'
#' * `axis.text.{x/y}.{position}` an [`<element_text>`][ggplot2::element_text]
#'   for the text displayed.
#'
#' ## As a legend guide
#'
#' * `legend.text` an [`<element_text>`][ggplot2::element_text] for the text
#'   displayed.
#'
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(interaction(drv, year), displ)) +
#'   geom_point()
#'
#' key <- key_range_manual(c(2, 4), c(5, 6), c("A", "B"))
#'
#' # Adding as secondary guides
#' p + guides(
#'   x.sec = primitive_fence(rail = "inner"),
#'   y.sec = primitive_fence(key = key, rail = "outer")
#' )
primitive_fence <- function(
  key = "range_auto",
  rail = "none",
  angle = waiver(),
  oob = "squish",
  drop_zero = TRUE,
  pad_discrete = 0.5,
  levels_text = NULL,
  levels_post = NULL,
  levels_rail = NULL,
  theme = NULL,
  position = waiver()
) {

  key <- resolve_key(key)
  oob <- arg_match0(oob, c("squish", "censor", "none"))
  rail <- arg_match0(rail, c("none", "inner", "outer", "both"))
  check_bool(drop_zero)
  check_number_decimal(pad_discrete, allow_infinite = FALSE)
  check_list_of(
    levels_text,
    c("element_text", "element_blank", "NULL"),
    allow_null = TRUE
  )
  check_list_of(
    levels_post,
    c("element_line",  "element_blank", "NULL"),
    allow_null = TRUE
  )
  check_list_of(
    levels_rail,
    c("element_line",  "element_blank", "NULL"),
    allow_null = TRUE
  )

  new_guide(
    key = key,
    oob = oob,
    rail = rail,
    angle = angle,
    drop_zero = drop_zero,
    pad_discrete = pad_discrete,
    levels_text = levels_text,
    levels_post = levels_post,
    levels_rail = levels_rail,
    theme = theme,
    position = position,
    available_aes = c("any", "x", "y", "r", "theta"),
    super = PrimitiveFence
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
PrimitiveFence <- ggproto(
  "PrimitiveFence", Guide,

  params = new_params(
    key = NULL, oob = "squish", drop_zero = TRUE,
    pad_discrete = 0.5, angle = waiver(),
    levels_text = NULL, levels_post = NULL, levels_rail = NULL,
    rail = "none"
  ),

  hashables = exprs(key$.start, key$.end),

  elements = list(
    position = list(
      text = "axis.text",
      post = I("gguidance.fence.post"),
      rail = I("gguidance.fence.rail")
    ),
    legend = list(
      text = "legend.text",
      post = I("gguidance.fence.post"),
      rail = I("gguidance.fence.rail")
    )
  ),

  extract_key = range_extract_key,

  extract_params = extract_range_params,

  extract_decor = function(scale, aesthetic, position, key, ...) {

    levels <- sort(unique(key$.level))
    key <- vec_slice(key, key$.draw)
    if (nrow(key) < 1) {
      return(NULL)
    }

    # Take unique positions by level
    split <- vec_split(c(key$start, key$end), c(key$.level, key$.level))
    split$val <- lapply(split$val, unique)

    decor <- data_frame0(
      !!aesthetic := unlist(split$val),
      .level     = min(levels),
      .level_end = rep(split$key, lengths(split$val))
    )
    decor <- vec_slice(decor, order(decor$.level_end, decor[[aesthetic]]))

    # We don't want fencepost of outer pieces poke through the railing of
    # the inner pieces.
    for (lvl in levels) {
      lower <- which(key$.level < lvl)
      current <- which(decor$.level_end == lvl)
      if (length(current) < 1 || length(lower) < 1) {
        next
      }
      trim <- in_ranges(
        decor[[aesthetic]][current],
        start = key$start[lower],
        end   = key$end[lower]
      )
      decor$.level[current[trim]] <- lvl
    }
    keep <- !duplicated(decor[c(aesthetic, ".level")], fromLast = TRUE)
    vec_slice(decor, keep)
  },

  transform = function(self, params, coord, panel_params) {
    params$key <-
      transform_key(params$key, params$position, coord, panel_params)
    params$decor <-
      transform_key(params$decor, params$position, coord, panel_params)
    params$bbox <- panel_params$bbox %||% list(x = c(0, 1), y = c(0, 1))
    params
  },

  setup_params = setup_range_params,

  setup_elements = primitive_setup_elements,

  build_fence = function(key, decor, elements, params) {

    levels   <- unique(c(key$.level, decor$.level, decor$.level_end))
    nlevels  <- length(levels)
    position <- params$position

    text_levels <- rep0(params$levels_text, length.out = nlevels)
    post_levels <- rep0(params$levels_post, length.out = nlevels)
    rail_levels <- rep0(params$levels_rail, length.out = nlevels)

    rail <- vec_slice(key, key$.draw)
    key <- justify_ranges(key, levels, elements$text, text_levels)

    if (is_theta(position)) {
      add <- if (position == "theta.sec") pi else 0
      key <- polar_xy(key, key$r, key$theta + add, params$bbox)
    }

    decor$.level <- match(decor$.level, levels)
    decor$.level_end <- match(decor$.level_end, levels)
    rail$.level <- match(rail$.level, levels)

    measure <- switch(
      position,
      left = , right = width_cm,
      top = , bottom = height_cm,
      get_size_attr
    )

    angle <- params$angle %|W|% NULL
    text <- angle_labels(elements$text, angle, position)
    offset <- elements$offset
    sizes <- numeric(nlevels + 1)
    grobs <- vector("list", nlevels)

    for (i in seq_len(nlevels)) {

      labels <- draw_labels(
        vec_slice(key, key$.level == levels[[i]]),
        combine_elements(text_levels[[i]], text),
        angle = angle, offset = offset, position = position
      )
      sizes[i + 1] <- measure(labels)
      offset <- offset + sizes[i + 1]

      fencepost <- draw_fencepost(
        vec_slice(decor, decor$.level_end == i),
        combine_elements(post_levels[[i]], elements$post),
        sizes = sizes[1:(i + 1)],
        offset = offset, position = position
      )

      fencerail <- draw_fencerail(
        vec_slice(rail, rail$.level == i),
        combine_elements(rail_levels[[i]], elements$rail),
        sizes = sizes[1:(i + 1)],
        offset = offset, position = position,
        side = params$rail, bbox = params$bbox
      )

      grobs[[i]] <- grobTree(fencepost, fencerail, labels)
    }

    sizes <- sizes[-1]
    if (position %in% c("top", "left")) {
      grobs <- rev(grobs)
      sizes <- rev(sizes)
    }

    attr(grobs, "sizes") <- sizes
    grobs
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    params <- replace_null(params, position = position, direction = direction)
    params <- self$setup_params(params)

    elems <- self$setup_elements(params, self$elements, theme)
    fence <- self$build_fence(params$key, params$decor, elems, params)

    if (length(fence) < 1) {
      return(zeroGrob())
    }

    primitive_grob(
      grob = fence,
      size = unit(attr(fence, "sizes"), "cm"),
      position = params$position,
      name = "fence"
    )
  }

)

# Helpers -----------------------------------------------------------------

draw_fencerail <- function(rail, element, sizes, offset, position, side, bbox) {
  if (side == "none" || nrow(rail) < 1 || is_blank(element)) {
    return(NULL)
  }

  if (is_theta(position)) {
    n <- as.integer(round(rail$thetaend - rail$theta) / (pi / 45))
    n <- pmax(n, 2L)

    theta <- Map(seq, rail$theta, rail$thetaend, length.out = n)
    i     <- rep(seq_along(theta), lengths(theta))

    xy <- data_frame0(
      theta = unlist(theta),
      r = rail$r[i],
      i = i
    )
    xy <- polar_xy(xy, xy$r, xy$theta, bbox)
    levels <- rail$.level[i]

    if (side == "inner") {
      r <- unit(rep(offset - sizes[rail$.level + 1], n), "cm")
    } else if (side == "outer") {
      r <- unit(rep(offset, sum(n)), "cm")
    } else {
      r <- unit(c(
        rep(offset - sizes[rail$.level + 1], n),
        rep(offset, sum(n))
      ), "cm")
      xy$i <- c(1, xy$i[-1] != xy$i[-nrow(xy)])
      xy <- vec_c(xy, xy)
      xy$i <- cumsum(xy$i)
    }

    rails <- element_grob(
      element,
      x = unit(xy$x, "npc") + sin(xy$theta) * r,
      y = unit(xy$y, "npc") + cos(xy$theta) * r,
      id.lengths = vec_unrep(xy$i)$times
    )
    return(rails)
  }

  aes <- switch(position, top = , bottom = "x", left = , right = "y", "theta")
  aesend <- paste0(aes, "end")

  mark <- vec_interleave(rail[[aes]], rail[[aesend]])
  if (side == "inner") {
    tick <- rep(0, length(mark))
  } else if (side == "outer") {
    tick <- rep(1, length(mark))
  } else {
    tick <- rep(c(0, 1), each = length(mark))
    mark <- c(mark, mark)
  }
  mark <- unit(mark, "npc")
  tick <- switch(
    position,
    top = , right = unit(0 + tick, "npc"),
    unit(1 - tick, "npc")
  )

  args <- list(x = tick, y = mark, id.lengths = rep(2L, length(tick) / 2))
  if (position %in% c("top", "bottom")) {
    args <- flip_names(args)
  }
  inject(element_grob(element, !!!args))
}

draw_fencepost <- function(decor, element, sizes, offset, position) {
  if (nrow(decor) < 1 || is_blank(element)) {
    return(NULL)
  }

  levels <- vec_interleave(decor$.level, decor$.level_end + 1)

  if (is_theta(position)) {
    angle <- rep(decor$theta, each = 2)
    x     <- rep(decor$x,     each = 2)
    y     <- rep(decor$y,     each = 2)
    length <- unit(offset - cumsum(sizes)[levels], "cm")

    ticks <- element_grob(
      element,
      x = unit(x, "npc") + sin(angle) * length,
      y = unit(y, "npc") + cos(angle) * length,
      id.lengths = rep(2, nrow(decor))
    )
    return(ticks)
  }

  aes <- switch(position, top = , bottom = "x", left = , right = "y", "theta")
  mark <- unit(rep(decor[[aes]], each = 2), "npc")

  tick <- unit(offset - cumsum(sizes)[levels], "cm")
  tick <- switch(
    position,
    top = , right = unit(1, "npc") - tick,
    unit(0, "npc") + tick
  )

  args <- list(x = tick, y = mark, id.lengths = rep(2L, nrow(decor)))
  if (position %in% c("top", "bottom")) {
    args <- flip_names(args)
  }
  inject(element_grob(element, !!!args))
}

