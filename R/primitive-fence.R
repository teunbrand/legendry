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
#' ## Styling options
#'
#' Below are the [theme][ggplot2::theme] options that determine the styling of
#' this guide, which may differ depending on whether the guide is used in
#' an axis or in a legend context.
#'
#' The possible `{position}` suffixes mentioned below are `x`, `x.top`,
#' `x.bottom`, `y`, `y.left`, `y.right`. The `theta` and `r` position suffixes
#' in \pkg{ggplot2} are *not* obeyed in \pkg{legendry}.
#'
#' | **Theme setting** | **Context** | **Type** | **Description** |
#' | ----------------- | ----------- | -------- | --------------- |
#' | `legendry.fence`  | Both | [`element_line()`] | Line segments for both 'post' and 'rail' segments |
#' | `legendry.fence.post` | Both | [`element_line()`] | Line segments orthogonal to the scale |
#' | `legendry.fence.rail` | Both | [`element_line()`] | Line segments parallel to the scale |
#' | `axis.text.{position}` | Axis | [`element_text()`] | The text labels at the fence. |
#' | `legend.text` | Legend | [`element_text()`] | The text labels at the fence. |
#'
#' Styling options *per level* can be set in the `levels_post`, `levels_rail`
#' and `levels_text` arguments. These override theme settings.
#'
#' Styling options *per range* can be set in the [range key][key_range].
#' The `line` and `text` prefixed properties are prioritised for the fence
#' and text respectively. The 'post' and 'rail' distinction does not apply
#' at the 'per range' settings. These override theme settings and the
#' 'per level' settings.
#'
#' The context-agnostic alternative to using `theme()` is to use
#' [`theme_guide()`]:
#'
#' ```r
#' primitive_fence(theme = theme_guide(
#'   fence = element_line(),
#'   fence.post = element_line(),
#'   fence.rail = element_line(),
#'   text = element_text()
#' ))
#' ```
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
    levels_text, element_classes("text", "blank"),
    allow_null = TRUE
  )
  check_list_of(
    levels_post, element_classes("line", "blank"),
    allow_null = TRUE
  )
  check_list_of(
    levels_rail, element_classes("line", "blank"),
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
#' @rdname legendry_extensions
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

  hashables = exprs(key, decor),

  elements = list(
    position = list(
      text = "axis.text",
      post = I("legendry.fence.post"),
      rail = I("legendry.fence.rail")
    ),
    legend = list(
      text = "legend.text",
      post = I("legendry.fence.post"),
      rail = I("legendry.fence.rail")
    )
  ),

  extract_key = range_extract_key,

  extract_params = extract_range_params,

  extract_decor = function(scale, aesthetic, position, key, ...) {

    levels <- sort(unique(key$.level))
    key <- vec_slice(key, key$.draw)
    if (nrow(key) < 1L) {
      return(NULL)
    }

    # Take unique positions by level
    split <- vec_split(key, key$.level)

    decor <- lapply(split$val, function(df) {
      aes <- vec_interleave(df$start, df$end)
      df <- vec_rep_each(df[setdiff(names(df), c("start", "end"))], 2)
      df[[aesthetic]] <- aes
      df
    })

    level_end <- rep(split$key, list_sizes(decor))
    decor <- vec_c(!!!decor)
    decor$.level <- min(levels)
    decor$.level_end <- level_end
    decor <- vec_slice(decor, order(decor$.level_end, decor[[aesthetic]]))

    # We don't want fencepost of outer pieces poke through the railing of
    # the inner pieces.
    for (lvl in levels[-1L]) {
      lower <- which(key$.level == lvl - 1L)
      current <- which(decor$.level_end >= lvl)
      if (length(current) < 1L || length(lower) < 1L) {
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
    params$bbox <- panel_params$bbox %||%
      list(x = c(0.0, 1.0), y = c(0.0, 1.0))
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
      add  <- if (position == "theta.sec") pi else 0.0
      key  <- polar_xy(key, key$r,   key$theta  + add, params$bbox)
      rail <- polar_xy(rail, rail$r, rail$theta + add, params$bbox)
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
    sizes <- numeric(nlevels + 1L)
    grobs <- vector("list", nlevels)

    for (i in seq_len(nlevels)) {

      labels <- draw_labels(
        vec_slice(key, key$.level == levels[[i]]),
        combine_elements(text_levels[[i]], text),
        angle = angle, offset = offset, position = position
      )
      sizes[i + 1L] <- measure(labels)
      offset <- offset + sizes[i + 1L]

      fencepost <- draw_fencepost(
        vec_slice(decor, decor$.level_end == i),
        combine_elements(post_levels[[i]], elements$post),
        sizes = sizes[1L:(i + 1L)],
        offset = offset, position = position
      )

      fencerail <- draw_fencerail(
        vec_slice(rail, rail$.level == i),
        combine_elements(rail_levels[[i]], elements$rail),
        sizes = sizes[1L:(i + 1L)],
        offset = offset, position = position,
        side = params$rail, bbox = params$bbox
      )

      grobs[[i]] <- grobTree(fencepost, fencerail, labels)
    }

    sizes <- sizes[-1L]
    if (position %in% c("top", "left")) {
      grobs <- rev(grobs)
      sizes <- rev(sizes)
    }

    attr(grobs, "size") <- sizes
    grobs
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    params <- replace_null(params, position = position, direction = direction)
    params <- self$setup_params(params)

    elems <- self$setup_elements(params, self$elements, theme)
    fence <- self$build_fence(params$key, params$decor, elems, params)

    if (length(fence) < 1L) {
      return(zeroGrob())
    }

    primitive_grob(
      grob = fence,
      size = unit(get_size_attr(fence), "cm"),
      position = params$position,
      name = "fence"
    )
  }

)

# Helpers -----------------------------------------------------------------

draw_fencerail <- function(rail, element, sizes, offset, position, side, bbox) {
  if (side == "none" || nrow(rail) < 1L || is_blank(element)) {
    return(NULL)
  }

  if (is_theta(position)) {
    n <- as.integer(round(rail$thetaend - rail$theta) / (pi / 45.0))
    n <- pmax(n, 2L)

    theta <- Map(seq, rail$theta, rail$thetaend, length.out = n)
    i     <- rep(seq_along(theta), lengths(theta))

    add <- as.integer(position == "theta.sec")
    xy <- data_frame0(
      theta = unlist(theta) + add * pi,
      r = rail$r[i],
      i = i
    )
    xy <- polar_xy(xy, xy$r, xy$theta, bbox)

    if (side == "inner") {
      r <- unit(rep(offset - sizes[rail$.level + 1L], n), "cm")
    } else if (side == "outer") {
      r <- unit(rep(offset, sum(n)), "cm")
    } else {
      r <- unit(c(
        rep(offset - sizes[rail$.level + 1L], n),
        rep(offset, sum(n))
      ), "cm")
      xy$i <- c(1L, xy$i[-1L] != xy$i[-nrow(xy)])
      xy <- vec_c(xy, xy)
      xy$i <- cumsum(xy$i)
    }
    if (add == 1L) {
      r <- r * -1.0
    }

    args <- list(
      x = unit(xy$x, "npc") + sin(xy$theta) * r,
      y = unit(xy$y, "npc") + cos(xy$theta) * r,
      id.lengths = vec_unrep(xy$i)$times
    )
  } else {

    aes <- switch(position, top = , bottom = "x", left = , right = "y", "theta")
    aesend <- paste0(aes, "end")

    mark <- vec_interleave(rail[[aes]], rail[[aesend]])
    if (side == "inner") {
      tick <- rep(0.0, length(mark))
    } else if (side == "outer") {
      tick <- rep(1.0, length(mark))
    } else {
      tick <- rep(c(0.0, 1.0), each = length(mark))
      mark <- c(mark, mark)
    }
    mark <- unit(mark, "npc")

    tick <- switch(
      position,
      top = , right = unit(0.0 + tick, "npc"),
      unit(1.0 - tick, "npc")
    )

    args <- list(x = tick, y = mark, id.lengths = rep(2L, length(tick) / 2L))
    if (position %in% c("top", "bottom")) {
      args <- flip_names(args)
    }

  }

  props <- element_key_properties(rail, "line")

  inject(element_grob(element, !!!args, !!!props))
}

draw_fencepost <- function(decor, element, sizes, offset, position) {
  if (nrow(decor) < 1L || is_blank(element)) {
    return(NULL)
  }

  levels <- vec_interleave(decor$.level, decor$.level_end + 1L)

  if (is_theta(position)) {
    add <- as.integer(position == "theta.sec")

    angle <- rep(decor$theta, each = 2L) + add * pi
    x     <- rep(decor$x,     each = 2L)
    y     <- rep(decor$y,     each = 2L)
    length <- cumsum(sizes)[levels] + offset - sum(sizes)
    if (add == 1L) {
      length <- length * -1L
    }
    length <- unit(length, "cm")

    args <- list(
      x = unit(x, "npc") + sin(angle) * length,
      y = unit(y, "npc") + cos(angle) * length
    )
  } else {
    aes <- switch(position, top = , bottom = "x", left = , right = "y", "theta")
    mark <- unit(rep(decor[[aes]], each = 2L), "npc")

    tick <- unit(offset - cumsum(sizes)[levels], "cm")
    tick <- switch(
      position,
      top = , right = unit(1.0, "npc") - tick,
      unit(0.0, "npc") + tick
    )

    args <- list(x = tick, y = mark)
    if (position %in% c("top", "bottom")) {
      args <- flip_names(args)
    }
  }
  id <- rep(2L, nrow(decor))

  props <- element_key_properties(decor, "line")

  inject(element_grob(element, id.lengths = id, !!!args, !!!props))
}
