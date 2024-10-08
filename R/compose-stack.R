# Constructor -------------------------------------------------------------

#' Compose guides as stack
#'
#' This guide can stack other guides.
#'
#' @param ... Guides to stack in [composition][guide-composition]. Each guide
#'   can be specified as one of the following:
#'   * A `<Guide>` class object.
#'   * A `<function>` that returns a `<Guide>` class object.
#'   * A `<character[1]>` naming such a function, without the `guide_` or
#'   `primitive_` prefix.
#' @param key A [standard key][key_standard] specification. The key is shared
#'   among all guides that have `NULL` keys themselves. See more information
#'   in the linked topic.
#' @inheritParams guide-composition
#' @param side.titles A `<character>` giving labels for titles displayed on the
#'   side of the stack. Set to `NULL` to display no side titles. If `waiver()`,
#'   an attempt is made to extract the titles from the guides and use these
#'   as side titles.
#' @param drop An `<integer>` giving the indices of guides that should be
#'   dropped when a facet requests no labels to be drawn at axes in between
#'   panels. The default, `NULL`, will drop every guide except the first.
#' @inheritParams common_parameters
#'
#' @return A `<ComposeStack>` guide object.
#' @export
#' @family composition
#'
#' @examples
#' ggplot() +
#'   geom_function(fun = dnorm, xlim = c(-3, 3)) +
#'   guides(x = compose_stack(
#'     "axis", "axis",
#'     side.titles = c("first", "second")
#'   )) +
#'   # Add margin to make room for side titles
#'   theme(plot.margin = margin(5.5, 5.5, 5.5, 11))
compose_stack <- function(
  ..., args = list(),
  key = NULL, title = waiver(), side.titles = waiver(),
  angle = waiver(), theme = NULL, order = 0, drop = NULL,
  position = waiver(), available_aes = NULL
) {
  new_compose(
    guides = list2(...),
    title = title,
    theme = theme,
    key = key,
    angle = angle,
    side_titles = side.titles,
    drop = drop,
    available_aes = available_aes,
    order = order,
    position = position,
    name = "stack_composition",
    super = ComposeStack
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
ComposeStack <- ggproto(
  "ComposeStack", Compose,

  params = new_params(
    guides = list(), guide_params = list(),
    key = NULL, side_titles = NULL, angle = waiver(), drop = NULL
  ),

  elements = list(
    side_titles   = "gguidance.axis.subtitle",
    side_position = "gguidance.axis.subtitle.position",
    spacing       = "gguidance.guide.spacing"
  ),

  train = function(self, params = self$params, scale, aesthetic = NULL, ...) {
    params$side_titles <- get_side_titles(
      params$side_titles, params$guide_params, call = expr(compose_stack())
    )
    Compose$train(params = params, scale = scale, aesthetic = aesthetic, ...)
  },

  transform = function(self, params, coord, panel_params) {
    params <- Compose$transform(params, coord, panel_params)
    if (!is.null(params$side_titles)) {
      params$sides <- get_sides(coord, panel_params)
    }
    params
  },

  override_elements = function(params, elements, theme) {
    if (!is_theta(params$position)) {
      elements$spacing <- cm(elements$spacing)
    }
    if (!is.null(params$side_titles)) {
      elements$side_position <- switch(
        params$position,
        top = , bottom = , theta = , theta.sec =
          setdiff(elements$side_position, c("top", "bottom")),
        left = , right = setdiff(elements$side_position, c("left", "right"))
      )
      check_argmatch(elements$side_position, .trbl)
      i <- match(opposite_position(elements$side_position), .trbl)
      elements$side_titles$margin[-i] <- unit(0, "pt")
    }
    elements
  },

  build_title = function(label, elements, params) {
    if (length(label) < 1) {
      return(NULL)
    }

    side  <- elements$side_position
    sides <- vec_slice(params$sides, params$sides$position == params$position)
    sides <- vec_slice(sides, sides$side == side)

    x <- sides$x
    y <- sides$y

    element <- elements$side_titles
    hjust <- switch(side, left = 1, right = 0, elements$hjust)
    vjust <- switch(side, top = 0, bottom = 1, elements$vjust)
    lapply(label, function(lab) {
      element_grob(
        element, lab, x = x, y = y,
        margin_x = TRUE, margin_y = TRUE,
        hjust = hjust, vjust = vjust
      )
    })
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    n_guides <- length(params$guides)
    if (n_guides < 1) {
      return(zeroGrob())
    }

    theme <- theme + params$theme
    position <- params$position <- params$position  %||% position
    check_position(position)
    params$guide_params <-
      set_list_element(params$guide_params, "position", position)
    direction <- params$direction <-  params$direction %||% direction

    elems <- self$setup_elements(params, self$elements, theme)
    elems <- self$override_elements(params, elems, theme)

    guide_index <- seq_len(n_guides)
    grobs  <- vector("list", n_guides)
    draw_label <- !isFALSE(params$draw_label %||% TRUE)

    keep <- rep(TRUE, n_guides)
    if (!draw_label && length(params$drop) > 0) {
      drop <- intersect(params$drop %||% guide_index[-1], guide_index)
      keep[drop] <- FALSE
    }

    if (is_theta(position)) {
      stack_offset <- unit(cm(params$stack_offset %||% 0), "cm")
      offset <- stack_offset
      offset_ranges <- vector("list", n_guides)
      guide_index <- guide_index[keep]

      for (i in guide_index) {
        pars <- params$guide_params[[i]]
        pars$draw_label <- params$draw_label
        pars$stack_offset <- offset
        grob <- params$guides[[i]]$draw(
          theme = theme, position = position, direction = direction,
          params = pars
        )
        if (!is.null(grob$offset) && !is.zero(grob)) {
          offset_ranges[[i]] <- unit.c(offset, grob$offset)
          offset <- unit(cm(offset + elems$spacing + grob$offset), "cm")
        }
        grobs[[i]] <- grob
      }
      keep <- !is_each(grobs, is.zero)
      if (!any(keep)) {
        return(zeroGrob())
      }
      offset <- offset - stack_offset
      grobs <- grobs[keep]
      side_titles <-
        theta_side_titles(params$side_titles, elems, params, offset_ranges)
      grobs <- inject(gList(!!!grobs, side_titles))
      grob  <- gTree(offset = offset, children = grobs)
      return(grob)
    }

    side_titles <- self$build_title(params$side_titles, elems, params)

    for (i in guide_index) {
      pars <- params$guide_params[[i]]
      pars$draw_label <- draw_label
      grobs[[i]] <- params$guides[[i]]$draw(
        theme = theme, position = position, direction = direction, params = pars
      )
    }

    keep <- keep & !is_each(grobs, is.zero)
    grobs <- grobs[keep]
    if (length(grobs) == 0) {
      return(zeroGrob())
    }
    side_titles <- side_titles[keep]

    origin  <- unit(as.numeric(position %in% c("left", "bottom")), "npc")
    just    <- opposite_position(position)
    along   <- seq_along(grobs)
    widths  <- width_cm(grobs)
    heights <- height_cm(grobs)

    if (position %in% c("top", "left")) {
      along   <- rev(along)
      widths  <- rev(widths)
      heights <- rev(heights)
    }

    if (position %in% c("bottom", "top")) {
      gt <- gtable(widths = unit(1, "npc"), heights = unit(heights, "cm"))
      gt <- gtable_add_grob(gt, grobs, t = along, l = 1, name = "stack", clip = "off")
      gt <- add_side_titles(gt, side_titles, params$position)
      gt <- gtable_add_row_space(gt, height = unit(elems$spacing, "cm"))
      vp <- viewport(y = origin, height = grobHeight(gt), just = just)
    } else {
      gt <- gtable(widths = unit(widths, "cm"), heights = unit(1, "npc"))
      gt <- gtable_add_grob(gt, grobs, t = 1, l = along, name = "stack", clip = "off")
      gt <- add_side_titles(gt, side_titles, params$position)
      gt <- gtable_add_col_space(gt, width = unit(elems$spacing, "cm"))
      vp <- viewport(x = origin, width = grobWidth(gt), just = just)
    }

    absoluteGrob(
      grob = gList(gt), vp = vp,
      width = gtable_width(gt), height = gtable_height(gt)
    )
  }
)


# Helpers -----------------------------------------------------------------

get_side_titles <- function(side_titles, params, call = caller_env()) {

  if (is_waive(side_titles)) {
    side_titles <- lapply(params, function(p) (p$title %|W|% NULL) %||% NA)
    side_titles <- unlist(lapply(side_titles, `[[`, i = 1L), recursive = FALSE)
    if (all(is.na(side_titles))) {
      side_titles <- names(params)
    }
  }
  if (is.null(side_titles)) {
    return(NULL)
  }
  if (!is_waive(side_titles) && length(side_titles) != length(params)) {
    cli::cli_abort(c(
      "Must have a number of {.arg side.titles} equal to the number of guides.",
      i = "There are {length(side_titles)} side titles.",
      i = "There are {length(params)} guides."
    ), call = call)
  }
  side_titles
}

add_side_titles <- function(gt, titles, position) {
  if (is.null(titles)) {
    return(gt)
  }
  along <- seq_along(titles)
  t <- switch(position, left = , right = 1, top  = rev(along), bottom = along)
  l <- switch(position, top = , bottom = 1, left = rev(along), right  = along)
  gtable_add_grob(gt, titles, t = t, l = l, clip = "off", name = "side titles")
}

theta_side_titles <- function(label, elements, params, ranges) {
  if (length(label) < 1) {
    return(zeroGrob())
  }

  ranges <- lapply(lapply(ranges, cm), cumsum)
  min <- map_dbl(ranges, min)
  max <- map_dbl(ranges, max)

  element <- elements$side_titles
  just <- justify_range(min, max, element$vjust)

  side <- elements$side_position
  sides <- vec_slice(params$sides, params$sides$side == side)
  position <- params$position

  origin_index <- switch(position, theta = which.max, which.min)(sides$r)
  origin <- vec_slice(sides, origin_index)

  rad <- origin$theta

  if (position == "theta") {
    dir <- switch(side, left = 1, right = -1)
  } else {
    rad <- rad + pi
    dir <- switch(side, left = -1, right = 1)
  }

  hjust <- switch(side, left = 1, right = 0, elements$hjust)
  margin <- max(cm(element$margin)) * dir

  xoffset <- sin(rad) * just - cos(rad) * margin
  yoffset <- cos(rad) * just + sin(rad) * margin

  x <- unit(origin$x, "npc") + unit(xoffset, "cm")
  y <- unit(origin$y, "npc") + unit(yoffset, "cm")

  angle <- -rad2deg(origin$theta) %% 360
  flip  <- angle > 90 & angle < 270
  if (flip) {
    angle <- angle + 180
    hjust <- 1 - hjust
  }

  element_grob(
    element, label, x = x, y = y,
    hjust = hjust,
    angle = angle
  )
}

get_sides <- function(coord, panel_params) {

  x <- panel_params$x.range %||%
    switch(coord$theta, x = panel_params$theta.range, panel_params$r.range)
  y <- panel_params$y.range %||%
    switch(coord$theta, y = panel_params$theta.range, panel_params$r.range)

  df <- data_frame0(
    position = rep(.trbl, each = 2),
    side     = rep(.trbl[c(4, 2, 1, 3)], 2),
    x = x[c(1, 2, 2, 2, 1, 2, 1, 1)],
    y = y[c(2, 2, 2, 1, 1, 1, 2, 1)],
    group = 1:8
  )
  coord_munch(coord, df, panel_params)
}
