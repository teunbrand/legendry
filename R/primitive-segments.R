# Constructor -------------------------------------------------------------

primitive_segments <- function(key = NULL, space = rel(10), vanish = FALSE,
                               theme = NULL, position = waiver()) {
  check_unit(space, allow_rel = TRUE)
  new_guide(
    key = key,
    space = space,
    vanish = vanish,
    theme = theme,
    position = position,
    available_aes = c("any", "x", "y", "r", "theta"),
    super = PrimitiveSegments
  )
}

# Class -------------------------------------------------------------------

PrimitiveSegments <- ggproto(
  "PrimitiveSegments", Guide,

  params = new_params(key = NULL, space = rel(10), vanish = FALSE),

  hashables = exprs(key$.value),

  extract_key = function(scale, aesthetic, key, ...) {
    key <- standard_extract_key(scale, aesthetic, key, ...)
    remove <- character()
    if (all(c("value", "value_end") %in% names(key))) {
      value <- vec_interleave(key$value, key$value_end)
      remove <- c(remove, c("value", "value_end"))
    }
    if (all(c("oppo", "oppo_end") %in% names(key))) {
      oppo <- vec_interleave(key$oppo, key$oppo_end)
      remove <- c(remove, c("oppo", "oppo_end"))
    }
    key[remove] <- NULL
    new <- data_frame0(value = value, oppo = oppo)
    i <- rep(vec_seq_along(key), each = 2)
    new[names(key)] <- key[i, , drop = FALSE]
    new$group <- new$group %||% i
    new$oppo <- rescale(new$oppo, from = range(new$oppo, 0))
    if (aesthetic == "x") {
      new <- rename(new, c("value", "oppo"), c("x", "y"))
    } else if (aesthetic == "y") {
      new <- rename(new, c("value", "oppo"), c("y", "x"))
    } else {
      new <- rename(new, "value", aesthetic)
    }
    new
  },

  elements = list(
    position = list(line = "axis.ticks",   size = "axis.ticks.length"),
    legend   = list(line = "legend.ticks", size = "legend.ticks.length")
  ),

  transform = function(self, params, coord, panel_params) {
    key <- params$key
    position <- params$position
    aesthetic <- params$aesthetic
    mult <- 10

    opposite <- setdiff(c("x", "y"), aesthetic)
    is_radius <- "theta.range" %in% names(panel_params) & !is_theta(position)
    if (is_radius) {
      range <- panel_params$r.range
      value <- squish_infinite(key[[aesthetic]], range)
      value <- rescale(value, panel_params$inner_radius, range)
      value <- rescale(value + 0.5, from = panel_params$bbox$x)
      key[[aesthetic]] <- value
      if (position == "left") {
        key[[opposite]] <- 1 - key[[opposite]]
      }
      params$key <- key
      return(params)
    }

    range <- switch(
      position,
      top = , bottom = "y.range",
      left = , right = "x.range",
      theta = , theta.sec = "r.range"
    )
    range <- panel_params[[range]]

    margin_lower <- function(value) range[1] - value * diff(range) / mult
    margin_upper <- function(value) range[2] + value * diff(range) / mult

    key[[opposite]] <- switch(
      position,
      bottom = , theta.sec = , left = margin_lower(key[[opposite]]),
      top = , theta = , right = margin_upper(key[[opposite]]),
      key[[opposite]]
    )

    key <- coord_munch(coord, key, panel_params)

    key[[opposite]] <- switch(
      position,
      left = , bottom = key[[opposite]] * mult + 1,
      top = , right   = key[[opposite]] * mult - mult,
      key[[opposite]]
    )

    if (!is_theta(position)) {
      return(vec_assign(params, "key", key))
    }

    radius <- panel_params$inner_radius
    if (position == "theta") {
      key$adjust <- (key$r - radius[2]) * (mult / radius[2])
      key$r <- radius[2]
    } else {
      key$adjust <- (key$r - radius[1]) / diff(radius) * -mult
      key$r <- radius[1]
    }
    bbox <- panel_params$bbox
    key$x <- rescale(key$r * sin(key$theta) + 0.5, from = bbox$x)
    key$y <- rescale(key$r * cos(key$theta) + 0.5, from = bbox$y)
    params$center <- c(rescale(0.5, from = bbox$x), rescale(0.5, from = bbox$y))

    params$key <- key
    params
  },

  setup_params = primitive_setup_params,
  setup_elements = primitive_setup_elements,
  override_elements = function(params, elements, theme) {
    size <- params$space
    if (is.rel(size)) {
      size <- unclass(size) * elements$size
    }
    elements$size <- convertUnit(size, "cm", valueOnly = TRUE)
    elements
  },

  build_ticks = function(key, elements, params, position = params$position) {
    if (is_empty(key)) {
      return(zeroGrob())
    }

    x <- unit(key$x, "npc")
    y <- unit(key$y, "npc")

    if (is_theta(position)) {
      vanish <- position == "theta.sec" && isTRUE(params$vanish)
      theta <- key$theta + as.numeric(position == "theta.sec") * pi
      offset <- elements$offset

      if (!vanish) {
        offset <- key$adjust * elements$size + offset
      }
      if (any(offset != 0)) {
        x <- x + unit(sin(theta) * offset, "cm")
        y <- y + unit(cos(theta) * offset, "cm")
      }
      if (vanish) {
        cx <- unit(params$center[1] * key$adjust, "npc")
        cy <- unit(params$center[2] * key$adjust, "npc")
        x <- (x * (1 - key$adjust)) + cx
        y <- (y * (1 - key$adjust)) + cy
      }
    }

    element_grob(
      elements$line, x = x, y = y,
      id.lengths = vec_run_sizes(key$group)
    )
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    params <- replace_null(params, position = position, direction = direction)
    params <- self$setup_params(params)

    elems <- self$setup_elements(params, self$elements, theme)
    elems <- self$override_elements(params, elems, theme)
    segments <- self$build_ticks(params$key, elems, params)

    primitive_grob(
      grob = segments,
      size = unit(elems$size, "cm"),
      position = params$position,
      name = "segments"
    )
  }
)
