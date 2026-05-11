# Constructor -------------------------------------------------------------

guide_axis_annotation <- function(
  aesthetic,
  label = as.character(aesthetic),
  ...,
  key   = NULL,
  arrow = NULL,
  inner = waiver(),
  title = waiver(),
  theme = NULL,
  order = 0L,
  position = waiver(),
  call = NULL
) {
  call <- call %||% current_call()
  key <- key %||% key_manual(aesthetic, label = label, ..., call = call)

  if (!is.null(arrow)) {
    arrow <- theme_guide(ticks = element_line(arrow = arrow))
  }
  if (is_waiver(inner)) {
    inner <- guide_axis_base()
    inner$params$implicit <- TRUE
  }

  guides <- list(
    inner = inner %||% guide_none(),
    annotation = compose_stack(
      ticks  = primitive_ticks(theme = arrow),
      labels = primitive_labels(),
      key = key, title = NULL, theme = theme,
      position = position, side.titles = NULL
    )
  )

  new_compose(
    guides,
    key = "auto",
    title = title,
    theme = theme,
    order = order,
    available_aes = c("any", "x", "y", "r", "theta"),
    position = position,
    super = GuideAxisAnnotation,
    call = call
  )
}



annotate_top <- function(..., position = "top") {
  guides(x.sec = guide_axis_annotation(
    ..., position = position, call = current_call()
  ))
}

annotate_right <- function(..., position = "right") {
  guides(y.sec = guide_axis_annotation(
    ..., position = position, call = current_call()
  ))
}

annotate_bottom <- function(..., position = "bottom") {
  guides(x = guide_axis_annotation(
    ..., position = position, call = current_call()
  ))
}

annotate_left <- function(..., position = "left") {
  guides(y = guide_axis_annotation(
    ..., position = left, call = current_call()
  ))
}

# Class -------------------------------------------------------------------

GuideAxisAnnotation <- ggproto(
  "GuideAxisAnnotation",
  ComposeOntop,

  train = function(self, params, scale, aesthetic = NULL, ...) {
    aesthetic <- aesthetic %||% scale$aesthetics[1]
    if (isTRUE(aesthetic %in% c("x", "y")) &&
        isTRUE(params$position %in% c("top", "right", "theta.sec")) &&
        isTRUE(params$guide_params$inner$implicit)) {
      params$guides$inner <- guide_none()
      params$guide_params$inner <- params$guides$inner$params
    }
    ggproto_parent(ComposeOntop, self)$train(params, scale, aesthetic, ...)
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    params$postion   <- position  <- params$position  %||% position
    params$direction <- direction <- params$direction %||% direction
    grobs <- build_annotate(params, theme, position, direction)
    if (is_theta(position)) {
      return(grobs)
    }
    self$assemble_drawing(grobs$children, params = params)
  }
)

build_annotate <- function(params, theme, position, direction) {
  stack_offset <- unit(cm(params$stack_offset %||% 0.0), "cm")
  offset <- stack_offset

  inner_guide  <- params$guides$inner
  if (inherits(inner_guide, "GuideNone")) {
    inner_grob <- zeroGrob()
  } else {
    inner_params <- params$guide_params$inner
    inner_params$draw_label <- params$draw_label
    inner_params$stack_offset <- stack_offset
    inner_grob <- inner_guide$draw(
      theme = theme, position = position, direction = direction,
      params = inner_params
    )
  }

  if (!is_zero(inner_grob)) {
    # We need to grab the size of the inner grob. The default `compose_ontop()`
    # only grabs this for theta axes.
    offset <- switch(
      position,
      theta = , theta.sec = inner_grob$offset,
      top = , bottom      = grobHeight(inner_grob),
      left = , right      = grobWidth(inner_grob),
      NULL
    ) %||% offset
  }

  anno_guide <- params$guides$annotation
  if (inherits(anno_guide, "GuideNone")) {
    anno_grob <- zeroGrob()
  } else {
    anno_params <- params$guide_params$annotation
    anno_params$stack_offset <- stack_offset
    if (!is.null(anno_params$guide_params$ticks)) {
      anno_params$guide_params$ticks$force_stretch <- offset
    }
    anno_grob <- anno_guide$draw(
      theme = theme, position = position, direction = direction,
      params = anno_params
    )
  }
  if (!is_zero(anno_grob) && !is.null(anno_grob$offset)) {
    offset <- unit(cm(max(anno_grob$offset, offset)), "cm")
  }
  gTree(
    offset = offset - stack_offset,
    children = gList(inner_grob, anno_grob)
  )
}
