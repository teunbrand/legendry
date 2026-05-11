# Constructor -------------------------------------------------------------

#' Annotation axis guide
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @description
#' This axis guide acts as annotation: it draws labels at specified places.
#' It also wraps an inner guide, making the behaviour look like one is 'adding'
#' the annotation on top of the regular guide.
#'
#' @param aesthetic
#' A vector of values for the guide to represent.
#' @param label
#' A `<character[n]>` or list of expressions to use as labels.
#' @param ...
#' Additional graphical properties parallel to `aesthetic`. Can be
#' `text_colour`, `size`, `face`, `hjust`, `vjust`, `angle` or `lineheight` for
#' the labels. Can be `line_colour`, `linewidth` and `linetype` for ticks.
#' Setting `colour` will also set `text_colour` and `line_colour`.
#' For `annotate_top()`, `annotate_right()`, `annotate_bottom()` and
#' `annotate_left()`, arguments are passed on to `guide_axis_annotation()`.
#' @param arrow
#' A [`grid::arrow()`] specification. Can be `NULL` (default) to draw no arrow.
#' @param inner
#' A guide that supports the aesthetic to draw the annotation across.
#' When `waiver()` (default), populates a `guide_axis_base()` except in the
#' `"top"`, `"right"` and `"theta.sec"` positions.
#' @param key
#' A [standard key][key_standard] overriding the `aesthetic`, `label` and `...`
#' arguments.
#' @param theme
#' A [`<theme>`][ggplot2::theme] object to style the *annotation* part of this
#' guide differently from the plot's theme settings. The `theme` argument
#' in this guide overrides and is combined with the plot's theme.
#' @inheritParams common_parameters
#'
#' @details
#' Under the hood, this guide is a hybrid composition guide. The `theme()`
#' options that govern the styling are partially determined by its consituents.
#' They are linked below so you can find their 'Styling options' sections.
#'
#' | **Constituent** | **Description** |
#' | `compose_ontop()` | Composes the annotation on top of the `inner` guide |
#' | `guide_axis_base()` | The default `inner` guide |
#' | `primitive_ticks()` | Display of the annotation ticks |
#' | `primitive_labels()` | Display of the annotation labels |
#'
#' Styling options *per annotation* can be set via `...` as described above.
#'
#' The context-agnostic alternative to using `theme()` is to use
#' [`theme_guide()`]:
#'
#' ```r
#' # Note that `theme` is used *only* for the annotation
#' guide_axis_annotation(theme = theme_guide(
#'   text = element_text(),
#'   ticks = element_line(),
#'   ticks.length = unit(5, "mm")
#' ))
#' ```
#'
#' @returns A `<Guide>` object
#' @export
#' @family standalone guides
#'
#' @examples
#' # Basic plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # Typical use
#' p +
#'   annotate_top(5, face = "bold") +
#'   annotate_bottom(c(2.5, 4.5), c("Bottom annotation", "Second label"))
#'
#' # If you want to combine them with secondary axes, you must
#' # set the `inner` argument manually.
#' p +
#'   scale_y_continuous(
#'     sec.axis = dup_axis(breaks = c(15, 25, 35))
#'   ) +
#'   annotate_right(30, inner = "axis")
#'
#' # Use in theta axis
#' p + coord_radial() +
#'   guides(theta = guide_axis_annotation(4.5, "Theta annotation"))
#'
#' # Specialised use as part of other guides
#' # Note that `guide_colbar` imposes white inward ticks
#' # We can overrule these impositions with a replacement theme
#' p + aes(colour = cty) +
#'   guides(colour = guide_colbar(
#'     second_guide = guide_axis_annotation(22, "This", theme = theme_gray())
#'   ))
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

#' @rdname guide_axis_annotation
#' @export
annotate_top <- function(..., position = "top") {
  guides(x.sec = guide_axis_annotation(
    ..., position = position, call = current_call()
  ))
}

#' @rdname guide_axis_annotation
#' @export
annotate_right <- function(..., position = "right") {
  guides(y.sec = guide_axis_annotation(
    ..., position = position, call = current_call()
  ))
}

#' @rdname guide_axis_annotation
#' @export
annotate_bottom <- function(..., position = "bottom") {
  guides(x = guide_axis_annotation(
    ..., position = position, call = current_call()
  ))
}

#' @rdname guide_axis_annotation
#' @export
annotate_left <- function(..., position = "left") {
  guides(y = guide_axis_annotation(
    ..., position = position, call = current_call()
  ))
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
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
