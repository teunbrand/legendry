# Constructor -------------------------------------------------------------

#' Compose guides in a cross
#'
#' This guide composition has a central guide optionally surrounded by other
#' guides on all four sides.
#'
#' @param key A [standard key][key_standard] specification. The key is shared
#'   among all guides that have `NULL` keys themselves. See more information
#'   in the linked topic.
#' @param centre,left,right,top,bottom Guides to use in
#'   [composition][guide-composition] per position. Each guide can be
#'   specified as one of the following:
#'   * A `<Guide>` class object.
#'   * A `<function>` that returns a `<Guide>` class object.
#'   * A `<character>` naming such a function, without the `guide_` or
#'   `primitive_` prefix.
#' @param args A `<list>` of arguments to pass to guides that are given either
#'   as a function or as a string.
#' @param complete A `<logical[1]>` whether to treat the composition as a
#'   complete guide. If `TRUE`, a title and margin are added to the result.
#'   If `FALSE` (default), no titles and margins are added.
#' @param theme A [`<theme>`][ggplot2::theme] object to style the guide
#'   individually of differently from the plot's theme settings. The `theme`
#'   arguments in the guide overrides, and is combined with, the plot's theme.
#' @param position Where this guide should be drawn: one of `"top"`, `"bottom"`,
#'   `"left"`, or `"right"`.
#' @inheritParams common_parameters
#'
#' @return Aa `<ComposeCrux>` guide object.
#' @export
#' @family composition
#'
#' @examples
#' NULL
compose_crux <- function(
  key = NULL,
  centre = "none",
  left = "none",
  right = "none",
  top = "none",
  bottom = "none",
  args = list(),
  complete = FALSE,
  theme = NULL,
  order = 0,
  title = waiver(),
  position = waiver(),
  available_aes = NULL
) {
  check_bool(complete)
  new_compose(
    guides = list(
      centre = centre,
      left   = left,
      right  = right,
      top    = top,
      bottom = bottom
    ),
    complete = complete,
    args  = args,
    title = title,
    key   = key,
    available_aes = available_aes,
    order = order,
    position = position,
    name = "crux_composition",
    super = ComposeCrux
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
ComposeCrux <- ggproto(
  "ComposeCrux", Compose,

  params = c(Compose$params, complete = FALSE),

  elements = list(
    title_position = "legend.title.position",
    text_position = "legend.text.position",
    title = "legend.title",
    margin = "legend.margin"
  ),

  setup_elements = function(params, elements, theme) {
    elements$title <- setup_legend_title(theme, params$direction)
    Guide$setup_elements(params, elements, theme)
  },

  override_elements = function(params, elements, theme) {
    elements$title_position <- elements$title_position %||%
      switch(params$direction, horizontal = "left", vertical = "top")
    elements$text_position <- elements$text_position %||%
      switch(params$direction, horizontal = "bottom", vertical = "right")

    check_position(elements$title_position, .trbl, arg = "legend.title.position")
    elements
  },

  draw = function(
    self, theme, position = NULL,
    direction = NULL, params = self$params
  ) {
    n_guides <- length(params$guides)
    if (n_guides < 1) {
      return(zeroGrob())
    }

    position  <- params$position  <- params$position  %||% position
    direction <- params$direction <- params$direction %||% direction
    check_position(position, .trbl)
    check_argmatch(direction, c("horizontal", "vertical"))

    elems <- self$setup_elements(params, self$elements, theme)
    elems <- self$override_elements(params, elems, theme)

    directions <- c(
      top = "horizontal", bottom = "horizontal",
      left = "vertical", right = "vertical", centre = direction
    )
    text_positions <- c(
      top = "top", bottom = "bottom", left = "left", right = "right",
      centre = elems$text_position
    )

    grobs <- vector("list", n_guides)
    names(grobs) <- names(params$guides)

    for (i in names(params$guides)) {
      pars <- params$guide_params[[i]]
      pars$position  <- switch(i, centre = position, i)
      pars$direction <- directions[i]

      grobs[[i]] <- params$guides[[i]]$draw(
        theme = theme + theme(legend.text.position = text_positions[i]),
        position = NULL, direction = NULL,
        params = pars
      )
    }

    gt <- grobs$centre
    valign <- gt$align$vertical   %||% c(1, -1)
    halign <- gt$align$horizontal %||% c(1, -1)

    if (!is.zero(grobs$top)) {
      gt <- gtable_add_rows(gt, grobs$top$height, 0)
      gt <- gtable_add_grob(
        gt, grobs$top, t = 1, l = halign[1], r = halign[2],
        clip = "off", name = "top-guide"
      )
      valign[1] <- valign[1] + 1
    }
    if (!is.zero(grobs$bottom)) {
      gt <- gtable_add_rows(gt, grobs$bottom$height, pos = -1)
      gt <- gtable_add_grob(
        gt, grobs$bottom, t = -1, l = halign[1], r = halign[2],
        clip = "off", name = "bottom-guide"
      )
      valign[2] <- valign[2] - 1
    }
    if (!is.zero(grobs$left)) {
      gt <- gtable_add_cols(gt, grobs$left$width, pos = 0)
      gt <- gtable_add_grob(
        gt, grobs$left, t = valign[1], b = valign[2], l = 1,
        clip = "off", name = "left-guide"
      )
      # halign[1] <- halign[1] + 1
    }
    if (!is.zero(grobs$right)) {
      gt <- gtable_add_cols(gt, grobs$right$width, pos = -1)
      gt <- gtable_add_grob(
        gt, grobs$right, t = valign[1], b = valign[2], l = -1,
        clip = "off", name = "right-guide"
      )
    }
    if (params$complete) {
      title <- self$build_title(params$title, elems, params)
      gt <- add_legend_title(gt, title, elems$title_position, elems$title)
      gt <- gtable_add_padding(gt, elems$margin)
    }
    gt
  }
)
