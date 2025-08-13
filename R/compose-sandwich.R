#' Compose guides as a sandwich
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This guide composition has a middle guide flanked by two parallel guides.
#'
#' @param key A [standard key][key_standard] specification. The key is shared
#'   among all guides that have `NULL` keys themselves. See more information
#'   in the linked topic.
#' @param middle Guide to use as the middle guide. Each guide can be specified
#'   as one of the following:
#'   * A `<Guide>` class object.
#'   * A `<function>` that returns a `<Guide>` class object.
#'   * A `<character>` naming such a function, without the `guide_` or
#'   `primitive_` prefix.
#' @param text,opposite Guides to use at the `legend.text.position` location
#'   and on the opposite side of the `middle` guide respectively. Guide
#'   specification is the same as in the `middle` argument.
#' @inheritParams compose_crux
#'
#' @return A `<ComposeSandwich>` guide object.
#' @export
#' @family composition
#'
#' @details
#' The sandwich composition is effectively the same as a
#' [crux composition][compose_crux] lacking two opposing arms.
#'
#' @examples
#' # A standard plot with a sandwich guide
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = cty)) +
#'   guides(colour = compose_sandwich(
#'     middle = "colourbar",
#'     text = "axis_base",
#'     opposite = primitive_bracket(key = key_range_manual(
#'       start = c(10, 20), end = c(25, 30), name = c("A", "B")
#'     ))
#'   ))
compose_sandwich <- function(
  key = key_auto(),
  middle = gizmo_barcap(),
  text = "none",
  opposite = "none",
  args = list(),
  suppress_labels = "opposite",
  complete = TRUE,
  theme = NULL,
  theme_defaults = list(),
  reverse = FALSE,
  order = 0,
  title = waiver(),
  position = waiver(),
  available_aes = NULL
) {
  new_compose(
    guides = list(
      middle = middle,
      text = text,
      opposite = opposite
    ),
    title = title,
    key = key,
    args = args,
    complete = complete,
    reverse = reverse,
    suppress_labels = suppress_labels,
    available_aes = available_aes,
    order = order,
    theme = theme,
    theme_defaults = theme_defaults,
    position = position,
    name = "sandwich_composition",
    super = ComposeSandwich
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
ComposeSandwich <- ggproto(
  "ComposeSandwich", Compose,

  params = c(Compose$params, list(complete = FALSE, theme_defaults = list(),
                                  reverse = FALSE, suppress_labels = "opposite")),

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    direction <- params$direction %||% direction
    text_position <- calc_element("legend.text.position", theme) %||%
      switch(direction, horizontal = "bottom", vertical = "right")

    valid_position <- switch(
      direction, horizontal = c("bottom", "top"), vertical = c("left", "right")
    )
    check_position(text_position, valid_position, arg = "legend.text.position")

    theme <- theme + params$theme
    theme <- apply_theme_defaults(theme, params$theme_defaults)

    opposite <- opposite_position(text_position)
    if ("opposite" %in% params$suppress_labels) {
      params$guide_params$opposite$draw_label <- FALSE
    }
    if ("text" %in% params$suppress_labels) {
      params$guide_params$text$draw_label <- FALSE
    }

    old <- c("middle", "text", "opposite")
    new <- c("centre", text_position, opposite)
    params$guides <- rename(params$guides, old, new)
    params$guide_params <- rename(params$guide_params, old, new)

    ComposeCrux$draw(theme, position, direction, params)
  }
)
