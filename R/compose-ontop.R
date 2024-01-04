# Constructor -------------------------------------------------------------

#' Compose guides on top of one another
#'
#' This guide can place place other guides on top of one another.
#'
#' @inheritParams compose_stack
#'
#' @return A `<ComposeOntop>` composite guide object.
#' @export
#' @family composition
#'
#' @examples
#' # Using the ontop composition to get two types of ticks with different
#' # lengths
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point() +
#'   guides(x = compose_ontop(
#'     guide_axis_custom(
#'       key_manual(c(2, 4, 6)),
#'       theme = theme(
#'         axis.ticks = element_line(colour = "limegreen"),
#'         axis.ticks.length = unit(11, "pt")
#'       )
#'     ),
#'     guide_axis_custom(
#'       key_manual(c(3, 5, 7)),
#'       theme = theme(
#'         axis.ticks = element_line(colour = "tomato"),
#'         axis.ticks.length = unit(5.5, "pt")
#'       )
#'     )
#'   ))
compose_ontop <- function(
  ..., args = list(),
  key = NULL, title = waiver(),
  angle = waiver(), theme = NULL, order = 0,
  position = waiver(), available_aes = NULL
) {
  new_compose(
    guides = list2(...),
    title = title,
    theme = theme,
    key = key,
    angle = angle,
    available_aes = available_aes,
    order = order,
    position = position,
    name = "ontop_composition",
    super = ComposeOntop
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
ComposeOntop <- ggproto(
  "ComposeOntop", Compose,

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    theme <- theme + params$theme

    position  <- params$position  %||% position
    direction <- params$direction %||% direction

    n_guides <- length(params$guides)
    guide_index <- seq_len(n_guides)
    grobs <- vector("list", n_guides)

    if (position %in% c("theta", "theta.sec")) {
      stack_offset <- unit(cm(params$stack_offset %||% 0), "cm")
      offset <- stack_offset

      for (i in guide_index) {
        pars <- params$guide_params[[i]]
        pars$stack_offset <- stack_offset
        grob <- params$guides[[i]]$draw(
          theme = theme, position = position, direction = direction,
          params = pars
        )
        if (!is.null(grob$offset) && !is.zero(grob)) {
          offset <- unit(cm(max(grob$offset, offset)), "cm")
        }
        grobs[[i]] <- grob
      }
      keep <- !vapply(grobs, is.zero, logical(1))
      if (!any(keep)) {
        return(zeroGrob())
      }
      offset <- offset - stack_offset
      grobs <- gTree(offset = offset, children = inject(gList(!!!grobs[keep])))
      return(grobs)
    }

    draw_label <- params$draw_label %||% TRUE

    for (i in guide_index) {
      pars <- params$guide_params[[i]]
      pars$draw_label <- draw_label
      grobs[[i]] <- params$guides[[i]]$draw(
        theme = theme, position = position, direction = direction, params = pars
      )
    }

    keep <- !vapply(grobs, is.zero, logical(1))
    grobs <- grobs[keep]
    if (length(grobs) == 0) {
      return(zeroGrob())
    }

    origin <- unit(as.numeric(position %in% c("left", "bottom")), "npc")
    just   <- opposite_position(position)
    along  <- seq_along(grobs)
    widths <- width_cm(grobs)
    heights <- height_cm(grobs)
    names <- paste0("guide-ontop-", along)

    if (position %in% c("bottom", "top")) {
      height <- unit(max(heights), "cm")
      gt <- gtable(widths = unit(1, "npc"), heights = height)
      gt <- gtable_add_grob(gt, grobs, t = 1, l = 1, name = names, clip = "off")
      vp <- viewport(y = origin, height = height, just = just)
    } else {
      width <- unit(max(widths), "cm")
      gt <- gtable(widths = width, heights = unit(1, "npc"))
      gt <- gtable_add_grob(gt, grobs, t = 1, l = 1, name = names, clip = "off")
      vp <- viewport(x = origin, width = width, just = just)
    }
    absoluteGrob(
      grob = gList(gt), vp = vp,
      width = gtable_width(gt), height = gtable_height(gt)
    )
  }
)
