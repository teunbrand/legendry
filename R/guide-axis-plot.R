# Constructor -------------------------------------------------------------

#' Side-plot axis
#'
#' Displays an axis-sharing plot to the side of the panel.
#'
#' @param plot
#' A `<ggplot>` object, subject to limitations listed in the 'Details' section.
#'
#' @param title
#' One of the following to indicate the title of the guide:
#' * A `<character[1]>` or `<expression[1]>` to set a custom title.
#' * `NULL` (default) to not display any title.
#' * [`waiver()`][ggplot2::waiver()] to take the name of the scale
#'   object or the name specified in [`labs()`][ggplot2::labs] as the title.
#' Please note that `plot` will still display a title unless instructed
#' otherwise. To avoid duplicated titles, the default is to have no title for
#' the guide.
#'
#' @param size
#' An absolute [`<unit>`][grid::unit] to set the size of the `plot` panel in
#' the orthogonal direction.
#'
#' @param reposition
#' A `<logical[1]>`. If `TRUE` (default) the `position` argument of this guide
#' will be propagated to the position scale bestowed upon the `plot` argument.
#' If `FALSE`, that position scale will retain its original `position` field.
#' Setting `reposition = TRUE` will generally tend to point axes outwards.
#'
#' @param theme
#' A [`<theme>`][ggplot2::theme] object to style the guide individually or
#' differently from the plot's theme settings. The order in which themes are
#' applies is as follows: (1) the main plot's theme (2) the `plot` argument's
#' theme and (3) this `theme` argument. The default `theme` argument suppresses
#' legends.
#'
#' @inheritParams common_parameters
#'
#' @details
#' This guide is subject to the following limitations:
#' * The x- or y-scale of the main plot override the corresponding scale in the
#'   `plot` argument. This ensures that the scales line up. The `plot` argument
#'   should not have the relevant scale.
#' * The `plot` argument cannot have custom facets. It must use the default
#'   `facet_null()`.
#' * This guide cannot be used in non-linear coordinate systems of the main plot
#'   and does not support non-linear coordinate systems in the `plot` argument.
#' * The `theme(panel.widths, panel.heights)` setting in the `plot` argument
#'   will be ignored in favour of the `size` argument.
#' * There is no mechanism to accommodate extra space needed by plot components
#'   outside the panel. This applies in the horizontal direction for x-axes and
#'   the vertical direction for y-axes. You may need to manually tweak the
#'   `theme(plot.margin)` setting of the main plot to accommodate these
#'   components.
#'
#' @returns A `<Guide>` object.
#' @export
#' @family standalone guides
#'
#' @examples
#' # A standard plot
#' main_plot <- ggplot(mpg, aes(displ, hwy, colour = drv)) +
#'   geom_point()
#'
#' # Simple plot sharing the x-variable
#' x_plot <- ggplot(mpg, aes(displ, fill = drv)) +
#'   geom_density(alpha = 0.7)
#'
#' # Simple plot sharing the y-variable
#' y_plot <- ggplot(mpg, aes(drv, hwy, colour = drv)) +
#'   geom_boxplot()
#'
#' # Typical use
#' main_plot + guides(
#'   x = guide_axis_plot(x_plot),
#'   y = guide_axis_plot(y_plot)
#' )
#'
#' main_plot + guides(
#'   # Include `fill` legend by overriding theme
#'   x = guide_axis_plot(x_plot, theme = NULL),
#'   # Change the size of the side-plot
#'   y = guide_axis_plot(y_plot, size = unit(4, "cm"))
#' )
#'
#' # Components outside panels may need to be manually acommodated
#' main_plot +
#'   guides(y = guide_axis_plot(y_plot + labs(title = "Boxplot"))) +
#'   theme(plot.margin = margin(25, 5.5, 5.5, 5.5))
#'
#' # Recursive use of this guide
#' main_plot + guides(x = guide_axis_plot(
#'   main_plot + guides(x = guide_axis_plot(x_plot))
#' ))
guide_axis_plot <- function(plot, title = NULL, size = unit(2, "cm"),
                            reposition = TRUE,
                            theme = theme_sub_legend(position = "none"),
                            position = waiver()) {
  check_plot(plot)

  if (!inherits(plot@facet, "FacetNull")) {
    facet_class <- class(plot@facet)[1]
    cli::cli_abort(c(
      "The {.arg plot} argument cannot have facets of class {.cls {facet_class}}.",
      `i` = "Only {.fn facet_null} is supported."
    ))
  }
  if (!plot@coordinates$is_linear()) {
    cli::cli_abort(
      "The {.arg plot} argument cannot have a non-linear coordinate system."
    )
  }

  check_bool(reposition)
  check_unit(size)

  new_guide(
    plot = plot,
    title = title,
    size = size,
    reposition = reposition,
    theme = theme,
    position = position,
    available_aes = c("x", "y"),
    super = GuideAxisPlot
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
GuideAxisPlot <- ggproto(
  "GuideAxisPlot", Guide,

  params = new_params(plot = NULL, size = unit(0.2, "null"), reposition = TRUE),

  hashables = exprs(plot),

  extract_params = function(scale, params, ...) {

    scale_copy <- scale$make_fixed_copy()

    if (params$reposition) {
      # We can modify-in-place here because the scale is already a copy
      scale_copy$position <- params$position
    }

    params$plot <- params$plot + scale_copy
    params
  },

  transform = function(params, coord, ...) {
    if (!coord$is_linear()) {
      cli::cli_abort(
        "Non-linear coordinates do not support {.fn guide_axis_plot}."
      )
    }
    return(params)
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    params <- replace_null(params, position = position, direction = direction)

    plot <- params$plot

    # Unify themes and apply
    theme <- theme + plot@theme + params$theme
    theme <- theme + switch(
      params$position,
      top = , bottom = theme(panel.heights = params$size, panel.widths  = NULL),
      left = , right = theme(panel.widths  = params$size, panel.heights = NULL),
      NULL
    )

    plot@theme <- theme
    plot <- ggplotGrob(plot)

    if (params$position %in% c("top", "bottom")) {

      panels <- panel_cols(plot)
      mid    <- plot[, (panels$l[1]:panels$r[1])]

      # Prepare plot parts to the left of the main panel
      left <- plot[, 1:(panels$l[1] - 1)]
      left_vp <- viewport(
        just = "right", x = 1,
        width = unit(sum(width_cm(left$widths)), "cm")
      )
      left <- editGrob(left, vp = left_vp)

      # Perpare plot parts to the right of the main panel
      right <- plot[, (panels$r[1] + 1):ncol(plot)]
      right_vp <- viewport(
        just = "left", x = 0,
        width = unit(sum(width_cm(right$widths)), "cm")
      )
      right <- editGrob(right, vp = right_vp)

      # Reassemble in gtable where only middle part has width
      height <- sum(plot$heights)
      gt <- gtable(widths = unit(c(0, 1, 0), "npc"), heights = height) |>
        gtable_add_grob(mid, t = 1, l = 2, clip = "off", name = "plot") |>
        gtable_add_grob(left, t = 1, l = 1, clip = "off", name = "left-decor") |>
        gtable_add_grob(right, t = 1, l = 3, clip = "off", name = "right-decor") |>
        gList() |>
        absoluteGrob(height = height)

    } else {

      panels <- panel_rows(plot)
      mid    <- plot[(panels$t[1]:panels$b[1])]

      # Prepare plot parts above the main panel
      top <- plot[1:(panels$t[1] - 1)]
      top_vp <- viewport(
        just = "bottom", y = 0,
        height = unit(sum(height_cm(top$heights)), "cm")
      )
      top <- editGrob(top, vp = top_vp)

      # Prepare plot parts below the main panel
      bottom <- plot[(panels$b[1] + 1):nrow(plot),  ]
      bottom_vp <- viewport(
        just = "top", y = 1,
        height = unit(sum(height_cm(bottom$heights)), "cm")
      )
      bottom <- editGrob(bottom, vp = bottom_vp)

      width <- sum(plot$widths)
      gt <- gtable(widths = width, heights = unit(c(0, 1, 0), "npc")) |>
        gtable_add_grob(mid, t = 2, l = 1, clip = "off", name = "plot") |>
        gtable_add_grob(top, t = 1, l = 1, clip = "off", name = "top-decor") |>
        gtable_add_grob(bottom, t = 3, l = 1, clip = "off", name = "bottom-decor") |>
        gList() |>
        absoluteGrob(width = width)
    }
    gt
  }

)


# Helpers -----------------------------------------------------------------

check_plot <- function(x, allow_null = FALSE, call = caller_env(),
                       arg = caller_arg(x)) {

  if (!missing(x)) {
    if (is_ggplot(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is.null(x)) {
      return(invisible(NULL))
    }
  }
  stop_input_type(
    x, as_cli("a {.cls ggplot} object"),
    allow_null = allow_null, arg = arg, call = call
  )
}
