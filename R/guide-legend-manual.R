# Constructor -------------------------------------------------------------

#' Manual legend
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This is a guide that displays user-defined keys independent of scales.
#' It should only be used as a last resort when struggling to format a
#' conventional legend.
#'
#' @param labels
#' Labels to display next to the keys. Can be a `<character>` or `<expression>`
#' vector to set labels, or `NULL` to draw no labels.
#' @param ...
#' Arguments interpreted as aesthetics. For example: `colour = "red"`.
#' The aesthetics must have the same size as the `labels` argument, or have
#' size 1. These aesthetics may be overruled by fixed aesthetics set in the
#' `layers` argument.
#' @param layers
#' A `<list>` of layers (`<LayerInstance>` objects) created by the `geom_*()` or
#' `stat_*()` family of functions. These layers are used for their
#' [`key_glyph`][ggplot2::draw_key] drawing functions, as well as to populate
#' default aesthetics. Any fixed aesthetics provided to these layers overrule
#' aesthetics passed to the `...` argument.
#' @param title
#' One of the following to indicate the title of the guide:
#' * A `<character[1]>` or `<expression[1]>` to set a custom title.
#' * `NULL` to not display any title.
#' @param legend_args
#' A `<list>` of arguments passed on to [`guide_legend_base()`].
#'
#' @details
#' Because this guide is not tied to a scale, it can be given an arbitrary name
#' in `guides()`; as long as it doesn't clash with other aesthetics.
#'
#'
#' @returns A `<GuideCustom>` object.
#' @export
#' @family standalone guides
#' @family legend guides
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mtcars, aes(disp, mpg)) +
#'   geom_point()
#'
#' # Typical usage: set `label` and some aesthetics
#' p + guides(
#'   some_name = guide_legend_manual(
#'     label  = c("foo", "bar"),
#'     colour = c(NA, "black"),
#'     fill   = c("grey40", NA),
#'     layers = geom_col()
#'   )
#' )
#'
#' # Alternative: use `layers` to set aesthetics
#' p + guides(
#'   some_name = guide_legend_manual(
#'     label = c("foo", "bar"),
#'     layers = geom_col(
#'       # Must match length of `label`
#'       colour = c(NA, "black"),
#'       fill = c("grey40", NA)
#'     )
#'   )
#' )
#'
#' # You can use >1 layer
#' p + guides(
#'   some_name = guide_legend_manual(
#'     label = c("foo", "bar"),
#'     colour = c("tomato", "dodgerblue"),
#'     fill = NA,
#'     layers = list(geom_col(), geom_point())
#'   )
#' )
guide_legend_manual <- function(
  labels,
  ...,
  layers = list(geom_point()),
  title = NULL,
  legend_args = list()
) {
  labels <- label_as_vector(labels)
  key <- data_frame0(labels = labels, ..., .error_call = current_call()) |>
    rename_aes() |>
    rename("labels", ".label")
  if (is_empty(key)) {
    return(guide_none())
  }

  if (is_layer(layers)) {
    layers <- list(layers)
  }
  check_list_of(layers, "LayerInstance")

  legend <- inject(guide_legend_base(title = title, !!!legend_args))$params

  if (isTRUE(legend$reverse)) {
    key <- vec_slice(key, rev(vec_seq_along(key)))
  }
  legend$key <- key

  new_guide(
    legend = legend,
    layers = layers,
    title = legend$title,
    position = legend$position,
    direction = legend$direction,
    hash = hash(list(title, legend$key)),
    order = legend$order,
    super = GuideLegendManual
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
GuideLegendManual <- ggproto(
  "GuideLegendManual", GuideCustom,

  params = new_params(legend = NULL, layers = NULL),

  elements = list(),

  hashables = exprs(title),

  get_layer_key = function(params, layers, data = NULL, theme = NULL, ...) {
    # This is a simplified version of GuideLegend$get_layer_key that doesn't
    # demand matching aesthetics

    if (nrow(params$key) < 1) {
      return(params)
    }
    decor <- lapply(layers, function(layer) {

      key <- params$key
      key$.id <- seq_len(nrow(key))

      static_aes <- layer$aes_params
      static_aes <- static_aes[lengths(static_aes) %in% c(1L, nrow(key))]

      key <- layer$compute_geom_2(key, static_aes, theme)
      key <- modify_list(key, params$override.aes)

      # We're using *non*-computed geom/stat params here.
      # Computing these may demand `key` to have particular constraints.
      key_params <- c(layer$geom_params, layer$stat_params)

      list(
        draw_key = layer$geom$draw_key,
        data     = key,
        params   = key_params
      )
    })

    params$decor <- compact(decor)
    params
  },

  draw = function(self, theme, position = NULL, direction  = NULL,
                  params = self$params) {
    legend <- self$get_layer_key(
      params = params$legend,
      layers = params$layers,
      data   = NULL,
      theme  = theme
    )
    guide <- guide_legend_base()
    guide$draw(
      theme = theme,
      position = position,
      direction = direction,
      params = legend
    )
  }
)
