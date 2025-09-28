
guide_legend_manual <- function(
  labels,
  ...,
  layers = list(geom_point()),
  title = waiver(),
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



GuideLegendManual <- ggproto(
  "GuideLegendManual", GuideCustom,

  params = new_params(legend = NULL, layers = NULL),

  elements = list(),

  hashables = exprs(title),

  get_layer_key = function(params, layers, data = NULL, theme = NULL) {
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
    legend <- self$get_layer_key(params$legend, params$layers, NULL, theme)
    guide <- guide_legend_base()
    guide$draw(
      theme = theme,
      position = position,
      direction = direction,
      params = legend
    )
  }
)
