# Constructor -------------------------------------------------------------

construct_guide <- function(
  title         = waiver(),
  available_aes = character(),
  name          = character(),
  ...,
  super
) {
  vec_assert(name, ptype = character(), size = 1L, arg = "name")

  ggproto(
    NULL, super,

    title         = title,
    available_aes = available_aes,
    name          = name,

    params = list(...)
  )
}

# Class -------------------------------------------------------------------

Guide <- ggproto(
  c("Guide", "guide"),

  # Stateful parameters
  title         = waiver(),
  available_aes = character(),
  name          = character(),
  hash          = character(),

  params        = list(),
  key           = new_data_frame(
    list(aesthetic = numeric(), .value = numeric(), .label = numeric())
  ),

  # Functions
  training_routine = function(self, scale, aesthetic = NULL) {
    self$train(scale, aesthetic)
  },

  train = function(self, scale, aesthetic = NULL) {
    abort("Not implemented")
  },

  merging_routine = function(self, new_guide) {
    self$merge(new_guide)
  },

  merge = function(self, new_guide) {
    abort("Not implemented")
  },

  scan_geoms = function(self, layers, default_mapping) {
    self$geom(layers, default_mapping)
  },

  geom = function(self, layers, default_mapping) {
    abort("Not implemented")
  },

  transform_routine = function(self, coord, panel_params) {
    self$transform(coord, panel_params)
  },

  transform = function(guide, coord, panel_params) {
    abort("Not implemented")
  },

  draw_guide = function(self, theme) {
    abort("Not implemented")
  }
)

# S3 Methods --------------------------------------------------------------

#' @export
guide_train.Guide <- function(guide, scale, aesthetic = NULL) {
  guide$training_routine(scale, aesthetic)
  guide
}

#' @export
guide_merge.Guide <- function(guide, new_guide) {
  guide$merging_routine(new_guide)
  guide
}

#' @export
guide_geom.Guide <- function(guide, layers, default_mapping) {
  guide$scan_geoms(layers, default_mapping)
  guide
}

#' @export
guide_transform.Guide <- function(guide, coord, panel_params) {
  guide$transform_routine(coord, panel_params)
  guide
}

#' @export
guide_gengrob.Guide <- function(guide, theme) {
  guide$draw_guide(theme)
}

