# Constructor -------------------------------------------------------------

#' Guide composition
#'
#' Guide composition is a meta-guide orchestrating an ensemble of other guides.
#' On their own, a 'composing' guide is not very useful as a visual reflection
#' of a scale.
#'
#' @param guides A `<list>` of guides wherein each element is one of the
#'   following:
#'   * A `<Guide>` class object.
#'   * A `<function>` that returns a `<Guide>` class object.
#'   * A `<character[1]>` naming such a function, without the `guide_` or
#'   `primitive_` prefix.
#' @param args A `<list>` of arguments to pass to guides that are given either
#'   as a function or as a string.
#' @param ... Additional parameters to pass on to
#'   [`new_guide()`][ggplot2::new_guide].
#' @param available_aes A `<character>` giving aesthetics that must match the
#'   the guides.
#' @param super A `<Compose>` class object giving a meta-guide for composition.
#' @param .call A [call][rlang::topic-error-call] to display in messages.
#'
#' @name guide-composition
#' @return A `<Compose>` (sub-)class guide that composes other guides.
#' @export
#' @family composition
#'
#' @examples
#' # new_compose() is not intended to be used directly
new_compose <- function(guides, args = list(), ...,
                        available_aes = c("any", "x", "y", "r", "theta"),
                        call = caller_env(), super = Compose) {

  guides <- lapply(guides, validate_guide, args = args, call = call)
  if (length(guides) < 1) {
    cli::cli_abort("There must be at least one guide to compose.", call = call)
  }

  available_aes <- compatible_aes(guides, available_aes)
  guide_params  <- lapply(guides, `[[`, name = "params")

  new_guide(
    guides = guides,
    guide_params = guide_params,
    available_aes = available_aes,
    super = super,
    ...
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
Compose <- ggproto(
  "Compose", Guide,

  params = new_params(
    guides = list(), guide_params = list(),
    key = NULL, angle = waiver()
  ),

  elements = list(spacing = "gguidance.guide.spacing"),

  train = function(self, params = self$params, scale, aesthetic = NULL, ...) {
    position  <- params$position  <- params$position %|W|% NULL
    aesthetic <- params$aesthetic <- aesthetic %||% scale$aesthetics[1]
    check_position(position, allow_null = TRUE)

    guide_params <- params$guide_params
    for (i in seq_along(params$guides)) {
      guide_params[[i]]$position <- position
      guide_params[[i]]$angle <- guide_params[[i]]$angle %|W|% params$angle
      guide_params[[i]]["key"] <- list(guide_params[[i]]$key %||% params$key)
      guide_params[[i]] <- params$guides[[i]]$train(
        params = guide_params[[i]], scale = scale, aesthetic = aesthetic,
        ...
      )
    }
    params$guide_params <- guide_params
    params
  },

  transform = function(self, params, coord, panel_params) {
    params$guide_params <- loop_guides(
      params$guides, params$guide_params, "transform",
      coord = coord, panel_params = panel_params
    )
    params
  },

  get_layer_key = function(params, layers, data = NULL) {
    params$guide_params <- loop_guides(
      params$guides, params$guide_params, "get_layer_key",
      layers = layers, data = data
    )
    params
  },

  draw = function(...) {
    cli::cli_abort("Not implemented.")
  }
)

# Helpers -----------------------------------------------------------------

loop_guides <- function(guides, params, method, ...) {
  for (i in seq_along(guides)) {
    params[[i]] <- guides[[i]][[method]](params = params[[i]], ...)
  }
  params
}

compatible_aes <- function(guides, available_aes, call = caller_env()) {

  available <- lapply(guides, `[[`, name = "available_aes")
  common <- Reduce(any_intersect, available)
  if (length(common) < 1) {
    cli::cli_abort(
      "The guides to combine have no shared {.field available aesthetics}.",
      call = call
    )
  }
  if (!is.null(available_aes)) {
    common <- any_intersect(available_aes, common)
    if (length(common) < 1) {
      cli::cli_abort(c(
        "The guides have incompatible {.arg available_aes} settings.",
        "They must include {.or {.val {available_aes}}}."
      ), call  = call)
    }
  }
  common
}

any_intersect <- function(x, y) {
  if ("any" %in% x) {
    x <- union(x, setdiff(y, c("x", "y", "r", "theta")))
  }
  if ("any" %in% y) {
    y <- union(y, setdiff(x, c("x", "y", "r", 'theta')))
  }
  intersect(x, y)
}

validate_guide <- function(guide, args = list(), env = global_env(),
                           call = caller_env()) {
  input <- guide
  if (is.character(guide)) {
    guide <- find_global(paste0("guide_", input), env = env, mode = "function")
  }
  if (is.null(guide) && is.character(input)) {
    guide <- find_global(paste0("primitive_", input), env = env, mode = "function")
  }
  if (is.function(guide)) {
    args  <- args[intersect(names(args), fn_fmls_names(guide))]
    guide <- inject(guide(!!!args))
  }
  if (inherits(guide, "Guide")) {
    return(guide)
  }
  cli::cli_abort("Unknown guide: {input}.", call = call)
}
