# Constructor -------------------------------------------------------------

#' Guide primitive: brackets
#'
#' This function constructs a brackets [guide primitive][guide-primitives].
#'
#' @param key A [range key][key_range] specification. See more information
#'   in the linked topic.
#' @param bracket A [bracket][bracket_options] by providing one of the
#'  following:
#'  * A bracket `<function>`, such as `bracket_square`.
#'  * A `<character[1]>` naming a bracket function without the
#'  '`bracket_`'-prefix, e.g. `"square"`.
#'  * A two-column `<matrix[n, 2]>` giving line coordinates for a bracket,
#'  like those created by bracket functions, such as `bracket_round()`.
#' @param oob A method for dealing with out-of-bounds (oob) ranges. Can be one
#'  of `"squish"`, `"censor"` or `"none"`.
#' @param drop_zero A `<logical[1]>` whether to drop near-zero width ranges
#'   (`TRUE`, default) or preserve them (`FALSE`).
#' @param pad_discrete A `<numeric[1]>` giving the amount ranges should be
#'   extended when given as a discrete variable. This is applied after
#'   the `drop_zero` setting.
#' @param levels_brackets A list of `<element_line>` objects to customise how
#'   brackets appear at every level.
#' @param levels_text A list of `<element_text>` objects to customise how
#'   text appears at every level.
#' @inheritParams common_parameters
#'
#' @return A `<PrimitiveBracket>` primitive guide that can be used inside other
#'   guides.
#' @family primitives
#' @export
#'
#' @details
#' # Styling options
#'
#' Below are the [theme][ggplot2::theme] options that determine the styling of
#' this guide, which may differ depending on whether the guide is used in an
#' axis or a legend context.
#'
#' Common to both types is the following:
#'
#' * `gguidance.bracket` an [`<element_line>`][ggplot2::element_line] for the
#'   line used to draw the brackets.
#' * `gguidance.backet.size` a [`<unit>`][grid::unit] setting the space afforded
#'   to a bracket.
#'
#' ## As an axis guide
#'
#' * `axis.text.{x/y}.{position}` an [`<element_text>`][ggplot2::element_text]
#'   for the text displayed over the brackets.
#'
#' ## As a legend guide
#'
#' * `legend.text` an [`<element_text>`][ggplot2::element_text] for the text
#'   displayed over the brackets.
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(interaction(drv, year), displ)) +
#'  geom_point()
#'
#' key <- key_range_manual(c(2, 4), c(5, 6), c("A", "B"))
#'
#' # Adding as secondary guides
#' p + guides(
#'   x.sec = primitive_bracket(),
#'   y.sec = primitive_bracket(key = key)
#' )
primitive_bracket <- function(
  key = "range_auto",
  bracket = "line",
  angle = waiver(),
  oob = "squish",
  drop_zero = TRUE,
  pad_discrete = 0.4,
  levels_brackets = NULL,
  levels_text = NULL,
  theme = NULL,
  position = waiver()
) {

  key <- resolve_key(key)
  oob <- arg_match0(oob, c("squish", "censor", "none"))
  check_bool(drop_zero)
  check_number_decimal(pad_discrete, allow_infinite = FALSE)
  check_list_of(
    levels_brackets,
    c("element_line", "element_blank", "NULL"),
    allow_null = TRUE
  )
  check_list_of(
    levels_text,
    c("element_text", "element_blank", "NULL"),
    allow_null = TRUE
  )
  bracket <- resolve_bracket(bracket)

  new_guide(
    key = key,
    oob = oob,
    angle = angle,
    drop_zero = drop_zero,
    pad_discrete = pad_discrete,
    bracket = bracket,
    levels_brackets = levels_brackets,
    levels_text = levels_text,
    theme = theme,
    position = position,
    available_aes = c("any", "x", "y", "r", "theta"),
    super = PrimitiveBracket
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
PrimitiveBracket <- ggproto(
  "PrimitiveBracket", Guide,

  params = new_params(
    key = NULL, oob = "squish", drop_zero = TRUE,
    pad_discrete = 0.4, angle = waiver(), bracket = cbind(c(0, 1), 0.5),
    levels_text = NULL, levels_brackets = NULL
  ),

  hashables = exprs(key, decor, bracket),

  elements = list(
    position = list(
      text = "axis.text", line = I("gguidance.bracket"),
      size = I("gguidance.bracket.size")
    ),
    legend = list(
      text = "legend.text", line = I("gguidance.bracket"),
      size = I("gguidance.bracket.size")
    )
  ),

  extract_key = range_extract_key,

  extract_params = extract_range_params,

  extract_decor = function(scale, aesthetic, position, key, bracket, ...) {
    bracket <- resolve_bracket(bracket)

    key <- vec_slice(key, key$.draw)
    n_keys <- nrow(key)

    brackets <- vec_rep(bracket, n_keys)
    keys <- vec_rep_each(key, nrow(bracket))

    value <- brackets[, 1] * (keys$end - keys$start) + keys$start

    data_frame0(
      !!aesthetic := value,
      offset = brackets[, 2],
      group = rep(seq_len(n_keys), each = nrow(bracket)),
      .level = keys$.level
    )
  },

  transform = function(self, params, coord, panel_params) {
    params$key <-
      transform_key(params$key, params$position, coord, panel_params)
    params$bbox  <- panel_params$bbox %||% list(x = c(0, 1), y = c(0, 1))
    params$decor <-
      transform_bracket(params$decor, params$position, coord, panel_params)
    params
  },

  setup_params = setup_range_params,

  setup_elements = primitive_setup_elements,

  override_elements = function(params, elements, theme) {
    elements$size <- cm(elements$size)
    elements
  },

  build_bracket = function(key, decor, elements, params) {
    levels   <- unique(c(key$.level, decor$.level))
    nlevels  <- length(levels)
    position <- params$position

    # Recycle custom elements per level to appropriate length
    bracket_levels <- rep0(params$levels_brackets, length.out = nlevels)
    text_levels    <- rep0(params$levels_text,     length.out = nlevels)

    # Justify labels along their ranges
    key <- justify_ranges(key, levels, elements$text, text_levels)

    if (is_theta(position)) {
      add <- if (position == "theta.sec") pi else 0
      key <- polar_xy(key, key$r, key$theta + add, params$bbox)
    }

    if (is_blank(elements$line) || is_empty(decor)) {
      decor <- vec_slice(decor, 0)
    } else if (position %in% .trbl) {
      offset  <- decor$offset
      offset  <- if (position %in% .trbl[1:2]) 1 - offset else offset
      decor$x <- switch(position, left = , right = offset, decor$x)
      decor$y <- switch(position, top = , bottom = offset, decor$y)
      decor$offset <- 0
    }

    offset <- elements$offset
    angle  <- params$angle %|W|% NULL
    size   <- elements$size
    text   <- angle_labels(elements$text, angle, params$position)

    brackets <- vector("list", nlevels)
    labels   <- vector("list", nlevels)

    for (i in seq_len(nlevels)) {

      # Render bracket
      brackets[[i]] <- draw_bracket(
        decor   = vec_slice(decor, decor$.level == levels[[i]]),
        element = combine_elements(bracket_levels[[i]], elements$line),
        size = size, offset = offset, position = position
      )
      offset <- offset + get_size_attr(brackets[[i]])

      # Render text
      labels[[i]] <- draw_labels(
        key     = vec_slice(key, key$.level == levels[[i]]),
        element = combine_elements(text_levels[[i]], text),
        angle = angle, offset = offset, position = position
      )
      offset <- offset + get_size_attr(labels[[i]])
    }
    if (params$position %in% c("top", "left")) {
      brackets <- rev(brackets)
      labels   <- rev(labels)
    }

    list(brackets = brackets, labels = labels)
  },

  measure_grobs = function(grobs, params, elements) {
    labels <- switch(
      params$position,
      top  = , bottom = height_cm(grobs$labels),
      left = , right  =  width_cm(grobs$labels),
      map_dbl(grobs$labels, get_size_attr)
    )
    is_bracket <- as.numeric(!is_each(grobs$brackets, is.zero))
    bracket <- is_bracket * elements$size
    list(brackets = bracket, labels = labels)
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    params <- replace_null(params, position = position, direction = direction)
    params <- self$setup_params(params)

    elems    <- self$setup_elements(params, self$elements, theme)
    elems    <- self$override_elements(params, elems, theme)
    brackets <- self$build_bracket(params$key, params$decor, elems, params)
    size     <- self$measure_grobs(brackets, params, elems)

    if (params$position %in% c("top", "left")) {
      grobs <- vec_interleave(brackets$labels, brackets$brackets)
      size  <- vec_interleave(size$labels, size$brackets)
    } else {
      grobs <- vec_interleave(brackets$brackets, brackets$labels)
      size  <- vec_interleave(size$brackets, size$labels)
    }

    primitive_grob(
      grob = grobs,
      size = unit(size, "cm"),
      position = params$position,
      name = "bracket"
    )
  }
)

# Helpers -----------------------------------------------------------------

draw_bracket <- function(decor, element, size, offset, position) {
  if (nrow(decor) < 2) {
    return(zeroGrob())
  }
  x <- unit(decor$x, "npc")
  y <- unit(decor$y, "npc")

  if (is_theta(position)) {
    offset <- (1 - decor$offset) * size + offset
    x <- x + unit(sin(decor$theta) * offset, "cm")
    y <- y + unit(cos(decor$theta) * offset, "cm")
  }

  id <- vec_unrep(decor$group)$times

  grob <- element_grob(element, x = x, y = y, id.lengths = id)
  if (!is_blank(element)) {
    attr(grob, "size") <- size
  }
  grob
}
