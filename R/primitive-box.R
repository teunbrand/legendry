# Constructor -------------------------------------------------------------

#' Guide primitives: boxes
#'
#' This function constructs a boxes [guide primitive][guide-primitives].
#'
#' @inheritParams primitive_bracket
#' @param min_size A [`<grid::unit[1]>`][grid::unit] setting the minimal size
#'   of a box.
#' @param levels_box A list of `<element_rect>` objects to customise how
#'   boxes appear at every level.
#'
#' @return A `<PrimitiveBox>` primitive guide that can be used inside other
#'   guides.
#' @family primitives
#' @export
#'
#' @details
#' # Styling options
#'
#' Below are the [theme][ggplot2::theme] options that determine the styling of
#' this guide, which may differ depending on whether the guide is used in
#' an axis or in a legend context.
#'
#' Common to both types is the following:
#'
#' * `gguidance.box` an [`<element_rect>`][ggplot2::element_rect] for the boxes
#'   to draw.
#'
#' ## As an axis guide
#'
#' * `axis.text.{x/y}.{position}` an [`<element_text>`][ggplot2::element_text]
#'   for the text inside the boxes.
#'
#' ## As a legend guide
#'
#' * `legend.text` an [`<element_text>`][ggplot2::element_text] for the text
#'   inside the boxes.
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
#'   x.sec = primitive_box(),
#'   y.sec = primitive_box(key = key)
#' )
primitive_box <- function(
  key = "range_auto",
  angle = waiver(),
  oob = "squish",
  drop_zero = TRUE,
  pad_discrete = 0.4,
  min_size = NULL,
  levels_box = NULL,
  levels_text = NULL,
  theme = NULL,
  position = waiver()
) {

  key <- resolve_key(key)
  oob <- arg_match0(oob, c("squish", "censor", "none"))
  check_bool(drop_zero)
  check_number_decimal(pad_discrete, allow_infinite = FALSE)
  check_list_of(
    levels_box,
    c("element_rect", "element_blank", "NULL"),
    allow_null = TRUE
  )
  check_list_of(
    levels_text,
    c("element_text", "element_blank", "NULL"),
    allow_null = TRUE
  )

  new_guide(
    key = key,
    oob = oob,
    angle = angle,
    drop_zero = drop_zero,
    pad_discrete = pad_discrete,
    min_size = min_size,
    levels_box = levels_box,
    levels_text = levels_text,
    theme = theme,
    position = position,
    available_aes = c("any", "x", "y", "r", "theta"),
    super = PrimitiveBox
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
PrimitiveBox <- ggproto(
  "PrimitiveBox", Guide,

  hashables = exprs(key, decor),

  params = new_params(
    key = NULL, oob = "squish", drop_zero = TRUE,
    pad_discrete = 0.4, angle = waiver(), min_size = NULL,
    levels_text = NULL, levels_box = NULL
  ),

  elements = list(
    position = list(text = "axis.text",   box = I("gguidance.box")),
    legend   = list(text = "legend.text", box = I("gguidance.box"))
  ),

  extract_key = range_extract_key,

  extract_params = extract_range_params,

  extract_decor = function(scale, aesthetic, key, ...) {

    key <- vec_slice(key, key$.draw)
    n_keys <- nrow(key)
    value <- vec_interleave(key$start, key$end)

    data_frame0(
      !!aesthetic := value,
      group  = rep(seq_len(n_keys), each = 2),
      .level = rep(key$.level, each = 2)
    )
  },

  transform = function(self, params, coord, panel_params) {
    params$key <-
      transform_key(params$key, params$position, coord, panel_params)
    params$bbox <- panel_params$bbox %||% list(x = c(0, 1), y = c(0, 1))
    if (!is_empty(params$decor)) {
      other <- switch(params$position, bottom = , left = , theta.sec = -Inf, Inf)
      params$decor <- replace_null(params$decor, x = other, y = other)
      params$decor <- coord_munch(coord, params$decor, panel_params)
      if (params$position == "theta.sec") {
        params$decor$theta <- params$decor$theta + pi
      }
    }
    params
  },

  setup_params = setup_range_params,

  setup_elements = primitive_setup_elements,

  build_box = function(key, decor, elements, params) {

    levels   <- unique(c(key$.level, decor$.level))
    nlevels  <- length(levels)
    position <- params$position

    # Recycle custom elements per level to appropriate length
    box_levels  <- rep0(params$levels_box,  length.out = nlevels)
    text_levels <- rep0(params$levels_text, length.out = nlevels)

    # Justify labels along their ranges
    key <- justify_ranges(key, levels, elements$text, text_levels)

    if (is_theta(position)) {
      add <- if (position == "theta.sec") pi else 0
      key <- polar_xy(key, key$r, key$theta + add, params$bbox)
    }

    grobs  <- vector("list", nlevels)
    offset <- elements$offset
    angle  <- params$angle %|W|% NULL
    min_size <- cm(params$min_size %||% 0.2)
    sizes <- numeric(nlevels)
    text  <- angle_labels(elements$text, angle, position)

    measure <- switch(
      position,
      left = , right = width_cm,
      top = , bottom = height_cm,
      get_size_attr
    )

    for (i in seq_len(nlevels)) {

      # Render text
      labels <- draw_labels(
        vec_slice(key, key$.level == levels[[i]]),
        combine_elements(text_levels[[i]], text),
        angle = angle, offset = offset, position = position
      )
      sizes[i] <- max(measure(labels), min_size)

      # Render box
      box <- draw_box(
        vec_slice(decor, decor$.level == levels[[i]]),
        combine_elements(box_levels[[i]], elements$box),
        size = sizes[i], offset = offset, position = position
      )

      offset <- offset + sizes[i]
      grobs[[i]] <- grobTree(box, labels)
    }

    if (position %in% c("top", "left")) {
      grobs <- rev(grobs)
      sizes <- rev(sizes)
    }

    attr(grobs, "sizes") <- sizes
    grobs
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {
    params <- replace_null(params, position = position, direction = direction)
    params <- self$setup_params(params)

    elems <- self$setup_elements(params, self$elements, theme)
    box <- self$build_box(params$key, params$decor, elems, params)

    if (length(box) < 1) {
      return(zeroGrob())
    }

    primitive_grob(
      grob = box,
      size = unit(attr(box, "sizes"), "cm"),
      position = params$position,
      name = "box"
    )

  }
)

# Helpers -----------------------------------------------------------------

draw_box = function(decor, element, size, offset, position) {
  if (nrow(decor) < 2 || is_blank(element)) {
    return(zeroGrob())
  }
  aes <- switch(position, top = , bottom = "x", left = , right = "y", "theta")

  rle <- new_rle(decor$group)
  if (is_theta(position)) {
    rev <- vec_slice(decor, nrow(decor):1)
    x <- unit(c(decor$x, rev$x), "npc")
    y <- unit(c(decor$y, rev$y), "npc")
    theta  <- c(decor$theta, rev$theta)
    offset <- rep(c(0, size) + offset, each = nrow(decor))
    x <- x + unit(sin(theta) * offset, "cm")
    y <- y + unit(cos(theta) * offset, "cm")
    id <- c(decor$group, rev$group)
    gp <- gpar(
      col = element$colour,
      fill = element$fill,
      lwd = (element$linewidth * .pt) %0% NULL,
      lty = (element$linetype)
    )
    grob <- polygonGrob(x = x, y = y, id = id, gp = gp)
    return(grob)
  }

  rle <- new_rle(decor$group)
  start <- decor[[aes]][rle$start]
  end   <- decor[[aes]][rle$end]
  min <- pmin(start, end)
  max <- pmax(start, end)
  args <- list(
    x = min, width = max - min, hjust = 0, vjust = 0.5,
    y = 0.5, height = 1
  )
  if (position %in% c("left", "right")) {
    args <- flip_names(args)
  }
  inject(element_grob(element, !!!args))
}
