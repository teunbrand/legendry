# Public constructor ------------------------------------------------------

#' Vanilla axis guide
#'
#' This is mostly a re-implementation of [`guide_axis()`][ggplot2::guide_axis()]
#' with one single change: if tick lengths are negative, the label placement is
#' readjusted so it is placed outside the panel instead of inside the tick.
#'
#' @inheritParams ggplot2::guide_axis
#' @param ... Not currently used.
#'
#' @return A `<Guide>` ggproto object that can be given to the
#'   [`guides()`][ggplot2::guides()] function, or set as the `guide` argument
#'   in a position scale.
#' @export
#' @family vanilla guides
#' @family axis variants
#'
#' @examples
#' # Works in the same way as `guide_axis`.
#' ggplot(mpg, aes(displ, hwy)) +
#'   geom_point(aes(colour = as.factor(cyl))) +
#'   guides(x = guide_axis_vanilla(position = "top"))
guide_axis_vanilla <- function(
  title         = waiver(),
  check.overlap = FALSE,
  angle         = NULL,
  n.dodge       = 1L,
  order         = 0L,
  position      = waiver(),
  ...
) {
  construct_axis(
    title         = title,
    check.overlap = check.overlap,
    angle         = angle,
    n.dodge       = n.dodge,
    order         = order,
    position      = position,
    ...
  )
}

# Internal constructor ----------------------------------------------------

construct_axis <- function(
    ...,
    available_aes = c("x", "y"),
    name     = "axis",
    position = waiver(),
    super    = GuideAxis
) {
  if (!is_waive(position)) {
    arg_match0(position, c("top", "bottom", "left", "right"), "position")
  }

  guide <- construct_guide(
    available_aes = available_aes,
    name  = name,
    super = super,
    ...
  )
  guide$position <- position
  guide
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideAxis <- ggproto(
  "GuideAxis", Guide,

  ## Guide attributes -----------------------------------------------------

  position = NULL,

  ## Method implementation ------------------------------------------------

  training_routine = function(self, scale, aesthetic = NULL) {

    aesthetic <- aesthetic %||% scale$aesthetics[1]
    names(self$key) <- c(aesthetic, ".value", ".label")

    breaks    <- scale$get_breaks()

    if (length(intersect(scale$aesthetics, self$available_aes)) == 0) {
      warn(glue(
        "axis guide needs appropriate scales: ",
        glue_collapse(self$available_aes, ", ", last = " or ")
      ))
    } else if (length(breaks) > 0) {
      self$key <- self$train(scale, aesthetic)
    }

    self$name <- paste0(self$name, "_", aesthetic)
    self$hash <- hash(list(
      self$title, self$key$.value, self$key$.label, self$name
    ))
    return(invisible())
  },

  train = function(self, scale, aesthetic) {
    breaks        <- scale$get_breaks()
    mapped_breaks <- if (scale$is_discrete()) scale$map(breaks) else breaks

    key <- new_data_frame(setNames(list(mapped_breaks), aesthetic))
    key$.value <- breaks
    key$.label <- scale$get_labels(breaks)

    key
  },

  transform_routine = function(self, coord, panel_params) {
    if (is.null(self$position) || nrow(self$key) == 0) {
      return(invisible())
    }

    aesthetics <- aes_from_key(self$key)

    if (all(c("x", "y") %in% aesthetics)) {
      self$key <- coord$transform(guide$key, panel_params)
    } else {
      self$transform(coord, panel_params, aesthetic = aesthetics)
      check_position(self)
    }

    return(invisible())

  },

  transform = function(self, coord, panel_params, aesthetic) {
    key <- self$key

    other_aesthetic <- setdiff(c("x", "y"), aesthetic)
    override_value  <- if (self$position %in% c("bottom", "left")) -Inf else Inf

    key[[other_aesthetic]] <- override_value
    key <- coord$transform(key, panel_params)
    self$key <- key
  },

  scan_geoms = function(self, layers, default_mapping) {
    return(invisible())
  },

  merging_routine = function(self, new_guide) {
    if (!inherits(new_guide, "guide_none")) {
      warn(paste0(
        snake_class(self), ": Discarding guide on merge. ",
        "Do you have more than one guide with the same position?"
      ))
    }
    return(invisible())
  },

  draw_guide = function(self, theme) {
    # Extract relevant info
    key       <- self$key
    aesthetic <- aes_from_key(key)
    position  <- match.arg(substr(self$position, 1, 1), c("t", "b", "l", "r"))
    params    <- self$params

    # Setup stuff
    params    <- self$setup_params(position, params)
    elements  <- self$setup_elements(position, theme, params)

    # Actual drawing stuff
    line_grob <- self$build_line(elements, params)
    if (nrow(key) == 0) {
      return(absGrob(line_grob))
    }

    label_grob <- self$build_labels(elements, key, params)
    tick_grob  <- self$build_ticks(elements, key, params)

    self$assemble_drawing(
      ticks    = tick_grob,
      labels   = label_grob,
      lines    = line_grob,
      elements = elements,
      params   = params
    )
  },

  ## Drawing helpers ------------------------------------------------------

  setup_elements = function(position, theme, params) {
    aes      <- if (position %in% c("t", "b")) "x" else "y"
    position <- match.arg(position, c("top", "bottom", "left", "right"))

    element_names <- c(
      line        = "axis.line",
      ticks       = "axis.ticks",
      tick_length = "axis.ticks.length",
      label       = "axis.text"
    )
    element_names <- setNames(
      paste(element_names, aes, position, sep = "."),
      names(element_names)
    )
    elements <- lapply(element_names, calc_element, theme)

    if (inherits(elements$label, "element_text") && !is.null(params$angle)) {
      override <- override_axis_label(position, angle = params$angle)
      elements$label$angle <- override$angle %||% elements$label$angle
      elements$label$hjust <- override$hjust %||% elements$label$hjust
      elements$label$vjust <- override$vjust %||% elements$label$vjust
    }

    if (!is.null(params$colour)) {
      has_colour <- list_has_name(elements, "colour")
      elements   <- clapply(elements, has_colour, function(x) {
        `[[<-`(x, "colour", params$colour)
      })
    }

    elements
  },

  setup_params = function(position, params) {
    aes <- if (position %in% c("t", "b")) "x" else "y"

    if (is_vertical  <- position %in% c("l", "r")) {
      position_size  <- "height"
      gtable_element <- gtable_row
      gtable_measure <- gtable_width
      gtable_insert  <- gtable_add_cols
      measure_labels <- grobWidth
    } else {
      position_size  <- "width"
      gtable_element <- gtable_col
      gtable_measure <- gtable_height
      gtable_insert  <- gtable_add_rows
      measure_labels <- grobHeight
    }

    alt_aes  <- setdiff(c("x", "y"), aes)
    alt_size <- setdiff(c("width", "height"), position_size)

    if (position %in% c("r", "t")) {
      tick_direction <- 1
      alt_pos_panel  <- 0
      tick_order     <- c(2, 1)
    } else {
      tick_direction <- -1
      alt_pos_panel  <- 1
      tick_order     <- c(1, 2)
    }

    labels_first <- position %in% c("l", "t")
    opposite     <- chartr("tblr", "btrl", position)
    opposite     <- match.arg(opposite, c("top", "bottom", "left", "right"))

    c(params, list(
      aes  = aes, alt_aes = alt_aes,
      size = position_size, alt_size = alt_size,
      gtable_element = gtable_element,
      gtable_measure = gtable_measure,
      gtable_insert  = gtable_insert,
      measure_labels = measure_labels,
      tick_dir       = tick_direction,
      tick_ord       = tick_order,
      pos            = alt_pos_panel,
      labels_first   = labels_first,
      opposite       = opposite,
      vertical       = is_vertical
    ))
  },

  build_line = function(elements, params) {
    args <- list(elements$line, unit(c(0, 1), "npc"), rep(params$pos, 2L))
    names(args) <- c("element",  params$aes, params$alt_aes)
    do.call(element_grob, args)
  },

  build_labels = function(elements, key, params) {
    if ({n_breaks <- nrow(key)} == 0) {
      return(list(zeroGrob()))
    }

    key$.label <- unlanguage(key$.label)

    dodge_pos <- rep(seq_len(params$n.dodge), length.out = n_breaks)
    dodge_idx <- split(seq_len(n_breaks), dodge_pos)

    aes    <- params$aes
    margin <- paste0("margin_", params$alt_aes)

    key <- lapply(dodge_idx, function(idx) key[idx, , drop = FALSE])
    if (params$check.overlap) {
      key <- lapply(key, function(k) {
        k[prioritise_labels(nrow(k)), , drop = FALSE]
      })
    }

    lapply(key, function(ky) {
      exec(
        .fn = element_grob,
        element    = elements$label,
        label      = ky$.label,
        !!aes     := ky[[aes]],
        !!margin  := TRUE,
        family     = ky$.family,
        face       = ky$.face,
        colour     = ky$.colour,
        size       = ky$.size,
        hjust      = ky$.hjust,
        vjust      = ky$.vjust,
        lineheight = ky$.lineheight,
        margin     = params$margin,
        check.overlap = params$check.overlap
      )
    })
  },

  build_ticks = function(elements, key, params) {
    breaks   <- key[[params$aes]]
    n_breaks <- length(breaks)

    pos <- unit(c(params$pos, params$pos + (params$tick_dir * 1)), "npc")
    pos <- rep(pos[params$tick_ord], times = n_breaks)

    breaks <- rep(breaks, each = 2)
    breaks <- as_unit(breaks, "native")

    args <- setNames(
      list(elements$ticks, breaks, pos, rep(2, times = n_breaks)),
      c("element", params$aes, params$alt_aes, "id.lengths")
    )
    do.call(element_grob, args)
  },

  assemble_drawing = function(ticks, labels, lines, elements, params) {
    alt_sizes <- paste0(params$alt_size, "s")

    label_dims <- do.call(unit.c, lapply(labels, params$measure_labels))

    grobs     <- c(list(ticks), labels)
    grob_dims <- unit.c(max(elements$tick_length), label_dims)
    is_ticks  <- 1

    if (params$labels_first) {
      grobs     <- rev(grobs)
      grob_dims <- rev(grob_dims)
      is_ticks  <- length(grobs) - 1
    }

    gt <- do.call(
      params$gtable_element,
      args = setNames(
        list("axis", grobs, grob_dims, unit(1, "npc")),
        c("name", "grobs", alt_sizes, params$size)
      )
    )

    # Dodge labels for negative tick lengths
    negative_length <- max(unit(0, "pt"), -1 * elements$tick_length)
    gt <- params$gtable_insert(gt, negative_length, pos = is_ticks)

    vp <- do.call(
      viewport,
      args = setNames(
        list(params$pos, params$gtable_measure(gt), params$opposite),
        c(params$alt_aes, params$alt_size, "just")
      )
    )

    absGrob(
      grob   = gList(lines, gt),
      width  = max(gtable_width(gt),  unit(0, "pt")),
      height = max(gtable_height(gt), unit(0, "pt")),
      vp = vp
    )
  }
)

# Helpers -----------------------------------------------------------------

# To ensure whatever is fed to text grobs is valid text
unlanguage <- function(x) {
  if (is.list(x)) {
    if (any(vapply(x, is.language, logical(1)))) {
      x <- do.call(expression, x)
    } else {
      x <- unlist(x)
    }
  }
  x
}

# Variation of ggplot2:::absoluteGrob that automatically infers dimensions from
# the grob.
absGrob <- function(
    grob,
    width = NULL, height = NULL,
    xmin  = NULL, ymin   = NULL,
    vp = NULL
) {
  if (inherits(grob, "gtable")) {
    width  <- width  %||% gtable_width(grob)
    height <- height %||% gtable_height(grob)
    grob   <- gList(grob)
  }
  gTree(
    children = grob,
    width    = width  %||% grobWidth(grob),
    height   = height %||% grobHeight(grob),
    xmin = xmin, ymin = ymin, vp = vp, cl = "absoluteGrob"
  )
}

# Simple convenience to check if list elements have a particular name
list_has_name <- function(x, name) {
  vapply(lapply(x, names), `%in%`, x = name, logical(1), USE.NAMES = FALSE)
}

# Conditional lapply
clapply <- function(x, test, fun, ...) {
  x[test] <- lapply(x[test], fun, ...)
  x
}

# Convention is that the aesthetic is the only column in a key that isn't
# prefixed with a period. This extracts such columns.
aes_from_key <- function(x) {
  names(x)[!grepl("^\\.", names(x))][1]
}
