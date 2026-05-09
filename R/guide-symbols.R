# Constructor -------------------------------------------------------------

#' Symbol and upset axis guide
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @description
#' These axis guides can be used for set annotations of discrete categories.
#' The upset guide displays set intersections in matrix and is can be used
#' to replace Venn/Euler diagrams. The symbol guide also displays a matrix of
#' symbols, but requires manually specifying them.
#'
#' @param key
#' One of the following:
#' * An [upset key][key_upset] specification. For `guide_axis_upset`, specifying
#'   a `<character[n]>` is also passed to the `key_upset(order)` argument. An
#'   exception is made when the string is a valid key specification.
#' * A [symbol key][key_symbols] specification.
#'
#' @param connect
#' One of the following:
#' * A `<data.frame>` containing the following columns:
#'     * `value_start`, `value_end` Scale break values or `<numeric[n]>` values
#'       connecting along the axis.
#'     * `level_start`, `level_end` must be `<integer[n]>` values connecting
#'       perpendicular to the axis.
#'     * (Optional) columns for graphical parameters: `colour`, `linewidth`
#'       and `linetype`.
#' * A `<character[1]>` keyword for upset guides or logical symbols:
#'     * `"perpendicular"`: connect `TRUE` symbols (set membership for upset)
#'       perpendicular to the axis.
#'     * `"parallel"`: connect `TRUE` symbols parallel to the axis. This is
#'       *not* appropriate for upset guides.
#'
#' @param override.aes
#' A named `<list>` specifying graphical properties of points to apply to
#' symbols. Every element must either be length 1 or match the number of symbols
#' determined by the key. 3 symbols are used in upset guides or for logical
#' symbolism. Otherwise the number of unique values to the `key_symbols(symbol)`
#' argument determines the number of symbols.
#'
#' @param call A [call][rlang::topic-error-call] to display in messages.
#'
#' @inheritParams common_parameters
#'
#' @details
#' The upset axis does not predetermine the order the categories. If any sorting
#' based on set size needs to occur, the scale is the correct tool to handle
#' this task.
#'
#' ## Styling options
#'
#' Several styling options are provided in the theme, while individualised
#' styling is discussed below the table.
#'
#' | Theme setting | Description |
#' | ------------- | ----------- |
#' | `legendry.axis.subtitle` | [`element_text()`] for titles on the side |
#' | `legendry.axis.subtitle.position` | `"top"`, `"right"`, `"bottom"` or `"left"` |
#' | `legendry.zebra.light` | [`element_rect()`] for row shading |
#' | `legendry.zebra.dark` | [`element_rect()`] for alternate row shading |
#' | `legendry.table.spacing` | [`rel()`]/[`unit()`] for padding between levels |
#' | `legendry.point` | [`element_point()`] for symbol style |
#' | `legendry.connector` | [`element_line()`] for drawing `connect` lines. |
#'
#' Moreover, styling options *per group* of symbols can be set via the
#' `override.aes` argument.
#'
#' Styling options *per symbol* can be set in [`key_symbols()`] via the `...`
#' argument.
#'
#' Styling options *per line* in line connectors can be set by including
#' graphical parameters as columns in the `connect = <data.frame>` argument.
#'
#' @returns A `<Guide>` object.
#' @export
#' @family standalone guides
#'
#' @examples
#' # Example plot
#' p <- ggplot(mpg, aes(paste(drv, year))) +
#'   geom_bar()
#'
#' # A standard upset axis might not have right order of levels
#' p + guides(x = "axis_upset")
#'
#' # The levels can be manually adjusted to taste
#' p + guides(x = guide_axis_upset(c("1999", "2008", "4", "f", "r")))
#'
#' # The connections can be turned off to just show symbols
#' p + guides(x = guide_axis_upset(connect = NULL))
#'
#' # The style can be changed per group of symbols.
#' # We need to give 3 colours to also cover NA-breaks
#' p + guides(x = guide_axis_upset(
#'   override.aes = list(colour = c("purple", "orange", NA))
#' ))
#'
#' # For symbol guides you have to manually specify where you want symbols and
#' # connection lines appear.
#' p + guides(x = guide_axis_symbols(
#'   key_symbols(
#'     aesthetic = c("4 1999", "4 2008", "r 1999"),
#'     level = c("Lvl 1", "Lvl 2", "Lvl 3")
#'   ),
#'   connect = data.frame(
#'     value_start = "4 2008", value_end = "r 1999",
#'     level_start = 2, level_end = 3
#'   )
#' ))
guide_axis_symbols <- function(
  key = NULL,
  connect = NULL,
  title = waiver(),
  theme = NULL,
  override.aes = list(),
  position = waiver(),
  direction = NULL,
  call = NULL
) {
  call <- call %||% current_call()
  if (is.null(key)) {
    cli::cli_abort(
      "The {.arg key} argument is required. You can set a manual key \\
      using {.fn key_symbols}.",
      call = call
    )
  }

  check_connect_arg(connect)

  new_guide(
    key = key,
    title = title,
    theme = theme,
    connect = connect,
    override.aes = override.aes,
    position = position,
    direction = direction,
    available_aes = c("x", "y", "any"),
    call = call,
    super = GuideSymbols
  )
}

#' @rdname guide_axis_symbols
#' @export
guide_axis_upset <- function(
  key = "upset",
  connect = "perpendicular",
  title = waiver(),
  theme = NULL,
  override.aes = list(),
  position = waiver(),
  direction = NULL,
  call = NULL
) {

  if (is_character(key) && !(length(key) == 1L && is_key_string(key))) {
    key <- key_upset(order = key)
  }

  call <- call %||% current_call()
  if (is.null(key)) {
    cli::cli_abort(
      "The {.arg key} argument is required. You can set a key \\
      using {.fn key_upset}.",
      call = call
    )
  }

  check_connect_arg(connect)


  guide_axis_symbols(
    key = key,
    title = title,
    theme = theme,
    override.aes = override.aes,
    connect = connect,
    position = position,
    direction = direction,
    call = call
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname legendry_extensions
#' @format NULL
#' @usage NULL
GuideSymbols <- ggproto(
  "GuideSymbols", Guide,

  params = new_params(
    key = "upset", connect = NULL, override.aes = list(), call = NULL
  ),

  elements = list(
    position  = list(
      text    = I("legendry.axis.subtitle"),
      text_position = I("legendry.axis.subtitle.position"),
      light   = I("legendry.zebra.light"),
      dark    = I("legendry.zebra.dark"),
      hlines  = I("legendry.table.hlines"),
      vlines  = I("legendry.table.vlines"),
      spacing = I("legendry.table.spacing"),
      point   = I("legendry.symbol"),
      connector = I("legendry.connector")
    ),
    legend = list(
      text    = I("legendry.legend.subtitle"),
      text_position = I("legendry.legend.subtitle.position"),
      light   = I("legendry.zebra.light"),
      dark    = I("legendry.zebra.dark"),
      hlines  = I("legendry.table.hlines"),
      vlines  = I("legendry.table.vlines"),
      spacing = I("legendry.table.spacing"),
      point   = I("legendry.symbol"),
      connector = I("legendry.connector")
    )
  ),

  transform = function(self, params, coord, panel_params) {
    for (i in c("key", "decor")) {
      params[[i]] <-
        transform_key(params[[i]], params$position, coord, panel_params)
    }
    params
  },

  extract_key = function(scale, aesthetic, key, ...) {
    key <- standard_extract_key(scale, aesthetic, key, ...)
    key$.symbol <- key$.symbol %||% 1L
    key <- vec_slice(key, !is.na(key$.symbol))
    symbol <- key$.symbol
    if (is.logical(symbol)) {
      index <- match(symbol, c(TRUE, FALSE, NA), nomatch = 3L)
    } else if (is_integerish(symbol)) {
      index <- as.integer(symbol)
    } else {
      index <- match(symbol, levels(symbol) %||% sort(unique(symbol)))
    }
    key$.index <- index
    key
  },

  # Decor in the symbol/upset guide are connections between symbols
  extract_decor = function(scale, aesthetic, key, connect, ...) {
    if (is.null(connect)) {
      return()
    }

    # Custom, user-defined connections
    if (is.data.frame(connect)) {

      level <- vec_interleave(connect$level_start, connect$level_end)
      value <- vec_interleave(connect$value_start, connect$value_end)
      value <- scale$map(value)

      required_cols <- c("value_start", "value_end", "level_start", "level_end")
      extra_cols <- setdiff(names(connect), required_cols)
      extra <- NULL
      if (length(extra_cols) > 0L) {
        extra <- vec_rep_each(connect[extra_cols], 2L)
        extra <- rename_aes(extra, arg = "connect")
        names(extra) <- paste0(".", names(extra))
      }
      connect <- data_frame0(
        !!aesthetic := value,
        .level = level,
        .id = rep(vec_seq_along(connect), each = 2L),
        !!!extra
      )
      return(connect)
    }

    if (!is_logical(key$.symbol)) {
      cli::cli_abort(
        "{.arg connect} cannot be {.val {connect}} when key symbols are \\
        not logical."
      )
    }

    # Upset connections
    major <- switch(connect, perpendicular = ".row", ".col")
    minor <- setdiff(c(".row", ".col"), major)

    connect <- vec_split(key, key[[major]])
    connect <- lapply(connect$val, function(df) {

      hits <- df$.symbol & !is.na(df$.symbol)
      if (sum(hits) < 2L) {
        return(NULL)
      }
      i <- which(df$.symbol)
      minor <- df[[minor]][i]
      i <- i[c(which.max(minor), which.min(minor))]
      vec_slice(df, i)

    })
    connect <- vec_rbind(!!!connect)
    connect$.id <- rep(seq(nrow(connect) / 2L), each = 2L)
    connect
  },

  extract_params = function(scale, params, key, ...) {
    # Ensure override.aes is setup with the right lengths
    override <- params$override.aes
    if (is.logical(params$key$.symbol)) {
      override$shape <- override$shape %||% c(19L, 1L, 12L)
      n <- 3L
    } else {
      n <- max(params$key$.index)
    }

    wrong <- which(!(lengths(override) %in% c(1L, n)))
    if (length(wrong)) {
      problems <- paste0("override.aes$", names(override)[wrong])
      lens <- lengths(override)[wrong]
      lens <- paste0(
        "size", if (length(lens) > 1L) "s", " ",
        oxford_comma(lens, final = "and")
      )
      cli::cli_abort(
        "Can't recycle {.and {.arg {problems}}} ({lens}) to size {n}.",
        call = params$call
      )
    }
    override <- rename_aes(override)
    params$override.aes <- df_list(!!!override, .size = n)
    params
  },

  setup_params = function(params) {
    key <- params$key
    key$.symbol <- key$.symbol %||% TRUE
    if (params$position %in% c("top", "bottom")) {
      key$.row <- key$.row %||% 1L
    } else {
      key$.col <- key$.col %||% 1L
    }
    params$key <- key
    params
  },

  setup_elements = function(params, elements, theme) {
    primitive_setup_elements(params, elements, theme)
  },

  override_elements = function(params, elements, theme) {
    elements$text_position <-
      switch(
        params$position,
        top = , bottom = setdiff(elements$text_position, c("top", "bottom")),
        setdiff(elements$text_position, c("left", "right"))
      )[[1L]]
    type <- if (any(params$aesthetic %in% c("x", "y"))) "axis" else "legend"
    elements$text <- elements$title <-
      setup_side_title(theme, elements$text_position, type)
    elements
  },

  build_labels = function(key, elements, params) {
    labels <- levels(key$.value)
    if (length(labels) < 1L) {
      return(NULL)
    }
    lapply(labels, function(lab) {
      element_grob(elements$text, label = lab, margin_x = TRUE, margin_y = TRUE)
    })
  },

  build_decor = function(self, decor, grobs, elements, params) {
    key <- params$key
    key <- vec_slice(key, !is.na(key$.symbol))
    levels <- vec_group_loc(key$.col)
    levels <- vec_slice(levels, order(levels$key))
    point <- elements$point

    groups <- vec_group_id(key$.symbol)
    groups[is.na(key$.symbol)] <- NA
    n_groups <- attr(groups, "n")

    override <- params$override.aes
    x <- switch(params$position, top = , bottom = key$x, rep(0.5, nrow(key)))
    y <- switch(params$position, left = , right = key$y, rep(0.5, nrow(key)))

    points <- lapply(levels$loc, function(level) {
      grobs <- list()
      size_tracker <- point$size
      for (group_id in seq_len(n_groups)) {
        member <- level[groups[level] == group_id]
        if (length(member) < 1L) {
          next
        }
        key_members <- vec_slice(key, member)
        i <- key_members$.index
        size <- key_members$.size %||% override$size[i]
        size_tracker <- max(size_tracker, size)
        grob <- element_grob(
          point,
          x = unit(x[member], "native"),
          y = unit(y[member], "native"),
          colour = key_members$.colour %||% override$colour[i],
          fill   = key_members$.fill   %||% override$fill[i],
          shape  = key_members$.shape  %||% override$shape[i],
          stroke = key_members$.stroke %||% override$stroke[i],
          size   = size
        )
        grobs <- c(grobs, list(grob))
      }
      gTree(children = inject(gList(!!!grobs)), size = size_tracker)
    })

    size <- map_dbl(points, function(x) x$size)
    size <- unit(size + height_cm(elements$spacing) * 10.0, "mm")
    along <- seq_along(points)

    zebra <- list(element_grob(elements$light), element_grob(elements$dark))
    zebra <- rep_len(zebra, length(size))

    if (params$position %in% c("top", "bottom")) {
      gt <- gtable(widths = unit(1.0, "npc"), heights = size) |>
        gtable_add_grob(
          points, l = 1L, t = along, z = 2L,
          name = paste0("symbols-", along)
        ) |>
        gtable_add_grob(
          zebra, l = 1L, t = along, z = 0L,
          name = paste0("zebra-", along)
        )
    } else {
      gt <- gtable(widths = size, heights = unit(1.0, "npc")) |>
        gtable_add_grob(
          points, l = along, t = 1L, z = 2L,
          name = paste0("symbols-", along)
        ) |>
        gtable_add_grob(
          zebra, l = along, t = 1L, z = 0L,
          name = paste0("zebra-", along)
        )
    }
    gt
  },

  measure_grobs = function(grobs, params, elements) {
    if (params$position %in% c("top", "bottom")) {
      lab_height <- height_cm(grobs$labels %||% unit(0.0, "cm"))
      tab_height <- height_cm(grobs$decor$heights)
      pmax(lab_height, tab_height)
    } else {
      lab_width <- width_cm(grobs$labels %||% unit(0.0, "cm"))
      tab_width <- width_cm(grobs$decor$widths)
      pmax(lab_width, tab_width)
    }
  },

  assemble_drawing = function(grobs, layout, sizes, params, elems) {
    connectors <- draw_connectors(params$decor, params, elems, sizes)

    labels <- grobs$labels
    along  <- seq_along(labels)
    table  <- grobs$decor

    if (params$position %in% c("top", "bottom")) {
      width <- unit(max(width_cm(labels)), "cm")
      table$heights <- unit(sizes, "cm")
      if (!is_zero(connectors)) {
        table <-
          gtable_add_grob(
            table, connectors, l = 1L, t = 1L, b = -1L,
            name = "connectors", clip = "off"
          )
      }
      if (elems$text_position == "left") {
        table <- table |>
          gtable_add_cols(c(-1.0, 1.0) * width, pos = 0L) |>
          gtable_add_grob(
            labels, t = along, l = 2L,
            name = paste0("label-", along)
          )
      } else {
        table <- table |>
          gtable_add_cols(c(1.0, -1.0) * width, pos = -1L) |>
          gtable_add_grob(
            labels, t = along, l = 2L,
            name = paste0("label-", along)
          )
      }
    } else {
      height <- unit(max(height_cm(labels)), "cm")
      table$widths <- unit(sizes, "cm")
      if (!is_zero(connectors)) {
        table <-
          gtable_add_grob(
            table, connectors, l = 1L, r = -1L, t = 1L,
            name = "connectors", clip = "off"
          )
      }
      if (elems$text_position == "bottom") {
        table <- table |>
          gtable_add_rows(c(1.0, -1.0) * height, pos = -1L) |>
          gtable_add_grob(
            labels, t = 2L, l = along,
            name = paste0("label-", along),
            clip = "off"
          )
      } else {
        table <- table |>
          gtable_add_rows(c(-1.0, 1.0) * height, pos = 0L) |>
          gtable_add_grob(
            labels, t = 2L, l = along,
            name = paste0("label-", along),
            clip = "off"
          )
      }
    }
    table
  }
)

check_connect_arg <- function(
  connect, arg = caller_arg(connect), env = caller_env()
) {
  if (is.null(connect)) {
    return(invisible())
  }
  if (is.character(connect)) {
    arg_match0(
      connect, c("perpendicular", "parallel"),
      arg_nm = arg, error_call = env
    )
    return(invisible())
  }
  cols <- c("value_start", "value_end", "level_start", "level_end")
  check_columns(connect, cols)
  for (col in cols[3L:4L]) {
    check_object(
      connect[[col]],
      is_integerish,
      what = "{.cls integer}",
      arg = paste0(arg, "$", col),
      call = env
    )
  }
  invisible()
}

draw_connectors <- function(decor, params, elems, sizes) {
  if (is.null(params$connect) || is.null(decor)) {
    return(zeroGrob())
  }

  levels <- decor[[".level"]] %||% decor[[".col"]]
  if (params$position %in% c("top", "bottom")) {
    oppo <- sum(sizes) - cumsum(sizes) + sizes / 2.0
    x <- unit(decor$x, "native")
    y <- unit(oppo[levels], "cm")
  } else {
    oppo <- c(0.0, cumsum(sizes[-length(sizes)])) + sizes / 2.0
    x <- unit(oppo[levels], "cm")
    y <- unit(decor$y, "native")
  }

  element_grob(
    elems$connector,
    x = x, y = y,
    id.lengths = vec_unrep(decor$.id)$times,
    colour = decor[[".colour"]],
    linewidth = decor[[".linewidth"]],
    linetype = decor[[".linetype"]]
  )
}
