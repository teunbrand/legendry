guide_symbols <- function(
    key = NULL,
    title = waiver(),
    theme = NULL,
    connect = NULL,
    override.aes = list(),
    position = waiver(),
    direction = NULL
) {

  if (is.null(key)) {
    cli::cli_abort(
      "The {.arg key} argument is required. You can set a manual key \\
      using {.fn key_symbols}.",
    )
  }

  new_guide(
    key = key,
    title = title,
    theme = theme,
    connect = connect,
    override.aes = rename_aes(override.aes),
    position = position,
    direction = direction,
    available_aes = c("x", "y", "any"),
    super = GuideSymbols
  )
}

guide_upset <- function(
  key = "upset",
  title = waiver(),
  theme = NULL,
  override.aes = list(),
  position = waiver(),
  direction = NULL
) {

  if (is_character(key) && !(length(key) == 1 && is_key_string(key))) {
    key <- key_upset(order = key)
  }

  if (is.null(key)) {
    cli::cli_abort(
      "The {.arg key} argument is required. You can set a key \\
      using {.fn key_upset}.",
    )
  }

  guide_symbols(
    key = key,
    title = title,
    theme = theme,
    override.aes = override.aes,
    connect = "perpendicular",
    position = position,
    direction = direction
  )
}

GuideSymbols <- ggproto(
  "GuideSymbols", Guide,

  params = new_params(key = "upset", side_titles = NULL, connect = NULL, override.aes = list()),

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
    params$key <-
      transform_key(params$key, params$position, coord, panel_params)
    params
  },

  extract_key = standard_extract_key,

  setup_params = function(params) {
    key <- params$key
    key$.symbol <- key$.symbol %||% TRUE
    if (params$position %in% c("top", "bottom")) {
      key$.row <- key$.row %||% 1
    } else {
      key$.col <- key$.col %||% 1
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
      )[[1]]
    type <- if (any(params$aesthetic %in% c("x", "y"))) "axis" else "legend"
    elements$text <- elements$title <-
      setup_side_title(theme, elements$text_position, type)
    elements
  },

  build_labels = function(key, elements, params) {
    labels <- levels(key$.value)
    if (length(labels) < 1) {
      return(NULL)
    }
    lapply(labels, function(lab) {
      element_grob(elements$text, label = lab, margin_x = TRUE, margin_y = TRUE)
    })
  },

  build_decor = function(decor, grobs, elements, params) {

    key <- params$key
    key <- vec_slice(key, !is.na(key$.symbol))
    idx <- vec_group_loc(key$.col)
    point <- elements$point

    x <- switch(params$position, top = , bottom = key$x, rep(0.5, nrow(key)))
    y <- switch(params$position, left = , right = key$y, rep(0.5, nrow(key)))
    groups <- vec_group_id(key$.symbol)
    groups[is.na(key$.symbol)] <- NA
    n_groups <- attr(groups, "n")

    if (is.logical(key$.symbol)) {
      j <- match(key$.symbol, c(TRUE, FALSE, NA), nomatch = 3)
      override <- df_list(!!!params$override.aes, .size = 3)
      override$shape <- override$shape %||% c(19, 1, 12)
    } else {
      if (is_integerish(key$.symbol)) {
        j <- as.integer(key$.symbol)
      } else {
        j <- match(key$.symbol, levels(key$.symbol) %||% sort(unique(key$.symbol)))
      }
      override <- df_list(!!!params$override.aes, .size = max(j))
    }

    points <- lapply(idx$loc, function(idx) {
      grobs <- list()
      track_size <- point$size
      for (i in seq_len(n_groups)) {
        member <- idx[groups[idx] == i]
        if (length(member) < 1) {
          next
        }
        key_members <- vec_slice(key, member)
        k <- j[member]
        size <- key_members$.size %||% override$size[k]
        track_size <- max(track_size, size)
        grob <- element_grob(
          point,
          x = unit(x[member], "native"),
          y = unit(y[member], "native"),
          colour = key_members$.colour %||% override$colour[k],
          fill   = key_members$.fill   %||% override$fill[k],
          shape  = key_members$.shape  %||% override$shape[k],
          stroke = key_members$.stroke %||% override$stroke[k],
          size   = size
        )
        grobs <- c(grobs, list(grob))
      }
      gTree(children = inject(gList(!!!grobs)), size = track_size)
    })

    size <- map_dbl(points, function(x) x$size)
    size <- unit(size + height_cm(elements$spacing) * 10, "mm")
    along <- seq_along(points)

    zebra <- rep(list(
      element_grob(elements$light),
      element_grob(elements$dark)
    ), length.out = length(size))

    if (params$position %in% c("top", "bottom")) {
      gt <- gtable(widths = unit(1, "npc"), heights = size) |>
        gtable_add_grob(
          points, l = 1, t = along, z = 2,
          name = paste0("symbols-", along)
        ) |>
        gtable_add_grob(
          zebra, l = 1, t = along, z = 0,
          name = paste0("zebra-", along)
        )
    } else {
      gt <- gtable(widths = size, heights = unit(1, "npc")) |>
        gtable_add_grob(
          points, l = along, t = 1, z = 2,
          name = paste0("symbols-", along)
        ) |>
        gtable_add_grob(
          zebra, l = along, t = 1, z = 0,
          name = paste0("zebra-", along)
        )
    }
    gt
  },

  measure_grobs = function(grobs, params, elements) {
    if (params$position %in% c("top", "bottom")) {
      lab_height <- height_cm(grobs$labels %||% unit(0, "cm"))
      tab_height <- height_cm(grobs$decor$heights)
      pmax(lab_height, tab_height)
    } else {
      lab_width <- width_cm(grobs$labels %||% unit(0, "cm"))
      tab_width <- width_cm(grobs$decor$widths)
      pmax(lab_width, tab_width)
    }
  },

  assemble_drawing = function(grobs, layout, sizes, params, elems) {
    connectors <- draw_connectors(params$key, params, elems, sizes)

    labels <- grobs$labels
    along  <- seq_along(labels)
    table  <- grobs$decor

    if (params$position %in% c("top", "bottom")) {
      width <- unit(max(width_cm(labels)), "cm")
      table$heights <- unit(sizes, "cm")
      if (!is_zero(connectors)) {
        table <- table |>
          gtable_add_grob(
            connectors, l = 1, t = 1, b = -1,
            name = "connectors", clip = "off"
          )
      }
      if (elems$text_position == "left") {
        table <- table |>
          gtable_add_cols(c(-1, 1) * width, pos = 0) |>
          gtable_add_grob(
            labels, t = along, l = 2,
            name = paste0("label-", along)
          )
      } else {
        table <- table |>
          gtable_add_cols(c(1, -1) * width, pos = -1) |>
          gtable_add_grob(
            labels, t = along, l = 2,
            name = paste0("label-", along)
          )
      }
    } else {
      height <- unit(max(height_cm(labels)), "cm")
      table$widths <- unit(sizes, "cm")
      if (!is_zero(connectors)) {
        table <- table |>
          gtable_add_grob(
            connectors, l = 1, r = -1, t = 1,
            name = "connectors", clip = "off"
          )
      }
      if (elems$text_position == "bottom") {
        table <- table |>
          gtable_add_rows(c(1, -1) * height, pos = -1) |>
          gtable_add_grob(
            labels, t = 2, l = along,
            name = paste0("label-", along),
            clip = "off"
          )
      } else {
        table <- table |>
          gtable_add_rows(c(-1, 1) * height, pos = 0) |>
          gtable_add_grob(
            labels, t = 2, l = along,
            name = paste0("label-", along),
            clip = "off"
          )
      }
    }
    table
  }
)

side_title_position <- function(element = element_text) {
  position_text(
    angle = c(90, 0, 90, 0),
    hjust = c(0, 0, 1, 1),
    vjust = 0.5,
    margin = list(
      margin(b = 5.5),# l = 2.75, r = 2.75),
      margin(l = 5.5),# t = 2.75, b = 2.75),
      margin(t = 5.5),# l = 2.75, r = 2.75),
      margin(r = 5.5)#, t = 2.75, b = 2.75)
    ),
    element = element
  )
}

draw_connectors <- function(key, params, elems, sizes) {
  if (is.null(params$connect)) {
    return(zeroGrob())
  }

  major <- switch(params$connect, perpendicular = ".row", ".col")
  minor <- setdiff(c(".row", ".col"), major)

  connect <- vec_split(key, key[[major]])
  connect <- lapply(connect$val, function(df) {
    sym <- df$.symbol
    sym[is.na(sym)] <- FALSE
    if (sum(sym) < 2) {
      return(NULL)
    }
    i <- which(df$.symbol)
    minor <- df[[minor]][i]
    i <- i[c(which.max(minor), which.min(minor))]
    vec_slice(df, i)
  })
  connect <- vec_rbind(!!!connect)

  if (params$position %in% c("top", "bottom")) {
    oppo <- sum(sizes) - cumsum(sizes) + sizes / 2
    x <- unit(connect$x, "native")
    y <- unit(oppo[connect[[".col"]]], "cm")
  } else {
    oppo <- c(0, cumsum(sizes[-length(sizes)])) + sizes / 2
    x <- unit(oppo[connect[[".row"]]], "cm")
    y <- unit(connect$y, "native")
  }

  id <- vec_unrep(connect[[major]])$times
  element_grob(elems$connector, x = x, y = y, id.lengths = id)
}
