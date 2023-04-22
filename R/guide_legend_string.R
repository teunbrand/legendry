# Constructor -------------------------------------------------------------

#' String legend
#'
#' This type of legend shows colour and fill mappings as coloured text. It
#' does not draw keys as [`guide_legend()`][ggplot2::guide_legend()] does.
#'
#' @inheritParams ggplot2::guide_legend
#' @param family,face,size Shortcuts for setting the `label.theme` fields for
#'   the family, face and size in [`element_text()`][ggplot2::element_text].
#'
#' @return A `<Guide>` ggproto object that can be given to the
#'   [`guides()`][ggplot2::guides()] function, or set as the `guide` argument
#'   in a colour or fill scale.
#' @family legend variants
#' @export
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy, colour = manufacturer)) +
#'   geom_point()
#'
#' # This guide gives the mapping as coloured labels
#' p + guides(colour = "legend_string")
#'
#' # The controls are the same as in `guide_legend`
#' p + guides(colour = guide_legend_string(
#'   ncol = 2, face = "bold", byrow = TRUE
#' ))
guide_legend_string <- function(
    # Title
  title          = waiver(),
  title.position = NULL,
  title.theme    = NULL,
  title.hjust    = NULL,
  title.vjust    = NULL,

  # Label
  label.theme    = NULL,
  label.hjust    = NULL,
  label.vjust    = NULL,
  family         = NULL,
  face           = NULL,
  size           = NULL,

  # General
  direction      = NULL,
  override.aes   = list(),
  nrow           = NULL,
  ncol           = NULL,
  byrow          = FALSE,
  reverse        = FALSE,
  order          = 0,
  ...
) {
  title.position <- arg_null_or_match(title.position, .trbl)

  title.theme <- title.theme %||% element_text(inherit.blank = TRUE)
  title.theme <- arg_class(title.theme, .text_or_blank)
  title.theme <- update_element(
    title.theme,
    hjust = title.hjust,
    vjust = title.vjust
  )

  label.theme <- label.theme %||% element_text(inherit.blank = TRUE)
  label.theme <- arg_class(label.theme, .text_or_blank)
  label.theme <- update_element(
    label.theme,
    hjust  = label.hjust,
    vjust  = label.vjust,
    family = family,
    face   = face,
    size   = size
  )

  new_guide(
    # Title and labels
    title = title,
    title.position = title.position,
    title.theme = title.theme,
    label.theme = label.theme,

    # General
    direction    = direction,
    override.aes = rename_aes(override.aes),
    nrow    = nrow,
    ncol    = ncol,
    byrow   = byrow,
    reverse = reverse,
    order   = order,

    # Fixed parameters
    available_aes = c("colour", "fill"),
    name  = "legend",
    super = GuideLegendString
  )
}

# Class -------------------------------------------------------------------

GuideLegendString <- ggproto(
  "GuideLegendString", GuideLegend,

  params = list(
    # Titles and labels
    title          = waiver(),
    title.position = NULL,
    title.theme    = NULL,
    label          = TRUE,
    label.theme    = NULL,
    label.position = "right",

    # General
    direction    = NULL,
    override.aes = list(),
    nrow         = NULL,
    ncol         = NULL,
    byrow        = FALSE,
    reverse      = FALSE,
    name         = "legend",
    hash         = character(),
    position     = NULL,
    direction    = NULL,
    order        = 0
  ),

  geom = function(params, layers, default_mapping) {
    GuideColourbar$geom(params, layers, default_mapping)
  },

  build_decor = function(decor, grobs, elements, params) {
    NULL
  },

  build_labels = function(key, elements, params) {
    Map(
      function(lab, colour) {
        ggname(
          "guide.label",
          element_grob(
            elements$text,
            label = lab,
            margin_x = TRUE,
            margin_y = TRUE,
            colour = colour
          )
        )
      },
      lab    = key$.label,
      colour = key$colour %||% key$fill
    )
  },

  measure_grobs = function(grobs, params, elements) {
    byrow    <- params$byrow    %||% FALSE
    n_breaks <- params$n_breaks %||% 1L
    dim      <- c(params$nrow %||% 1L, params$ncol %||% 1L)

    # Measure label sizes
    zeroes <- rep(0, prod(dim) - n_breaks)
    label_widths <- apply(matrix(
      c(width_cm(grobs$labels), zeroes),
      nrow = dim[1], ncol = dim[2], byrow = byrow
    ), 2, max)
    label_heights <- apply(matrix(
      c(height_cm(grobs$labels), zeroes),
      nrow = dim[1], ncol = dim[2], byrow = byrow
    ), 1, max)

    widths  <- head(vec_interleave(label_widths,  elements$hgap), -1)
    heights <- head(vec_interleave(label_heights, elements$vgap), -1)

    title_width  <- width_cm(grobs$title)
    title_height <- height_cm(grobs$title)

    widths <- switch(
      params$title.position,
      "left"  = c(title_width, elements$hgap, widths),
      "right" = c(widths, elements$hgap, title_width),
      c(widths, max(0, title_width - sum(widths)))
    )
    heights <- switch(
      params$title.position,
      "top"    = c(title_height, elements$vgap, heights),
      "bottom" = c(heights, elements$vgap, title_height),
      c(heights, max(0, title_height - sum(heights)))
    )

    list(
      widths  = widths,
      heights = heights,
      padding = elements$padding
    )
  },

  arrange_layout = function(key, sizes, params) {
    break_seq <- seq_len(params$n_breaks %||% 1L)
    dim <- c(params$nrow %||% 1L, params$ncol %||% 1L)

    if (params$byrow %||% FALSE) {
      df <- data_frame0(
        R = ceiling(break_seq / dim[2]),
        C = (break_seq - 1) %% dim[2] + 1
      )
    } else {
      df <- arrayInd(break_seq, dim)
      df <- data_frame0(R = df[, 1], C = df[, 2])
    }


    label_row <- df$R * 2
    label_col <- df$C * 2

    switch(
      params$title.position,
      "top" = {
        label_row <- label_row + 2
        title_row <- 2
        title_col <- seq_along(sizes$widths) + 1
      },
      "bottom" = {
        title_row <- length(sizes$heights)   + 1
        title_col <- seq_along(sizes$widths) + 1
      },
      "left" = {
        label_col <- label_col + 2
        title_row <- seq_along(sizes$heights) + 1
        title_col <- 2
      },
      "right" = {
        title_row <- seq_along(sizes$heights) + 1
        title_col <- length(sizes$widths)     + 1
      }
    )

    df <- cbind(df, label_row, label_col)
    list(layout = df, title_row = title_row, title_col = title_col)
  }
)
