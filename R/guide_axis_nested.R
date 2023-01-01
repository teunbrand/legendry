
#' Nested axis guide
#'
#' Like regular [axis guides][ggplot2::guide_axis()], this guide is a visual
#' representation of position scales. In addition, this axis guide has extra
#' options to visualise various ranges in the scale.
#'
#' @param sep A `character(1)` that acts as a ['regex'][base::regex]
#'   pattern to split strings into different layers. No string splitting is
#'   performed when the `range_*` arguments are supplied.
#' @param range_start A `numeric` vector indicating the start of ranges.
#' @param range_end A `numeric` vector indicating the end of ranges.
#' @param range_name A `character` vector giving labels to use for ranges.
#' @param range_depth (Optional) an `integer` vector indicating at what depth
#'   a range should be displayed. This is mostly useful for overruling the
#'   default bracket dodging.
#' @param bracket One of the following: \itemize{
#'   \item{A `<matrix[n, 2]>` giving point coordinates for bracket shapes, such
#'   as returned from [bracket functions][bracket_options].}
#'   \item{A `character(1)` giving one of the
#'   [bracket functions][bracket_options] as a string. The current options are
#'   `"sigmoid"`, `"atan"`, `"curvy"`, `"line"` (default), `"round"`,
#'   `"chevron"` or `"square"`.}}
#' @param bracket_size A [`<unit>`][grid::unit()] that sets the size of a
#'   bracket in the direction orthogonal to the axis.
#' @param bracket_theme An [`<element_line>`][ggplot2::element_line()] object
#'   or `<element_blank>` object controlling the appearance of the brackets. By
#'   default, it inherits from the `axis.ticks.{x/y}.{position}` theme setting.
#' @param deep_text An [`<element_text>`][ggplot2::element_text()] object or
#'   `<element_blank>` object. Alternatively, a list of such elements, where
#'   every item applies to a layer of text that are not the regular labels. See
#'   also [`elements_text()`] to easily construct a list of text elements.
#' @param mirror_padding A `logical(1)` which if `TRUE` (default), will mirror
#'   the `margin` field in the `axis.text.{x/y}.{position}` theme setting. If
#'   `FALSE`, margins are taken as-is. This is an option because the default
#'   margins may sometimes be 0 between text and a bracket, which may look
#'   uncomfortable.
#' @inheritDotParams guide_axis_extend
#'
#' @inherit guide_axis_extend return
#' @export
#' @family axis variants
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(class, displ)) +
#'   geom_boxplot()
#'
#' # If no `range_*` argument is given, the guide tries to split the labels
#' p + aes(x = interaction(cyl, year)) +
#'   guides(x = "axis_nested")
#'
#' # For discrete axes, you can select the ranges either by numerical
#' # equivalent, or by break level.
#' p + guides(x = guide_axis_nested(
#'   range_start = c("2seater", "pickup"),
#'   range_end   = c(4, 7),
#'   range_name  = c("First range", "Second range")
#' ))
#'
#' # If ranges overlap, they are automatically dodged. You can override this
#' # by setting the `range_depth` argument.
#' p + guides(x = guide_axis_nested(
#'   range_start = c(1, 3),
#'   range_end   = c(4, 6),
#'   range_name  = c("First range", "Second range")
#' ))
#'
#' # You can change the appearance of the brackets. See ?bracket_options for
#' # ready-to-go bracket functions.
#' p + aes(x = interaction(cyl, year)) +
#'   guides(x = guide_axis_nested(
#'     bracket       = "curvy",
#'     bracket_size  = unit(5, "mm"),
#'     bracket_theme = element_line(colour = "forestgreen"),
#'     deep_text     = element_text(colour = "tomato")
#'   ))
guide_axis_nested <- function(
  sep            = "[^[:alnum:]]+",
  range_start    = NULL,
  range_end      = NULL,
  range_name     = NULL,
  range_depth    = NULL,
  bracket        = "line",
  bracket_size   = unit(2, "mm"),
  bracket_theme  = element_line(),
  deep_text      = element_text(),
  mirror_padding = TRUE,
  ...
) {

  ranges <- data_frame0(
    start  = range_start,
    end    = range_end,
    .label = range_name,
    .level = range_depth
  )
  bracket <- validate_bracket(bracket)

  guide_axis_extend(
    sep            = sep,
    ranges         = ranges,
    bracket        = bracket,
    bracket_size   = bracket_size,
    bracket_theme  = bracket_theme,
    deep_text      = deep_text,
    mirror_padding = mirror_padding,
    ...,
    super          = GuideAxisNested
  )
}

# Class -------------------------------------------------------------------

GuideAxisNested <- ggproto(
  "GuideAxisNested", GuideAxisExtend,

  params = c(
    GuideAxisExtend$params,
    list(
      sep = "^[[:alphanum:]]+",
      ranges = data_frame(),
      bracket = matrix(ncol = 2),
      bracket_size  = unit(2, "mm"),
      bracket_theme = element_line(),
      deep_text     = element_text(),
      mirror_padding = TRUE
    )
  ),

  extract_params = function(scale, params, hashables, ...) {

    aes <- params$aesthetic
    ranges <- params$ranges

    if (prod(dim(ranges)) == 0) {
      # Derive ranges from key
      key <- params$key
      # Split labels
      labels <- strsplit(key$.label, params$sep)
      # Pad labels with empty strings
      labels <- pad_list(labels, padding = "")
      labels <- do.call(rbind, labels)
      # Replace axis labels with first label
      key$.label <- labels[, 1]
      params$key <- key
      labels <- labels[, -1, drop = FALSE]

      if (prod(dim(labels)) == 0) {
        ranges <- data_frame0()
      } else {
        # Run-length encode labels to get ranges
        value  <- key[[aes]]
        ranges <- apply(labels, 2, function(labs) {
          rle   <- new_rle(labs)
          start <- rle_start(rle)
          data_frame0(
            start = value[start],
            end   = value[rle_end(rle)],
            .label = labs[start]
          )
        })
        # Combine and assign levels
        nrows  <- vapply(ranges, nrow, integer(1))
        ranges <- vec_c(!!!ranges)
        ranges$.level <- rep(seq_along(nrows), each = nrows)
      }
    }
    if (is.null(ranges$.level)) {
      ranges <- disjoin_ranges(ranges)
    }
    colnames(ranges)[1:2] <- c(aes, paste0(aes, "end"))

    params$ranges <- ranges
    params$name <- paste0(params$name, "_", params$aesthetic)
    Guide$extract_params(scale, params, hashables)
  },

  transform = function(self, params, coord, panel_params) {
    params <- GuideAxisExtend$transform(params, coord, panel_params)
    params$ranges <- coord$transform(params$ranges, panel_params)
    params
  },

  override_elements = function(params, elements, theme) {

    elements <- GuideAxisExtend$override_elements(params, elements, theme)

    if (params$mirror_padding) {
      margin <- elements$text$margin
      switch(
        params$position,
        top    = {margin[1] <- margin[3]},
        bottom = {margin[3] <- margin[1]},
        right  = {margin[2] <- margin[4]},
        left   = {margin[4] <- margin[2]}
      )
      elements$text$margin <- margin
    }
    deep_text <- params$deep_text
    if (inherits(deep_text, .text_or_blank)) {
      deep_text <- list(deep_text)
    }
    deep_text <- lapply(deep_text, function(elem) {
      combine_elements(elem, elements$text)
    })
    elements$deep_text <- deep_text
    elements$bracket <- combine_elements(params$bracket_theme, elements$ticks)
    elements

  },

  build_labels = function(key, elements, params) {

    ranges <- params$ranges

    # Build first layer
    first_layer <- GuideAxisExtend$build_labels(key, elements, params)

    # Build subsequent layers
    aes <- params$aes
    end <- paste0(aes, "end")
    params$n.dodge <- 1L
    if (params$vertical) {
      just <- elements$text$vjust
    } else {
      just <- elements$text$hjust
    }
    new_key <- ranges
    new_key <- unname(split(new_key, new_key$.level))

    deep_text <- elements$deep_text
    deep_text <- rep_len(deep_text, length(new_key))

    next_layers <- Map(
      function(key, elem) {
        dummy_elem <- elements
        dummy_elem$text <- elem
        just <- if (params$vertical) elem$vjust else elem$hjust
        key[[aes]] <- key[[aes]] + (key[[end]] - key[[aes]]) * just
        GuideAxisExtend$build_labels(key, dummy_elem, params)[[1]]
      },
      key  = new_key,
      elem = deep_text,
      USE.NAMES = FALSE
    )

    # Build brackets
    brackets <- expand_bracket(
      ranges, params$bracket, params$position,
      params$bracket_size, elements$bracket
    )
    next_layers <- vec_interleave(brackets, next_layers)

    c(first_layer, next_layers)
  }
)

# Helpers -----------------------------------------------------------------

pad_list <- function(list, padding = "") {
  lengths <- lengths(list)
  max_len <- max(lengths)
  if (all(lengths == max_len)) {
    return(list)
  }
  lapply(list, function(el) {
    c(el, rep("", max_len - length(el)))
  })
}

# This is conceptually similar to IRanges::disjointBins
disjoin_ranges <- function(ranges) {
  if (nrow(ranges) < 2) {
    ranges$.level <- rep(1L, nrow(ranges))
    return(ranges)
  }

  # Sort and extract
  ranges <- ranges[order(ranges$start, ranges$end), , drop = FALSE]
  starts <- ranges$start
  ends   <- ranges$end

  # Initialise first range
  track_end <- ends[1]
  bin <- rep(NA_integer_, nrow(ranges))
  bin[1] <- 1L

  # Find bins
  for (range_id in tail(seq_row(ranges), -1)) {
    candidate <- which(track_end < starts[range_id])
    if (length(candidate) > 0) {
      ans <- candidate[1]
      # If bin is available, update bin
      track_end[ans] <- ends[range_id]
    } else {
      # Make new bin
      track_end <- c(track_end, ends[range_id])
      ans <- length(track_end)
    }
    bin[range_id] <- ans
  }

  ranges$.level <- bin
  ranges
}
