# Constructor -------------------------------------------------------------

#' Nested axis guide
#'
#' Like regular [axis guides][ggplot2::guide_axis()], this guide is a visual
#' representation of position scales. In addition, this axis guide has extra
#' options to visualise various ranges in the scale.
#'
#' @param range_data An optional `data.frame` containing relevant data for
#'   ranges.
#' @param range_mapping An optional call to [`aes()`][ggplot2::aes] with
#'   mappings for `start`, `end`, `name` and/or `level` in the `range_data`
#'   argument.
#' @param range_start,range_end Optional vectors giving the starts and ends
#'   of ranges. Overrules any `start` or `end` aesthetic set in `range_mapping`.
#' @param range_name An optional `character` vector giving labels to use for
#'   ranges. Overrules any `name` mapping set in `range_mapping`.
#' @param range_level An optional `integer` vector indicating at what depth
#'   a range should be displayed. This is mostly useful for overruling the
#'   default bracket dodging. Overrules any `level` mapping set in
#'   `range_mapping`.
#' @param sep A `character(1)` that acts as a ['regex'][base::regex]
#'   pattern to split strings into different layers. No string splitting is
#'   performed when the `range_*` arguments are supplied.
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
#' @param handle_oob A `character(1)` describing how to deal with out-of-bounds
#'   (oob) ranges. The default `"squish"` deletes ranges where the start and
#'   endpoints are on the same side of limits, and squeezes the remainers to
#'   fit inside the limits. `"censor"` will delete ranges where any part
#'   is outside the limits. `"none"` will retain ranges regardless of limits.
#' @param mirror_margin A `logical(1)` which if `TRUE` (default), will mirror
#'   the `margin` field in the `axis.text.{x/y}.{position}` theme setting. If
#'   `FALSE`, margins are taken as-is. This is an option because the default
#'   margins may sometimes be 0 between text and a bracket, which may look
#'   uncomfortable.
#' @param extend_discrete A `numeric(1)` giving how much brackets should be
#'   extended beyond the tick mark in discrete scales. Should be less than
#'   `0.5` for adjacent ranges to not touch.
#' @param drop_zero A `logical(1)` determining whether ranges with no
#'   difference, up to some tolerance, in start and end point should be drawn.
#'   If `TRUE` (default), such ranges don't get brackets, and if `FALSE`, such
#'   ranges do get brackets. Useful to set to `FALSE` when brackets should be
#'   drawn for single breaks in discrete scales. Note that `extend_discrete`
#'   has no bearing on `drop_zero`.
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
#' # by setting the `range_level` argument.
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
  range_data      = NULL,
  range_mapping   = NULL,
  range_start     = NULL,
  range_end       = NULL,
  range_name      = NULL,
  range_level     = NULL,
  sep             = "[^[:alnum:]]+",
  bracket         = "line",
  bracket_size    = unit(2, "mm"),
  bracket_theme   = element_line(),
  deep_text       = element_text(),
  handle_oob      = "squish",
  mirror_margin   = TRUE,
  extend_discrete = 0.4,
  drop_zero       = TRUE,
  ...
) {
  mapped <- FALSE
  ranges <- NULL
  if (!is.null(range_data) && !is.null(range_mapping)) {
    mapped <- TRUE
    ranges <- eval_aes(
      range_data, range_mapping,
      c("start", "end", "name", "level")
    )
  }
  ranges <- data_frame0(
    start  = range_start %||% ranges$start,
    end    = range_end   %||% ranges$end,
    .label = range_name  %||% ranges$name,
    .level = range_level %||% ranges$level
  )
  if (nrow(ranges) > 0L) {
    if (!all(c("start", "end") %in% names(ranges))) {
      missing <- setdiff(c("start", "end"), names(ranges))
      param <- "aesthetic: {.field "
      if (!mapped) {
        missing <- paste0("range_", missing)
        param <- "argument: {.arg "
      }
      cli::cli_abort(
        paste0("Missing required ", param, "{missing}}.")
      )
    }
  }
  if (!is.null(ranges$.level)) {
    ranges$.level <- match(ranges$.level, sort(unique(ranges$.level)))
  }

  bracket <- validate_bracket(bracket)

  guide_axis_extend(
    sep             = sep,
    ranges          = ranges,
    bracket         = bracket,
    bracket_size    = bracket_size,
    bracket_theme   = bracket_theme,
    deep_text       = deep_text,
    handle_oob      = handle_oob,
    mirror_margin   = mirror_margin,
    extend_discrete = extend_discrete,
    drop_zero       = drop_zero,
    ...,
    super = GuideAxisNested
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideAxisNested <- ggproto(
  "GuideAxisNested", GuideAxisExtend,

  params = c(
    GuideAxisExtend$params,
    list(
      sep = "^[[:alphanum:]]+",
      ranges  = data_frame(),
      bracket = matrix(ncol = 2),
      bracket_size   = unit(2, "mm"),
      bracket_theme  = element_line(),
      deep_text      = element_text(),
      handle_oob     = "squish",
      mirror_margin  = TRUE,
      extend_discrete = 0.4,
      drop_zero      = TRUE
    )
  ),

  extract_params = function(scale, params, hashables, ...) {

    aes <- params$aesthetic
    ranges <- params$ranges

    # If no manual ranges were provided, infer from labels
    if (prod(dim(ranges)) == 0) {
      ranges <- ranges_from_labels(params$key, params$sep, aes,
                                   "guide_axis_nested")
      params$key <- ranges$key
      ranges <- ranges$ranges
    }
    if (is.null(ranges$.level)) {
      ranges <- disjoin_ranges(ranges)
    }

    # Assess if input is discrete before scale transform
    discrete_start <- -1 * is_discrete(ranges$start)
    discrete_end   <-  1 * is_discrete(ranges$end)

    # User input is expected on original scale, so transform input
    ranges$start <- scale_transform(ranges$start, scale)
    ranges$end   <- scale_transform(ranges$end,   scale)

    # Sort ranges
    ranges[, c("start", "end")] <- list(
      start = pmin(ranges$start, ranges$end),
      end   = pmax(ranges$start, ranges$end)
    )

    # Omit drawing labels that have ranges that are too small
    ranges$.draw <- TRUE
    if (isTRUE(params$drop_zero)) {
      ranges$.draw <- (ranges$end - ranges$start) > 1000 * .Machine$double.eps
    }

    extend <- params$extend_discrete
    if (scale$is_discrete() && !is.null(extend)) {
      ranges$start <- ranges$start + extend * discrete_start
      ranges$end   <- ranges$end   + extend * discrete_end
    }

    ranges <- switch(
      arg_match0(params$handle_oob, c("squish", "censor", "none")),
      "squish" = range_squish(ranges, scale),
      "censor" = range_censor(ranges, scale),
      "none"   = ranges
    )

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

    if (params$mirror_margin) {
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

    elements$deep_text <- combine_element_list(
      params$deep_text, elements$text, .text_or_blank
    )
    elements$bracket <- combine_element_list(
      params$bracket_theme, elements$ticks, .line_or_blank
    )

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

range_squish <- function(ranges, scale) {
  limits <- sort(scale$continuous_range)
  start  <- ranges$start
  end    <- ranges$end
  oob_start <- start < limits[1] | start > limits[2]
  oob_end   <- end   < limits[1] | end   > limits[2]
  keep <- !oob_start | !oob_end | (start < limits[1] & end > limits[2])
  ranges <- vec_slice(ranges, keep)
  ranges$start <- pmin(pmax(ranges$start, limits[1]), limits[2])
  ranges$end   <- pmin(pmax(ranges$end,   limits[1]), limits[2])
  ranges
}

range_censor <- function(ranges, scale) {
  limits    <- sort(scale$continuous_range)
  oob_start <- ranges$start < limits[1] | ranges$start > limits[2]
  oob_end   <- ranges$end   < limits[1] | ranges$end   > limits[2]
  keep <- !oob_start & !oob_end
  vec_slice(ranges, keep)
}

ranges_from_labels <- function(key, sep = "[^[:alnum:]]+", aes,
                               fun_nm, rev = FALSE) {
  # Split labels
  labels <- strsplit(key$.label, sep)

  # Pad label vectors if necessary
  lengths <- lengths(labels)
  max_len <- max(lengths)
  if (!all(lengths == max_len)) {
    cli::cli_warn(c(
      "Not all {.field labels} in {.fn {fun_nm}} can be split into equal lengths.",
      i = "Is \"{sep}\" the correct {.arg sep} argument?"
    ))
    labels <- lapply(labels, function(el) {
      c(el, rep(NA, max_len - length(el)))
    })
  }
  labels <- do.call(rbind, labels)
  if (isTRUE(rev)) {
    labels <- labels[, rev(seq_len(ncol(labels))), ncol = FALSE]
  }

  # Set key label to the first one
  key$.label <- labels[, 1]
  labels <- labels[, -1, drop = FALSE]

  # Early exit if there are no ranges to infer
  if (prod(dim(labels)) == 0) {
    return(list(key = key, ranges = data_frame0()))
  }

  # Run-length encode labels to get ranges
  value  <- key[[aes]]
  ranges <- apply(labels, 2, function(labs) {
    rle   <- new_rle(labs)
    start <- rle_start(rle)
    data_frame0(
      start  = value[start],
      end    = value[rle_end(rle)],
      .label = labs[start]
    )
  })
  nrows  <- list_sizes(ranges)

  # Combine
  ranges <- vec_c(!!!ranges)

  # Assign levels
  ranges$.level <- rep.int(seq_along(nrows), nrows)

  # Delete padded ranges
  ranges <- vec_slice(ranges, !is.na(ranges$.label))

  list(key = key, ranges = ranges)
}
