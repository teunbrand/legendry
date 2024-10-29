# Range keys --------------------------------------------------------------

#' Range keys
#'
#' @description
#' These functions are helper functions for working with ranged data as keys
#' in guides. They all share the goal creating of a guide key, but have
#' different methods:
#'
#' * `key_range_auto()` is a function factory whose functions make an attempt
#'   to infer ranges from the scale's labels.
#' * `key_range_manual()` uses user-provided vectors to set ranges.
#' * `key_range_map()` makes mappings from a `<data.frame>` to set ranges.
#'
#' @param sep A `<character[1]>` giving a [regular expression][base::regex] to
#'   use for splitting labels provided by the scale using
#'   [`strsplit()`][base::strsplit]. Defaults to splitting on any
#'   non-alphanumeric character.
#' @param reverse A `<logical[1]>` which if `FALSE` (default) treats the first
#'   labels as the inner labels and the last labels as the outer labels.
#'   If `TRUE`, thee first labels are treated as the outer labels and the last
#'   labels are treated as the inner labels.
#' @param start,end A vector that can be interpreted by the scale, giving the
#'   start and end positions of each range respectively.
#' @param name A `<character>` or list of expressions
#' @param level An `<integer>` giving the depth of each range to avoid overlaps
#'   between different ranges. When `level` is smaller than 1, no brackets are
#'   drawn.
#' @param data A `<data.frame>` or similar object coerced by
#'   [`fortify()`][ggplot2::fortify] to a `<data.frame>`, in which the `mapping`
#'   argument is evaluated.
#' @param ... [`<data-masking>`][rlang::topic-data-mask] A set of mappings
#'   similar to those provided to [`aes()`][ggplot2::aes], which will be
#'   evaluated in the `data` argument.
#'   For `key_range_map()`, these *must* contain `start` and `end` mappings.
#'   Can contain additional parameters for text styling, namely `colour`,
#'   `family`, `face`, `size`, `hjust`, `vjust`, `angle` and `lineheight`.
#' @param .call A [call][rlang::topic-error-call] to display in messages.
#'
#' @details
#' The `level` variable is optional and when missing, the guides use an
#' algorithm similar to `IRanges::disjointBins()` to avoid overlaps.
#'
#' The `key_range_auto()` does *not* work with expression labels.
#'
#' @name key_range
#' @family keys
#' @return
#' For `key_range_auto()` a function. For `key_range_manual()` and
#' `key_range_map()` a `<data.frame>` with the `<key_range>` class.
#'
#' @examples
#' # Example scale
#' template <- scale_x_discrete(limits = c("A 1", "B 1", "C&1", "D&2", "E&2"))
#'
#' # By default, splits on all non-alphanumeric characters
#' auto <- key_range_auto()
#' auto(template)
#'
#' # Only split on a specific character
#' auto <- key_range_auto(sep = "&")
#' auto(template)
#'
#' # Treating the letters as outer labels and numbers as inner labels
#' auto <- key_range_auto(reverse = TRUE)
#' auto(template)
#'
#' # Providing custom values
#' key_range_manual(
#'   start = c(1, 5,  10),
#'   end   = c(4, 15, 11),
#'   level = c(0, 2, 1),
#'   name  = c("A", "B", "C")
#' )
#'
#' # Values from a <data.frame>
#' key_range_map(presidential, start = start, end = end, name = name)
NULL

#' @rdname key_range
#' @export
key_range_auto <- function(sep = "[^[:alnum:]]+", reverse = FALSE, ...) {
  check_string(sep)
  check_bool(reverse)
  force(sep)
  force(reverse)
  dots <- label_args(...)
  call <- current_call()
  function(scale, aesthetic = NULL) {
    range_from_label(
      scale = scale, aesthetic = aesthetic,
      sep = sep, reverse = reverse, extra_args = dots,
      call = call
    )
  }
}

#' @rdname key_range
#' @export
key_range_manual <- function(start, end, name = NULL, level = NULL, ...) {
  df <- data_frame0(
    start = start, end = end, .label = name, .level = level,
    !!!label_args(...)
  )
  check_columns(df, c("start", "end"))
  class(df) <- c("key_range", "key_guide", class(df))
  df
}

#' @rdname key_range
#' @export
key_range_map <- function(data, ..., .call = caller_env()) {
  mapping <- enquos(...)
  mapping <- Filter(Negate(quo_is_missing), mapping)
  mapping <- new_aes(mapping, env = .call)

  df <- eval_aes(
    data, mapping,
    required = c("start", "end"),
    optional = c("name", "level", .label_params),
    call = .call, arg_mapping = "mapping", arg_data = "data"
  )

  df <- rename(
    df, c("name", "level", .label_params),
    c(".label", ".level", paste0(".", .label_params))
  )
  df$colour <- df$color
  df$color <- NULL
  class(df) <- c("key_range", "key_guide", class(df))
  df
}

key_range_rle <- function(x, ...) {
  rle <- vec_unrep(x)
  end <- cumsum(rle$times) + 0.5
  start <- end - rle$times
  key_range_manual(start, end, name = rle$key, level = 1L, ...)
}

# Extractor ---------------------------------------------------------------

range_extract_key <- function(
  scale, aesthetic, key,
  drop_zero = TRUE, pad_discrete = 0,  oob = "squish",
  ...
) {
  if (is.function(key)) {
    key <- key(scale, aesthetic)
  }
  if (is.null(key$.level)) {
    key <- disjoin_ranges(key)
  }

  # Mark discrete variables separately for start and end
  disc_start <- -1 * is_discrete(key$start)
  disc_end   <- +1 * is_discrete(key$end)

  map <- aesthetic %in% c("x", "y")
  key$start <- scale_transform(key$start, scale, map = map, "start")
  key$end   <- scale_transform(key$end,   scale, map = map, "end")

  # Sort starts and ends
  key[c("start", "end")] <- list(
    start = pmin(key$start, key$end),
    end   = pmax(key$start, key$end)
  )

  # Mark ranges where no brackets should be drawn
  key$.draw <- TRUE
  if (!isFALSE(drop_zero)) {
    key$.draw <- abs(key$end - key$start) > sqrt(.Machine$double.eps)
  }
  key$.draw <- key$.draw & key$.level > 0

  # Apply padding for discrete variables
  extend <- pad_discrete
  if (scale$is_discrete() && !is.null(extend)) {
    key$start <- key$start + extend * disc_start
    key$end   <- key$end   + extend * disc_end
  }

  # Apply out-of-bounds rules
  limits <- scale$continuous_range %||% scale$get_limits()
  range_oob(key, oob, limits)
}


# Helpers -----------------------------------------------------------------

## Out-of-bounds utilities ------------------------------------------------

range_oob <- function(ranges, method, limits) {
  limits <- sort(limits)
  ranges <- switch(
    method,
    "squish" = range_squish(ranges, limits),
    "censor" = range_censor(ranges, limits),
    ranges
  )
  vec_slice(ranges, !is.na(ranges$.draw))
}

range_squish <- function(ranges, limits) {
  start  <- ranges$start
  end    <- ranges$end
  oob_start <- is_oob(start, limits)
  oob_end   <- is_oob(end,   limits)
  keep <- !oob_start | !oob_end | (start < limits[1] & end > limits[2])
  ranges$.draw[!keep] <- NA
  ranges$start <- pmin(pmax(ranges$start, limits[1]), limits[2])
  ranges$end   <- pmin(pmax(ranges$end,   limits[1]), limits[2])
  ranges
}

range_censor <- function(ranges, limits) {
  oob_start <- is_oob(ranges$start, limits)
  oob_end   <- is_oob(ranges$end, limits)
  keep      <- !oob_start & !oob_end
  ranges$.draw[!keep] <- NA
  ranges
}

## Other helpers ----------------------------------------------------------

range_from_label <- function(
  scale, aesthetic = NULL, sep =  "[^[:alnum:]]+", reverse = FALSE,
  extra_args = list(), call = caller_env()
) {
  # Extract a standard key from the scale
  aesthetic <- aesthetic %||% scale$aesthetics[1]
  key <- Guide$extract_key(scale, aesthetic)

  # Reject expressions, as we cannot split these
  if (!is.character(key$.label)) {
    type <- obj_type_friendly(key$.label)
    cli::cli_abort(
      c("Cannot split the guide's {.field label}.",
        i = "It must be a {.cls character} vector, not {type}."),
      call = call
    )
  }

  # Split labels
  labels <- strsplit(key$.label, sep)

  # Pad label vectors if necessary
  lengths <- lengths(labels)
  max_len <- max(lengths)
  if (any(lengths != max_len)) {
    cli::cli_warn(c(
      paste0("Not all {.field labels} in {.fn key_range_auto} can be split ",
             "into equal lengths."),
      i = "Is \"{sep}\" the correct {.arg sep} argument?"
    ))
    labels <- lapply(labels, pad, length = max_len)
  }
  labels <- do.call(rbind, labels)

  if (isTRUE(reverse)) {
    labels <- labels[, rev(seq_len(ncol(labels)))]
  }

  key$.label <- labels[, 1, drop = TRUE]
  labels <- labels[, -1, drop = FALSE]

  # Set first series of unbracketed labels
  value  <- key[[1]]
  key <- data_frame0(
    start = value, end = value, .label = key$.label, .level = 0
  )
  if (is_empty(labels)) {
    return(data_frame0(key, !!!extra_args))
  }
  ranges <- apply(labels, 2, function(labs) {
    rle   <- vec_unrep(labs)
    start <- cumsum(rle$times) - rle$times + 1L
    data_frame0(
      start = value[start],
      end   = value[cumsum(rle$times)],
      .label = labs[start]
    )
  })
  nrows  <- list_sizes(ranges)
  ranges <- vec_c(!!!ranges)
  ranges$.level <- rep.int(seq_along(nrows), nrows)
  range <- vec_slice(ranges, !is.na(ranges$.label))
  df <- vec_rbind(key, range)
  df <- data_frame0(df, !!!extra_args)
  class(df) <- c("key_range", "key_guide", class(df))
  df
}

justify_ranges <- function(key, levels, element, level_elements) {

  if (is_blank(element)) {
    return(key)
  }

  ends <- intersect(c("thetaend", "xend", "yend"), names(key))
  if (length(ends) < 1) {
    return(key)
  }
  starts <- gsub("end$", "", ends[1])

  just_name <- switch(ends[1], yend = "vjust", "hjust")
  just <- element[[just_name]] %||% 0.5

  if (!is.null(level_elements)) {
    just <- map_dbl(level_elements, function(x) x[[just_name]] %||% just)
    just <- just[match(key$.level, levels)]
  }

  key[[starts]] <- switch(
    ends[1],
    thetaend = justify_range(key$theta, key$thetaend, just, theta = TRUE),
    xend     = justify_range(key$x, key$xend, just),
    yend     = justify_range(key$y, key$yend, just)
  )

  key
}

justify_range <- function(start, end, just, theta = FALSE) {
  if (theta) {
    add <- end < start
    end[add] <- end[add] + 2 * pi
  }
  (end - start) * just + start
}

disjoin_ranges <- function(ranges) {

  n_ranges <- nrow(ranges)
  if (n_ranges < 2) {
    ranges$.level <- rep(1L, nrow(ranges))
    return(ranges)
  }

  # Sort and extract
  ranges <- ranges[order(ranges$start, ranges$end), , drop = FALSE]
  ranges <- vec_slice(ranges, order(ranges$start, ranges$end))
  starts <- ranges$start
  ends   <- ranges$end

  # Initialise first range
  end_tracker <- ends[1]
  bin <- rep(NA_integer_, nrow(ranges))
  bin[1] <- 1L

  # Find bins
  for (range_id in seq_len(n_ranges)[-1]) {
    candidate <- which(end_tracker < starts[range_id])
    if (length(candidate) > 0) {
      # If there is room in this bin, update this bin
      ans <- candidate[1]
      end_tracker[ans] <- ends[range_id]
    } else {
      # Register new bin
      end_tracker <- c(end_tracker, ends[range_id])
      ans <- length(end_tracker)
    }
    bin[range_id] <- ans
  }

  ranges$.level <- bin
  ranges
}

extract_range_params <- function(scale, params, ...) {
  params$position <- params$position %|W|% NULL
  params$limits   <- scale$get_limits()

  new_names <- c("start", "end")
  aesthetic <- params$aesthetic
  if (aesthetic %in% c("x", "y")) {
    new_names  <- paste0(aesthetic, c("", "end"))
  } else if (is_theta(params$position)) {
    new_names <- c("x", "xend")
  }
  params$key <- rename(params$key, c("start", "end"), new_names)
  params
}

setup_range_params <- function(params) {
  if (params$aesthetic %in% c("x", "y")) {
    # parameters are already transformed
    return(params)
  }

  limits   <- params$limits %||% c(0, 1)
  other    <- switch(params$position, bottom = , left = 1, 0)
  position <- params$position

  if (!is_empty(params$key)) {
    key <- params$key

    start <- guide_rescale(key$start, limits)
    end   <- guide_rescale(key$end, limits)

    key <- switch(
      params$direction,
      horizontal = replace_null(key, x = start, xend = end, y = other),
      vertical   = replace_null(key, y = start, yend = end, x = other),
      key
    )

    params$key <- key
  }

  if (!is_empty(params$decor)) {
    decor <- params$decor
    value <- guide_rescale(decor[[params$aesthetic]], limits)
    decor <- switch(
      params$direction,
      horizontal = replace_null(decor, x = value, y = other),
      vertical   = replace_null(decor, y = value, x = other),
      decor
    )

    params$decor <- decor
  }

  params
}
