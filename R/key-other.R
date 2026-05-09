# Speciality keys ----------------------------------------------------------

#' Speciality keys
#'
#' @description
#' These functions are helper functions for working with keys in guides. The
#' functions described here are not widely applicable and may only apply
#' to a small subset of guides. As such, it is fine to adjust the arguments
#' of a speciality key, but swapping types is ill-advised.
#'
#' * `key_sequence()` is a function factory whose functions create a regularly
#'   spaced sequence between the limits of a scale. It is used in colour bar
#'   guides.
#' * `key_bins()` is a function factory whose function create a binned key
#'   given the breaks in the scale. It is used in colour steps guides.
#' * `key_upset()` is a function factory whose function creates an upset key
#'   from splitting the breaks in the scale. It is used in the upset guide.
#' * `key_symbols()` is a function factory whose function creates a key
#'   from the literal provided values. It is used in the symbols guide.
#'
#' @param n A positive `<integer[1]>` giving the number of colours to use for a
#'   gradient.
#' @param even.steps A `<logical[1]>` indicating whether the size of bins
#'   should be displayed as equal (`TRUE`) or proportional to their length in
#'   data space (`FALSE`).
#' @param show.limits A `<logical[1]>` stating whether the limits of the scale
#'   should be shown with labels and ticks (`TRUE`) or remain hidden (`FALSE`).
#'   Note that breaks coinciding with limits are shown regardless of this
#'   setting. The default, `NULL`, consults the scale's `show.limits` setting
#'   or defaults to `FALSE`.
#' @inheritParams key_group sep
#' @param order Order to set the upset layers in. One of the following:
#' * A `<character[n]>` giving pieces of split labels.
#' * An `<integer[n]>` giving the numerical order in which pieces of split
#'   labels should appear.
#' @param empty_label A `<character[1]>` giving a level label to assign to the
#'   breaks that match no values to the pieces of split labels. Can be `NULL`
#'   to omit labels for empty levels.
#' @param aesthetic A vector of values for the guide to represent equivalent to
#'   the `breaks` argument in scales. These will be mapped by the scale to
#'   positions. Alternatively, a `<numeric[n]>` vector to set positions
#'   directly. Positions are used to place symbols.
#' @param level A `<factor[n]>` or `<character[n]>` parallel to the `aesthetic`
#'   argument setting the label level of the symbol.
#' @param symbol (Optional) An `<integer[n]>` indexing the guide's
#'   `override.aes` parameter.
#' @param ... Additional graphical properties to set for each symbol. Valid
#'   properties are `colour`, `shape`, `size`, `fill` and `stroke`. These
#'   graphical properties have priority over properties derived via `symbol`
#'   or the theme.
#'
#' @return
#' A function.
#'
#' @name key_specialty
#' @family keys
#'
#' @examples
#' # An example scale
#' template <- scale_fill_viridis_c(limits = c(0, 10), breaks = c(2, 4, 6, 8))
#'
#' # Retrieving colourbar and colourstep keys
#' key_sequence()(template)
#' key_bins()(template)
#'
#' # Upset key with example scale
#' template <- scale_x_discrete(limits = c("A", "A,B", ""))
#' key_upset()(template)
#' # Putting 'B' in 1st level
#' key_upset(order = c("B", "A"))(template)
#' # Omit level for the empty break
#' key_upset(empty_label = NULL)(template)
#'
#' # Symbol key with example scale
#' template <- scale_x_discrete(limits = LETTERS[1:5])
#' key_symbols(aesthetic = LETTERS[1:3], level = 3:1)(template)
#' # Aesthetic can also be numeric
#' key_symbols(1:3, 3:1)(template)
#' # Setting level order via factors
#' ordered <- factor(c("X", "Y", "Z"), c("Y", "X", "Z"))
#' key_symbols(1:3, level = ordered)(template)
#' # Setting groups for symbols, for `guide_axis_symbols(override.aes)`
#' key_symbols(1:3, 1:3, symbol = c(1, 1, 2))(template)
#' # Passing individual graphical parameters
#' key_symbols(1:3, 3:1, colour = c("red", "green", "blue"))(template)
NULL

#' @export
#' @rdname key_specialty
key_sequence <- function(n = 15L) {
  force(n)
  check_number_whole(n, min = 2.0)
  function(scale, aesthetic = NULL) {
    aesthetic <- aesthetic %||% scale$aesthetics[1L]
    df <- map_sequence(scale = scale, aesthetic = aesthetic, nbin = n)
    class(df) <- c("key_sequence", "key_guide", class(df))
    df
  }
}

#' @export
#' @rdname key_specialty
key_bins <- function(even.steps = FALSE, show.limits = NULL) {
  force(even.steps)
  force(show.limits)
  function(scale, aesthetic = NULL) {
    aesthetic <- aesthetic %||% scale$aesthetics[1L]
    df <- binned_key(
      scale = scale, aesthetic = aesthetic,
      even_steps = even.steps, show_limits = show.limits
    )
    class(df) <- c("key_bins", "key_guide", class(df))
    df
  }
}

#' @export
#' @rdname key_specialty
key_upset <- function(
  sep = "[^[:alnum:]]+", order = NULL, empty_label = "Other"
) {
  check_string(sep)
  force(string)
  force(order)
  function(scale, aesthetic = NULL) {
    upset_from_split_label(
      scale = scale, aesthetic = aesthetic,
      sep = sep, order = order, empty_label = empty_label
    )
  }
}

#' @export
#' @rdname key_specialty
key_symbols <- function(aesthetic, level, symbol = NULL, ...) {
  if (is_integerish(level)) {
    level_universe <- sort(unique(level))
  } else {
    level_universe <- levels(level) %||% unique(level)
  }
  index <- match(level, level_universe)
  valid <- c("colour", "color", "shape", "size", "fill", "stroke")
  key <- data_frame0(
    aesthetic = aesthetic,
    .value = factor(level, level_universe),
    .col = index,
    .symbol = symbol,
    !!!extra_args(..., .valid_args = valid)
  )
  function(scale, aesthetic = NULL) {
    merge_symbol_key(scale, aesthetic, key)
  }
}

# Helpers -----------------------------------------------------------------

map_sequence <- function(scale, aesthetic, nbin = 15L, ...) {
  if (scale$is_discrete()) {
    cli::cli_abort("Cannot use {.fn key_sequence} for discrete scales.")
  }

  limits <- scale$get_limits()
  key <- seq(limits[1L], limits[2L], length.out = nbin)
  if (length(key) == 0L) {
    key <- vec_unique(limits)
  }
  key <- data_frame0(
    !!aesthetic := scale$map(key),
    .value  = key,
    .size  = length(key)
  )
  key
}

binned_key <- function(scale, aesthetic, even_steps, show_limits = NULL) {

  breaks <- scale$get_breaks()
  limits <- scale$get_limits()

  if (even_steps) {
    breaks <- parse_binned_breaks(scale, breaks, even.steps = even_steps)

    n <- length(breaks$bin_at)
    seq <- seq(0.0, n)
    seq <- rescale(seq, to = limits, from = c(0.0, n))

    key <- data_frame0(
      !!aesthetic := scale$map(breaks$bin_at),
      min = seq[-length(seq)],
      max = seq[-1L]
    )
    key <- vec_c(key, NA)
    key$.label <- key$.value <- NA

    key$.label[breaks$all %in% breaks$breaks] <- scale$get_labels(breaks$breaks)
    key$.value <- seq
  } else {
    all <- unique(sort(c(limits, breaks)))
    n <- length(all)
    bin_at <- (all[-1L] + all[-n]) / 2.0
    key <- data_frame0(
      !!aesthetic := scale$map(bin_at),
      min = all[-n],
      max = all[-1L]
    )
    key <- vec_c(key, NA)
    key$.label <- NA

    key$.label[all %in% breaks] <- scale$get_labels(breaks)
    key$.value <- all
  }

  show_limits <- show_limits %||% scale$show.limits %||% FALSE

  if (show_limits && (is.character(scale$labels) || is.numeric(scale$labels))) {
    cli::cli_warn(c(paste0(
      "{.arg show.limits} is ignored when {.arg labels} are given as a ",
      "{.cls character} vector."
    ), i = paste0(
      "Either add the limits to {.arg breaks} or provide a function for ",
      "{.arg labels}."
    )))
    show_limits <- FALSE
  }

  n <- nrow(key)
  if (show_limits) {
    key$.label[c(1L, n)] <- scale$get_labels(limits)
  }
  key$.value[is.na(key$.label)] <- NA
  key
}

parse_binned_breaks <- function(scale, breaks = scale$get_breaks(),
                                even.steps = TRUE) {
  breaks <- breaks[!is.na(breaks)]
  if (length(breaks) == 0L) {
    return(NULL)
  }
  breaks <- sort(breaks)
  if (is.numeric(breaks)) {
    limits <- scale$get_limits()
    if (!is.numeric(scale$breaks)) {
      breaks <- breaks[!breaks %in% limits]
    }
    all_breaks <- unique0(c(limits[1L], breaks, limits[2L]))
    bin_at <- all_breaks[-1L] - diff(all_breaks) / 2.0
  } else {
    if (isFALSE(even.steps)) {
      cli::cli_warn(paste0(
        "{.code even.steps = FALSE} is not supported when used with a ",
        "discrete scale."
      ))
    }
    bin_at <- breaks
    nums <- as.character(breaks)
    nums <- strsplit(gsub("\\(|\\)|\\[|\\]", "", nums), ",\\s?")
    nums <- as.numeric(unlist(nums, FALSE, FALSE))

    if (anyNA(nums)) {
      cli::cli_abort(c(
        "Breaks are not formatted correctly for a bin legend.",
        i = "Use {.code (<lower>, <upper>]} format to indicate bins."
      ))
    }
    all_breaks <- nums[c(1L, seq_along(breaks) * 2L)]
    limits <- all_breaks[ c(1L, length(all_breaks))]
    breaks <- all_breaks[-c(1L, length(all_breaks))]
  }
  list(
    breaks = breaks,
    limits = limits,
    bin_at = bin_at,
    all    = all_breaks
  )
}

upset_from_split_label <- function(
  scale, aesthetic, sep, order, empty_label = NULL
) {

  aesthetic <- aesthetic %||% scale$aesthetics[1L]
  key <- Guide$extract_key(scale, aesthetic)

  label <- strsplit(key$.label, split = sep)

  levels <- unique(unlist(label))
  if (is.character(order)) {
    levels <- c(intersect(order, levels), setdiff(levels, order))
  } else if (is.numeric(order)) {
    levels <- levels[union(order, seq_along(levels))]
  }
  levels <- factor(levels, levels)

  mtx <- t(vapply(label, function(x) levels %in% x, logical(length(levels))))
  colnames(mtx) <- levels

  empty <- rowSums(mtx) == 0.0
  if (any(empty) && !is.null(empty_label)) {
    empty[!empty] <- NA
    mtx <- cbind(mtx, empty)
    colnames(mtx)[ncol(mtx)] <- empty_label
    levels <- c(levels, factor(empty_label, empty_label))
  }

  row <- as.vector(row(mtx))
  col <- as.vector(col(mtx))
  df <- data_frame0(
    x = key$x[row],
    y = key$y[row],
    .col = col,
    .row = row,
    .value = levels[col],
    .symbol = as.vector(mtx)
  )

  class(df) <- c("key_guide", "key_upset", class(df))
  df
}

merge_symbol_key <- function(scale, aesthetic, key) {
  aesthetic <- aesthetic %||% scale$aesthetics[1L]
  if (!is.numeric(key$aesthetic)) {
    scale_key <- Guide$extract_key(scale, aesthetic)
    i <- match(key$aesthetic, scale_key$.value)
    key <- data_frame0(
      !!!key[setdiff(names(key), "aesthetic")],
      !!!scale_key[i, setdiff(names(scale_key), names(key))]
    )
  }
  key$.row <- key[[aesthetic]]
  key
}
