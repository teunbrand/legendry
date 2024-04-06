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
#'
#' @return
#' For `key_sequence()` a function.
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
NULL

#' @export
#' @rdname key_specialty
key_sequence <- function(n = 15) {
  force(n)
  check_number_whole(n, min = 2)
  function(scale, aesthetic = NULL) {
    aesthetic <- aesthetic %||% scale$aesthetics[1]
    df <- map_sequence(scale = scale, aesthetic = aesthetic, nbin = n)
    class(df) <- c("key_sequence", "key", class(df))
    df
  }
}

#' @export
#' @rdname key_specialty
key_bins <- function(even.steps = FALSE, show.limits = NULL) {
  force(even.steps)
  force(show.limits)
  function(scale, aesthetic = NULL) {
    aesthetic <- aesthetic %||% scale$aesthetics[1]
    df <- binned_key(
      scale = scale, aesthetic = aesthetic,
      even_steps = even.steps, show_limits = show.limits
    )
    class(df) <- c("key_bins", "key", class(df))
    df
  }
}

# Helpers -----------------------------------------------------------------

map_sequence <- function(scale, aesthetic, nbin = 15, ...) {
  if (scale$is_discrete()) {
    cli::cli_abort("Cannot use {.fn key_sequence} for discrete scales.")
  }

  limits <- scale$get_limits()
  key <- seq(limits[1], limits[2], length.out = nbin)
  if (length(key) == 0) {
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
    seq <- seq(0, n)
    seq <- rescale(seq, to = limits, from = c(0, n))

    key <- data_frame0(
      !!aesthetic := scale$map(breaks$bin_at),
      min = seq[-length(seq)],
      max = seq[-1]
    )
    key <- vec_c(key, NA)
    key$.label <- key$.value <- NA

    key$.label[breaks$all %in% breaks$breaks] <- scale$get_labels(breaks$breaks)
    key$.value <- seq
  } else {
    all <- unique(sort(c(limits, breaks)))
    n <- length(all)
    bin_at <- (all[-1] + all[-n]) / 2
    key <- data_frame0(
      !!aesthetic := scale$map(bin_at),
      min = all[-n],
      max = all[-1],
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
    key$.label[c(1, n)] <- scale$get_labels(limits)
  }
  key$.value[is.na(key$.label)] <- NA
  key
}

parse_binned_breaks <- function(scale, breaks = scale$get_breaks(),
                                even.steps = TRUE) {
  breaks <- breaks[!is.na(breaks)]
  if (length(breaks) == 0) {
    return(NULL)
  }
  breaks <- sort(breaks)
  if (is.numeric(breaks)) {
    limits <- scale$get_limits()
    if (!is.numeric(scale$breaks)) {
      breaks <- breaks[!breaks %in% limits]
    }
    all_breaks <- unique0(c(limits[1], breaks, limits[2]))
    bin_at <- all_breaks[-1] - diff(all_breaks) / 2
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
    all_breaks <- nums[c(1, seq_along(breaks) * 2)]
    limits <- all_breaks[ c(1, length(all_breaks))]
    breaks <- all_breaks[-c(1, length(all_breaks))]
  }
  list(
    breaks = breaks,
    limits = limits,
    bin_at = bin_at,
    all    = all_breaks
  )
}
