# Standard keys -----------------------------------------------------------

#' Standard keys
#'
#' @description
#' These functions are helper functions for working with tick marks as keys
#' in guides. They all share the goal of creating a guide key, but have
#' different outcomes:
#'
#' * `key_auto()` is a function factory whose functions extract a typical
#'   key from major breaks in a scale.
#' * `key_manual()` uses user-provided vectors to make a key.
#' * `key_map()` makes mappings from a `<data.frame>` to make a key.
#' * `key_minor()` is a function factory whose functions also extract minor
#'   break positions for minor tick marks.
#' * `key_log()` is a function factory whose functions place ticks at intervals
#'   in log10 space.
#'
#' @param aesthetic,value A vector of values for the guide to represent
#'   equivalent to the `breaks` argument in scales. The `aesthetic` will be
#'   mapped, whereas `value` will not. For most intents and purposes,
#'   `aesthetic` and `value` should be identical.
#' @param label A `<character>` or list of expressions to use as labels.
#' @param type A `<character>` vector representing the one of the break types:
#'   `"major"`, `"minor"` or `"mini"`. If `NULL` (default), all breaks are
#'   treated as major breaks.
#' @param data A `<data.frame>` or similar object coerced by
#'   [`fortify()`][ggplot2::fortify] to a `<data.frame>`, in which the `mapping`
#'   argument is evaluated.
#' @param ... [`<data-masking>`][rlang::topic-data-mask] A set of mappings
#'   similar to those provided to [`aes()`][ggplot2::aes], which will be
#'   evaluated in the `data` argument. These must contain `aesthetic` mapping.
#' @param .call A [call][rlang::topic-error-call] to display in messages.
#' @inheritParams ggplot2::guide_axis_logticks
#' @param labeller A `<function>` that receives major breaks and returns
#'   formatted labels.
#'
#' @name key_standard
#' @family keys
#' @return
#' For `key_auto()`, `key_minor()` and `key_log()` a function. For
#' `key_manual()` and `key_map()` a `<data.frame>` with the `<key_standard>`
#' class.
#'
#' @examples
#' # An example scale
#' template <- scale_x_continuous(limits = c(0, 10))
#'
#' # The auto, minor and log keys operate on scales
#' key_auto()(template)
#' key_minor()(template)
#'
#' # So does the log key
#' template <- scale_x_continuous(transform = "log10", limits = c(0.1, 10))
#' key_log()(template)
#'
#' # Providing custom values
#' key_manual(
#'   aesthetic = 1:5,
#'   label = c("one", "two", "three", "four", "five")
#' )
#'
#' # Values from a `<data.frame>`
#' key_map(ToothGrowth, aesthetic = unique(supp))
NULL

#' @rdname key_standard
#' @export
key_auto <- function() {
  function(scale, aesthetic = NULL) {
    aesthetic <- aesthetic %||% scale$aesthetics[1]
    df <- Guide$extract_key(scale, aesthetic)
    class(df) <- c("key_standard", "key", class(df))
    df
  }
}

#' @rdname key_standard
#' @export
key_manual <- function(aesthetic, value = aesthetic, label = as.character(value), type = NULL) {
  df <- data_frame0(aesthetic = aesthetic, value = value,
                    label = label, type = type)
  check_columns(df, c("aesthetic", "value", "label"))
  df <- rename(df, c("value", "label", "type"), c(".value", ".label", ".type"))
  class(df) <- c("key_standard", "key", class(df))
  df
}

#' @rdname key_standard
#' @export
key_map <- function(data, ..., .call = caller_env()) {
  mapping <- enquos(...)
  mapping <- Filter(Negate(quo_is_missing), mapping)
  mapping <- new_aes(mapping)

  df <- eval_aes(
    data, mapping,
    required = c("aesthetic"),
    optional = c("value", "label"),
    call = .call, arg_mapping = "mapping", arg_data = "data"
  )
  df$value <- df$value %||% df$aesthetic
  df$label <- df$label %||% as.character(df$aesthetic)
  check_columns(df, c("aesthetic", "value", "label"))

  df <- rename(df, c("value", "label"), c(".value", ".label"))
  class(df) <- c("key_standard", "key", class(df))
  df
}


#' @rdname key_standard
#' @export
key_minor <- function() {
  function(scale, aesthetic = NULL) {
    aesthetic <- aesthetic %||% scale$aesthetics[1]
    df <- GuideAxis$extract_key(scale, aesthetic, minor.ticks = TRUE)
    class(df) <- c("key_standard", "key", class(df))
    df
  }
}

#' @rdname key_standard
#' @export
key_log <- function(
  prescale_base = NULL, negative_small = 0.1, expanded = TRUE,
  labeller = label_log()
) {
  check_number_decimal(
    negative_small, min = 1e-100,
    allow_infinite = FALSE,
    allow_null = TRUE
  )
  check_bool(expanded)
  check_number_decimal(prescale_base, allow_infinite = FALSE, allow_null = TRUE)
  check_function(labeller)
  force(prescale_base)
  force(negative_small)
  force(expanded)
  call <- expr(key_log())
  function(scale, aesthetic = NULL) {
    log10_keys(
      scale = scale, aesthetic = aesthetic,
      prescale_base = prescale_base,
      negative_small = negative_small,
      expanded = expanded, call = call
    )
  }
}

# Extractor ---------------------------------------------------------------

standard_extract_key <- function(scale, aesthetic, key, ...) {
  key <- resolve_key(key %||% "auto")
  if (is.function(key)) {
    key <- key(scale, aesthetic)
  } else {
    key$aesthetic <-
      scale_transform(key$aesthetic, scale, map = TRUE, "aesthetic")
    key$.value <-
      scale_transform(key$.value, scale, map = FALSE, "value")
  }
  key <- rename(key, "aesthetic", aesthetic)
  key <- validate_key_types(key)

  if (is.numeric(key$.value)) {
    range <- scale$continuous_range %||% scale$get_limits()
    key <- vec_slice(key, is.finite(oob_censor_any(key$.value, range)))
  } else {
    key
  }

  key
}

# Helpers -----------------------------------------------------------------

validate_key_types <- function(key, call = caller_env()) {
  types <- c("major", "minor", "mini")
  if (!all(key$.type %in% types)) {
    extra <- setdiff(unique(key$.type), types)
    cli::cli_warn(c(paste0(
      "Key types must be one of {.or {.val {types}}}, not ",
      "{.or {.val {extra}}}."
    ), i = "Unknown types are dropped."), call = call)
    key <- vec_slice(key, key$.type %in% types)
  }
  key
}

resolve_key <- function(x) {
  if (is.character(x)) {
    fun <- find_global(paste0("key_", x), env = global_env(),
                       mode = "function")
    if (is.function(fun)) {
      x <- fun()
    }
  }
  if (is.function(x)) {
    return(x)
  }
  if (inherits(x, "key")) {
    return(x)
  }
  cli::cli_abort("Unknown key specification: {x}.")
}

log10_keys <- function(scale, aesthetic,
                       prescale_base = prescale_base,
                       negative_small = negative_small,
                       expanded = expanded,
                       labeller = label_log(),
                       call = caller_env()) {
  aesthetic <- aesthetic %||% scale$aesthetics[1]
  if (scale$is_discrete()) {
    cli::cli_abort(
      "Cannot calculate logarithmic ticks for discrete scales.",
      call = call
    )
  }

  transform <- scale$get_transformation()
  if (!is.null(prescale_base)) {
    if (transform$name != "identity") {
      cli::cli_warn(paste0(
        "The {.arg prescale_base} argument will override the scale's ",
        "{.field {transform$name}} transformation in log-tick positioning."
      ), call = call)
    }
    transform <- transform_log(base = prescale_base)
  }

  limits <- transform$inverse(scale$get_limits())
  has_negatives <- any(limits <= 0)

  if (!has_negatives) {
    start <- floor(log10(min(limits))) - 1L
    end   <- ceiling(log10(max(limits))) + 1L
  } else {
    negative_small <- negative_small %||% 0.1
    start <- floor(log10(abs(negative_small)))
    end   <- ceiling(log10(max(abs(limits)))) + 1L
  }

  tens  <- 10^seq(start, end, by = 1)
  fives <- tens * 5
  ones  <- as.vector(outer(setdiff(2:9, 5), tens))

  if (has_negatives) {
    tens  <- tens[tens >= negative_small]
    tens  <- c(tens, -tens, 0)
    fives <- fives[fives >= negative_small]
    fives <- c(fives, -fives)
    ones  <- ones[ones >= negative_small]
    ones  <- c(ones, -ones)
  }
  labels <- labeller(tens)

  ticks  <- transform$transform(c(tens, fives, ones))
  nticks <- c(length(tens), length(fives), length(ones))

  if (is.expression(labels)) {
    labels <- as.list(labels)
    labels <- pad(labels, sum(nticks), fill = list(NULL))
  } else {
    labels <- pad(labels, sum(nticks))
  }

  key <- data_frame0(
    !!aesthetic := ticks,
    .label = labels,
    .type = rep(c("major", "minor", "mini"), times = nticks)
  )

  if (expanded) {
    range <- scale$continuous_range %||% scale$get_limits()
  } else {
    range <- scale$get_limits()
  }
  key <- vec_slice(key, !is_oob(ticks, range))
  class(key) <- c("key_standard", "key", class(key))
  key
}

transform_key <- function(key, position, coord, panel_params) {
  if (is_empty(key)) {
    return(key)
  }
  other <- switch(position, bottom = , left = , theta.sec = -Inf, Inf)
  key <- replace_null(key, x = other, y = other)
  transformed <- coord$transform(key, panel_params)

  if (position %in% c("theta", "theta.sec")) {
    add <- if (position == "theta.sec") pi else 0
    transformed$theta <- transformed$theta + add
  }

  ends <- c("xend", "yend") %in% names(key)
  if (!any(ends)) {
    return(transformed)
  }

  if (ends[1]) {
    key <- rename(key, c("x", "xend"), rev)
  }
  if (ends[2]) {
    key <- rename(key, c("y", "yend"), rev)
  }
  key <- coord$transform(key, panel_params)
  if (position %in% c("theta", "theta.sec")) {
    transformed$thetaend <- key$theta + add
  } else {
    if (ends[1]) {
      transformed$xend <- key$x
    }
    if (ends[2]) {
      transformed$yend <- key$y
    }
  }

  transformed
}
