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
#' * `key_none()` makes an empty key with no entries.
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
#' @param labeller A `<function>` that receives major breaks and returns
#'   formatted labels. For `key_log()`, `NULL` will default to
#'   [`scales::label_log()`] for strictly positive numbers and a custom labeller
#'   when negative numbers are included.
#' @param prescale_base A `<numeric[1]>` giving the base of logarithm to
#'   transform data manually. The default, `NULL`, will use the scale
#'   transformation to calculate positions. It is only advisable to set the
#'   `prescale_base` argument when the data have already been log-transformed.
#'   When using a log-transform in the scale or in
#'   [`coord_trans()`][ggplot2::coord_trans], the default `NULL` is recommended.
#' @param negative_small A `<numeric[1]>` setting the smallest absolute value
#'   that is marked with a tick in case the scale limits include 0 or negative
#'   numbers.
#' @param expanded A `<logical[1]>` determining whether the ticks should cover
#'   the entire range after scale expansion (`TRUE`, default), or be restricted
#'   to the scale limits (`FALSE`).
#' @param .call A [call][rlang::topic-error-call] to display in messages.
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
#'
#' # Empty key
#' key_none()
NULL

#' @rdname key_standard
#' @export
key_auto <- function(...) {
  function(scale, aesthetic = NULL) {
    aesthetic <- aesthetic %||% scale$aesthetics[1]
    df <- Guide$extract_key(scale, aesthetic)
    df <- data_frame0(df, !!!extra_args(...))
    class(df) <- c("key_standard", "key_guide", class(df))
    df
  }
}

#' @rdname key_standard
#' @export
key_manual <- function(aesthetic, value = aesthetic,
                       label = as.character(value), type = NULL,
                       ...) {
  df <- data_frame0(aesthetic = aesthetic, value = value,
                    label = label, type = type, !!!extra_args(...))
  check_columns(df, c("aesthetic", "value", "label"))
  df <- rename(df, c("value", "label", "type"), c(".value", ".label", ".type"))
  class(df) <- c("key_standard", "key_guide", class(df))
  df
}

#' @rdname key_standard
#' @export
key_map <- function(data, ..., .call = caller_env()) {
  mapping <- enquos(...)
  mapping <- Filter(Negate(quo_is_missing), mapping)
  mapping <- new_aes(mapping, env = .call)

  df <- eval_aes(
    data, mapping,
    required = c("aesthetic"),
    optional = c("value", "label", .label_params),
    call = .call, arg_mapping = "mapping", arg_data = "data"
  )
  df$value <- df$value %||% df$aesthetic
  df$label <- df$label %||% as.character(df$aesthetic)
  check_columns(df, c("aesthetic", "value", "label"))

  df <- rename(
    df,
    c("value", "label", .label_params),
    c(".value", ".label", paste0(".", .label_params))
  )
  class(df) <- c("key_standard", "key_guide", class(df))
  df
}


#' @rdname key_standard
#' @export
key_minor <- function(...) {
  dots <- extra_args(...)
  function(scale, aesthetic = NULL) {
    aesthetic <- aesthetic %||% scale$aesthetics[1]
    df <- GuideAxis$extract_key(scale, aesthetic, minor.ticks = TRUE)
    df <- data_frame0(df, !!!dots)
    class(df) <- c("key_standard", "key_guide", class(df))
    df
  }
}

#' @rdname key_standard
#' @export
key_log <- function(
  prescale_base = NULL, negative_small = 0.1, expanded = TRUE,
  labeller = NULL, ...
) {
  check_number_decimal(
    negative_small, min = 1e-100,
    allow_infinite = FALSE,
    allow_null = TRUE
  )
  check_bool(expanded)
  check_number_decimal(prescale_base, allow_infinite = FALSE, allow_null = TRUE)
  check_function(labeller, allow_null = TRUE)
  force(prescale_base)
  force(negative_small)
  force(expanded)
  dots <- extra_args(...)
  call <- expr(key_log())
  function(scale, aesthetic = NULL) {
    key <- log10_keys(
      scale = scale, aesthetic = aesthetic,
      prescale_base = prescale_base,
      negative_small = negative_small,
      expanded = expanded, extra_args = dots, call = call
    )
  }
}

#' @rdname key_standard
#' @export
key_none <- function() {
  new_data_frame(
    list(aesthetic = logical(), .value = logical(), .label = character()),
    n = 0L, class = c("key_standard", "key_guide")
  )
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

resolve_key <- function(x, allow_null = FALSE) {
  if (allow_null && is.null(x)) {
    return(NULL)
  }
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
  if (inherits(x, "key_guide")) {
    return(x)
  }
  cli::cli_abort("Unknown key specification: {x}.")
}

log10_keys <- function(scale, aesthetic,
                       prescale_base = NULL,
                       negative_small = 0.1,
                       expanded = TRUE,
                       labeller = NULL,
                       extra_args = NULL,
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
    labeller <- labeller %||% negative_log_label
  }
  labeller <- labeller %||% scales::label_log()
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
    .value = ticks,
    .label = labels,
    .type = rep(c("major", "minor", "mini"), times = nticks),
    !!!extra_args
  )

  if (expanded) {
    range <- scale$continuous_range %||% scale$get_limits()
  } else {
    range <- scale$get_limits()
  }
  key <- vec_slice(key, !is_oob(ticks, range))
  class(key) <- c("key_standard", "key_guide", class(key))
  key
}

negative_log_label <- function(x) {
  if (length(x) == 0) {
    return(expression)
  }
  sign <- as.character(sign(x))
  sign[sign == "-1"] <- "-"
  sign[sign == "1"] <- "+"
  sign[sign == "0"] <- ""

  abs <- abs(x)
  exponent <- format(log(abs, base = 10), digits = 3)
  text <- paste0(sign, 10, "^", exponent)
  text[x == 0] <- "0"

  out <- vector("expression", length(text))
  for (i in seq_along(text)) {
    expr <- parse(text = text[[i]])
    out[[i]] <- if (length(expr) == 0) NA else expr[[1]]
  }
  out[is.na(x)] <- NA
  out
}

transform_key <- function(key, position, coord, panel_params) {
  if (is_empty(key)) {
    return(key)
  }
  other <- switch(position, bottom = , left = , theta.sec = -Inf, Inf)
  key <- replace_null(key, x = other, y = other)
  transformed <- coord$transform(key, panel_params)

  if (is_theta(position)) {
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
  if (is_theta(position)) {
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
