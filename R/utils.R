
is_blank <- function(x) inherits(x, c("element_blank", "NULL"))

.in2cm <- 2.54

match_self <- function(x) {
  match(x, sort(unique(x)))
}

eval_aes <- function(
  data, mapping,
  required = character(),
  optional = character(),
  call     = caller_env(),
  arg_mapping = caller_arg(mapping),
  arg_data    = caller_arg(data)
) {
  valid <- c(optional, required)
  call <- call %||% current_call()
  if (!inherits(mapping, "uneval")) {
    cli::cli_abort(
      "{.arg {arg_mapping}} must be created by {.fn aes}.",
      call = call
    )
  }
  data <- fortify(data)

  values <- lapply(mapping, eval_tidy, data = data)
  sizes  <- list_sizes(values)
  values <- values[sizes > 0]

  if (!is.null(valid)) {
    extra_nms <- setdiff(names(values), valid)
    if (length(extra_nms) > 0) {
      cli::cli_warn(
        "Ignoring unknown aesthetics: {.field {extra_nms}}.",
        call = call
      )
    }
    values <- values[intersect(names(values), valid)]
    sizes  <- list_sizes(values)
  }

  if (length(sizes) == 0) {
    cli::cli_warn(
      "No valid data found with {.arg {arg_mapping}} in {.arg {arg_data}}.",
      call = call
    )
    return(data_frame0())
  }
  df <- data_frame0(
    !!!values, .size = max(sizes),
    .error_call = call
  )

  check_columns(df, required, arg = 'key', call = call)

  df
}

`%|NA|%` <- function(x, y) {
  if (length(x) == 0) {
    return(y)
  }
  if (length(y) == 1) {
    x[is.na(x)] <- y
  } else {
    x[is.na(x)] <- y[is.na(x)]
  }
  x
}

get_attr <- function(x, which, default = NULL) {
  attr(x, which = which, exact = TRUE) %||% default
}

get_size_attr <- function(x, default = 0) {
  get_attr(x, "size", default = default)
}

get_width_attr <- function(x, default = 0) {
  get_attr(x, "width", default = default)
}

get_height_attr <- function(x, default = 0) {
  get_attr(x, "height", default = default)
}

pad <- function(x, length, fill = NA, where = "end") {
  padding <- rep(fill, length - length(x))
  switch(where, start = c(padding, x), c(x, padding))
}

rep0 <- function(x, ...) {
  if (is.null(x)) {
    return(NULL)
  }
  rep(x, ...)
}

rename <- function(df, old, new) {
  if (is.function(new)) {
    new <- new(old)
  }
  i <- match(old, names(df))
  new <- new[!is.na(i)]
  i <- i[!is.na(i)]
  names(df)[i] <- new
  df
}

.flip_names <-
  c(x = "y", y = "x", width = "height", height = "width", hjust = "vjust",
    vjust = "hjust", margin_x = "margin_y", margin_y = "margin_x",
    x1 = "y1", x2 = "y2", y1 = "x1", y2 = "x2")

flip_names <- function(x) {
  rename(x, .flip_names, names(.flip_names))
}

is_discrete <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x) || is_mapped_discrete(x)
}

is_oob <- function(x, limits) {
  limits <- sort(limits)
  x < limits[1] | x > limits[2]
}

in_range <- function(x, range) {
  range <- sort(range)
  x >= range[1] & x <= range[2]
}

in_ranges <- function(x, start, end) {
  lower <- pmin(start, end)
  upper <- pmax(start, end)
  smaller <- outer(lower, x, FUN = "<")
  larger  <- outer(upper, x, FUN = ">")
  colSums(larger & smaller) > 0
}

polar_xy <- function(data, r, theta, bbox) {
  data$x <- rescale(r * sin(theta) + 0.5, from = bbox$x)
  data$y <- rescale(r * cos(theta) + 0.5, from = bbox$y)
  data
}

scale_transform <- function(x, scale, map = FALSE, arg = caller_arg(x)) {
  if (is_discrete(x) && !scale$is_discrete()) {
    cli::cli_abort(
      "The key {.field {arg}} must be {.emph continuous}, not discrete."
    )
  }
  transform <- scale$get_transformation()
  if (is.null(transform)) {
    if (map) {
      x <- (scale$scale$map %||% scale$map)(x)
    }
    return(x)
  }
  x <- transform$transform(x)
  if (map) {
    x <- scale$map(x)
  }
  x
}

cm <- function(x) {
  if (!is.unit(x)) {
    return(x)
  }
  convertUnit(x, "cm", valueOnly = TRUE)
}

new_rle <- function(x) {
  rle <- vec_unrep(x)
  rle$end   <- cumsum(rle$times)
  rle$start <- rle$end - rle$times + 1
  rle
}

suffix_position <- function(value, position) {

  aesthetic <- switch(position, left = , right = "y", "x")
  position  <- switch(position, theta = "bottom", theta.sec = "top", position)
  suffix <- paste0(".", aesthetic, ".", position)

  char <- is_each(value, is.character)
  char <- char & !is_each(value, inherits, what = "AsIs")
  value[char] <- lapply(value[char], paste0, suffix)
  value
}

is_theta <- function(x) {
  if (is_missing(x) || !is.character(x)) {
    return(FALSE)
  }
  x %in% c("theta", "theta.sec")
}

# Based on example in ?vctrs::vec_chop
# It's faster than stats::ave
vec_ave <- function(x, group, fun, ...) {
  index   <- vec_group_loc(group)$loc
  chopped <- lapply(vec_chop(x, indices = index), fun, ...)
  list_unchop(chopped, indices = index)
}

by_group <- function(x, group, fun, ..., value = x[1]) {
  index <- vec_group_loc(group)$loc
  vapply(vec_chop(x, indices = index), FUN = fun, FUN.VALUE = value, ...)
}

set_list_element <- function(x, i, value) {
  lapply(x, `[<-`, i = i, value = list(value))
}

guide_rescale <- function(value, from = range(value), oob = oob_squish_infinite) {
  from <- from %||% c(0, 1)
  rescale(oob(value, from), to = c(0, 1), from)
}

is_each <- function(x, fun, ...) {
  vapply(x, FUN = fun, FUN.VALUE = logical(1), ...)
}

map_dbl <- function(x, fun, ...) {
  vapply(x, FUN = fun, FUN.VALUE = numeric(1), ...)
}

map_chr <- function(x, fun, ...) {
  vapply(x, FUN = fun, FUN.VALUE = character(1), ...)
}

filter_finite <- function(x) {
  x[is.finite(x)]
}

match_list <- function(x, list) {
  findInterval(
    match(x, unlist(list, FALSE, FALSE)),
    cumsum(lengths(list)), left.open = TRUE
  ) + 1
}

apply_theme_defaults <- function(theme, defaults = NULL) {
  if (is.null(defaults)) {
    return(theme)
  }
  theme    <- replace_null(theme, !!!defaults)
  relative <- names(defaults)[is_each(defaults, is.rel)]
  relative <- intersect(relative, names(theme))
  for (i in relative) {
    theme[[i]] <- theme[[i]] * unclass(defaults[[i]])
  }
  theme
}

insert_before <- function(x, i, value) {
  new <- vec_init(x, length(x) + length(i))
  i <- i + seq_along(i) - 1
  new[i] <- value
  new[-i] <- x
  new
}

insert_after <- function(x, i, value) {
  new <- vec_init(x, length(x) + length(i))
  i <- i + seq_along(i)
  new[i] <- value
  new[-i] <- x
  new
}

get_just <- function(element) {
  rotate_just(
    element$angle %||% 0,
    element$hjust %||% 0.5,
    element$vjust %||% 0.5
  )
}

.label_params <- setdiff(fn_fmls_names(element_text), c("margin", "debug", "inherit.blank"))
.line_params <- c("colour", "color", "linewidth", "linetype")

extra_args <- function(..., .valid_args = .label_params, call = caller_env()) {
  args <- list2(...)
  if (length(args) == 0) {
    return(NULL)
  }

  if (!is.null(args$color)) {
    args$colour <- args$color
    args$color <- NULL
  }
  extra <- setdiff(names(args), .valid_args)
  if (length(extra) > 0) {
    cli::cli_warn("Ignoring unknown parameters: {.and {extra}}.", call = call)
  }
  args <- args[lengths(args) > 0]
  names(args) <- paste0(".", names(args))
  args
}
