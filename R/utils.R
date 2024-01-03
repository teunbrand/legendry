
is_blank <- function(x) inherits(x, c("element_blank", "NULL"))

.in2cm <- convertUnit(unit(1, "in"), "cm", valueOnly = TRUE)

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


pad <- function(x, length, fill = NA, where = "end") {
  padding <- rep(fill, length - length(x))
  switch(where, start = c(padding, x), c(x, padding))
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
    vjust = "hjust", margin_x = "margin_y", margin_y = "margin_x")

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
    x <- (scale$scale$map %||% scale$map)(x)
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

  char <- vapply(value, is.character, logical(1))
  char <- char & !vapply(value, inherits, logical(1), "AsIs")
  value[char] <- lapply(value[char], paste0, suffix)
  value
}
