
is_blank <- function(x) inherits(x, "element_blank")

.in2cm <- convertUnit(unit(1, "in"), "cm", valueOnly = TRUE)

eval_aes <- function(
  data, mapping, valid = NULL,
  call = caller_env(),
  arg_mapping = caller_arg(mapping),
  arg_data    = caller_arg(data)
) {
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
  data_frame0(
    !!!values, .size = max(sizes),
    .error_call = call
  )
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


