
check_columns <- function(data, columns, call = caller_env(),
                          arg = caller_arg(data)) {
  check_data_frame(data, arg = arg, call = call)
  if (all(columns %in% names(data))) {
    return()
  }
  missing <- setdiff(columns, names(data))
  cli::cli_abort(
    "The {.field {missing}} column{?s} {?is/are} required.",
    call = call
  )
}

check_object <- function(
  x, check_fun, what, ...,
  allow_null = FALSE, arg = caller_arg(x), call = caller_env()
) {
  if (!missing(x)) {
    if (check_fun(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }
  stop_input_type(
    x, as_cli(what), ...,
    allow_null = allow_null, arg = arg, call = call
  )
}

check_inherits <- function(
  x, class, what = NULL, ...,
  allow_null = FALSE, call = caller_env()
) {
  if (!missing(x)) {
    if (inherits(x, class)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }
  what <- what %||% paste(
    "a", oxford_comma(paste0("{.cls ", class, "}")), "object"
  )

  stop_input_type(
    x, as_cli(what), ...,
    allow_null = allow_null, arg = arg, call = call
  )
}
