
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

check_unit <- function(x, allow_null = FALSE, call = caller_env(),
                       arg = caller_arg(x)) {
  if (!missing(x)) {
    if (is.unit(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }
  stop_input_type(
    x, as_cli("a {.cls unit} object"),
    allow_null = allow_null, arg = arg, call = call
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

check_length <- function(
  x, min = NULL, max = NULL, exact = NULL,
  allow_null = FALSE, arg = caller_arg(x), call = caller_env()
) {
  if (!missing(x)) {
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
    length <- length(x)
    if (!is.null(min %||% max)) {
      between <- c(min %||% 0, max %||% Inf)
      if (length >= between[1] && length <= between[2]) {
        return(invisible(NULL))
      }
    }
    if (!is.null(exact) && length %in% exact) {
      return(invisible(NULL))
    }
  }
  if (is.null(min) && is.null(max) && is.null(exact)) {
    return(invisible(NULL))
  }
  if (!is.null(min) && !is.null(max)) {
    what <- "a vector with {.field length} between {min} and {max}"
  } else if (!is.null(min)) {
    what <- "a vector with {.field length} more than or equal to {min}"
  } else if (!is.null(max)) {
    if (max > 1) {
      what <- "a vector with {.field length} less than or equal to {max}"
    } else {
      what <- "a value with {.field length} less than or equal to {max}"
    }
  } else if (!is.null(exact)) {
    if (exact > 1) {
      what <- "a vector with {.field length} equal to {exact}"
    } else {
      what <- "a single value with {.field length} equal to {exact}"
    }
  }
  obj <- obj_type_friendly(x, value = FALSE)
  msg <- "{.arg {arg}} must be {as_cli(what)}, not {obj} with length {length(x)}."
  cli::cli_abort(msg, call = call)
}

check_inherits <- function(
  x, class, what = NULL, ...,
  allow_null = FALSE, arg = caller_arg(x), call = caller_env()
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

check_argmatch <- function(
    x, options,
    ...,
    allow_null = FALSE,
    arg = caller_arg(x),
    call = caller_env()
) {
  if (!missing(x)) {
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
    if (is.character(x)) {
      arg_match0(x, options, arg_nm = arg, error_call = call)
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x, "a single string", ...,
    allow_na = FALSE, allow_null = allow_null,
    arg = arg, call = call
  )
}

check_position <- new_function(
  `[[<-`(fn_fmls(check_argmatch), "options", .trblt),
  body(check_argmatch),
  fn_env(check_argmatch)
)
