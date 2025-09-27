
check_columns <- function(data, columns, call = caller_env(),
                          arg = caller_arg(data)) {
  check_data_frame(data, arg = arg, call = call)
  if (all(columns %in% names(data))) {
    return(invisible())
  }
  missing <- setdiff(columns, names(data))
  cli::cli_abort(
    "The {.field {missing}} column{?s} {?is/are} required.",
    call = call
  )
}

check_list_names <- function(data, names, call = caller_env(),
                             arg = caller_arg(data)) {
  check_object(data, is.list, what = "a {.cls list}", arg = arg, call = call)
  if (all(names %in% names(data))) {
    return(invisible())
  }
  missing <- setdiff(names, names(data))
  a <- if (length(missing) == 1) "a" else ""
  cli::cli_abort(paste0(
    "The {.arg {arg}} argument must have {a} named {.field {missing}} ",
    "element{?s}."
  ), call = call)
}

check_list_of <- function(x, class, allow_null = FALSE,
                          call = caller_env(), arg = caller_arg(x)) {
  problems <- character()
  if (!missing(x)) {
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
    if (is.list(x)) {
      fail <- !map_lgl(x, inherits, what = class)
      if (!any(fail)) {
        return(invisible(NULL))
      }
      problems <- map_chr(x[fail], obj_type_friendly)
      problems <- paste0(arg, "[[", which(fail), "]] is ", problems)
      names(problems) <- rep("x", length(problems))
      if (length(problems) > 5) {
        problems <- c(problems[1:5], "x" = "...and more mismatches.")
      }
    }
  }

  class <- map_chr(class, function(x) as_cli("{.cls {x}}"))
  end <- if (is.list(x)) "." else paste0(", not ", obj_type_friendly(x), ".")

  message <- sprintf(
    "`%s` must be %s%s",
    arg, as_cli("a {.cls list} object with {.or {class}} elements"),
    end
  )
  message <- c(message, problems)
  abort(message, call = call, arg = arg)
}

check_grob <- function(x, allow_null = FALSE, call = caller_env(),
                       arg = caller_arg(x)) {
  if (!missing(x)) {
    if (is.grob(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }
  stop_input_type(
    x, as_cli("a {.cls grob} object"),
    allow_null = allow_null, arg = arg, call = call
  )
}

check_unit <- function(x, allow_null = FALSE, allow_rel = FALSE,
                       call = caller_env(), arg = caller_arg(x)) {
  if (!missing(x)) {
    if (is.unit(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x) || allow_rel && is_rel(x)) {
      return(invisible(NULL))
    }
  }
  stop_input_type(
    x, as_cli("a {.cls unit} object"),
    allow_null = allow_null, arg = arg, call = call
  )
}

check_bare_numeric <- function(x, ..., allow_null = FALSE,
                               arg = caller_arg(x), call = caller_env()) {
  if (!missing(x)) {
    if (is_bare_numeric(x)) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible())
    }
  }
  stop_input_type(
    x, as_cli("a bare {.cls numeric} vector"), ...,
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
      if (in_range(length, between)) {
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
  type <- if (typeof(x) == "list") "list" else "vector"
  if (!is.null(min) && !is.null(max)) {
    what <- "a {type} with {.field length} between {min} and {max}"
  } else if (!is.null(min)) {
    what <- "a {type} with {.field length} more than or equal to {min}"
  } else if (!is.null(max)) {
    if (max > 1) {
      what <- "a {type} with {.field length} less than or equal to {max}"
    } else {
      what <- "a value with {.field length} less than or equal to {max}"
    }
  } else if (!is.null(exact)) {
    if (any(exact > 1)) {
      what <- "a {type} with {.field length} equal to {.or {exact}}"
    } else {
      what <- "a single value with {.field length} equal to {.or {exact}}"
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

check_position <- function(
    x, options = .trbl, theta = TRUE, inside = FALSE,
    ...,
    allow_null = FALSE,
    arg = caller_arg(x), call = caller_env()
) {
  if (!missing(x)) {
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
    if (is.character(x)) {
      if (theta) {
        options <- c(options, "theta", "theta.sec")
      }
      if (inside) {
        options <- c(options, "inside")
      }
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

check_unique <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!vec_duplicate_any(x)) {
    return(invisible())
  }
  dups <- unique(x[vec_duplicate_detect(x)])
  more <- if (length(dups) > 5) " and more." else "."
  dups <- dups[1:pmin(5, length(dups))]
  n <- length(dups)
  cli::cli_abort(c(
    "{.arg {arg}} must only have unique values.",
    i = paste0("Example {cli::qty(n)}duplicate{?s}: {.and {.val {dups}}}", more)
  ))
}

check_exclusive <- function(
  x, y, required = FALSE,
  x_arg = caller_arg(x), y_arg = caller_arg(y), call = caller_env()
) {
  x_present <- !(is_missing(x) || is.null(x))
  y_present <- !(is_missing(y) || is.null(y))
  if (xor(x_present, y_present)) {
    return(invisible())
  }
  if (required && !x_present && !y_present) {
    cli::cli_abort(
      "Either the {.arg {x_arg}} or {.arg {y_arg}} argument is required.",
      call = call
    )
  }
  if (!x_present && !y_present) {
    return(invisible())
  }
  cli::cli_abort(c(
    "The {.arg {x_arg}} and {.arg {y_arg}} arguments are mutually exclusive.",
    i = "Please use one, but not both."
  ), call = call)
}

check_matrix <- function(
  x, allow_null = FALSE, zero_dim = FALSE,
  arg = caller_arg(x), call = caller_env()
) {
  check_object(
    x, is.matrix, "a {.cls matrix}", allow_null = allow_null,
    arg = arg, call = call
  )

  # Test dimensions
  dim <- dim(x)
  valid_dim <- length(dim) == 2 && !anyNA(dim) &&
    all(dim >= (0 + as.numeric(!zero_dim)))

  if (valid_dim) {
    return(invisible(NULL))
  }

  cli::cli_abort(
    "The {.arg {arg}} argument has invalid dimensions: {.value {dim}}.",
    call = call
  )
}
