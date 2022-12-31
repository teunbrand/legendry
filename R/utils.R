as_unit <- function(x, unit) {
  if (!is.unit(x) && length(x)) {
    x <- unit(x, units = unit)
  }
  x
}

seq_row <- function(x) {seq_len(NROW(x))}
seq_col <- function(x) {seq_len(NCOL(x))}

chr_vapply <- function(
    X, FUN, FUN.VALUE = character(1),
    ..., USE.NAMES = FALSE
) {
  vapply(X, FUN, FUN.VALUE = FUN.VALUE, ..., USE.NAMES = USE.NAMES)
}

# Conditional lapply
clapply <- function(x, test, fun, ...) {
  x[test] <- lapply(x[test], fun, ...)
  x
}

# Function for checking object class is as expected
arg_class <- function(
  arg, class,
  arg_nm = caller_arg(arg),
  error_call = caller_env()
) {
  if (inherits(arg, class)) {
    return(arg)
  }
  arg_class <- if (is_vector(arg)) {
    vec_ptype_full(arg)
  } else {
    class(arg)[[1]]
  }
  arg_class <- paste0("<", arg_class, ">")
  class     <- paste0("<", class, ">")
  class     <- glue_collapse(class, sep = ", ", last = " or ")
  arg_nm    <- paste0("`", arg_nm, "`")
  msg <- paste0(
    arg_nm, " must be of the class ", class, ", not ", arg_class, "."
  )
  abort(msg, call = error_call)
}

arg_range <- function(
  arg, range, allow_na = FALSE, arg_nm = caller_arg(arg),
  error_call = caller_env()
) {
  arg <- arg_class(arg, c("numeric"), arg_nm = arg_nm, error_call = error_call)

  oob  <- arg < range[1] | arg > range[2]
  isna <- is.na(arg)
  oob[isna] <- !allow_na

  if (!any(oob)) {
    return(arg)
  }
  n_oob <- sum(oob)
  n_na  <- sum(isna)

  if (n_na == length(arg)) {
    cli::cli_abort(
      "{.arg {arg_nm}} cannot be {.code NA}: it has {n_na} {.code NA}{?s}."
    )
  }

  if (all(range == c(0, Inf))) {
    msg <- c(i = "{.arg {arg_nm}} must be positive.")
  } else if (all(range == c(-Inf, 0))) {
    msg <- c(i = "{.arg {arg_nm}} must be negative.")
  } else {
    msg <- c(i = "{.arg {arg_nm}} must be inside [{range[1]}, {range[2]}].")
  }

  if (!allow_na && n_na > 0) {
    msg <- c(
      msg,
      i = "{.arg {arg_nm}} cannot have {.code NA}s: it has {n_na} {.code NA}{?s}."
    )
  }

  cli::cli_abort(c(
    "The {.arg {arg_nm}} argument has {n_oob} out-of-bounds value{?s}.",
    msg
  ))
}

is_blank <- function(x) {inherits(x, "element_blank")}

prtct_zlen <- function(x) {
  if (length(x) == 0) return(NULL) else x
}

#' @importFrom vctrs data_frame
data_frame0 <- function(...) {
  data_frame(..., .name_repair = "minimal")
}
