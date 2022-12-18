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

is_blank <- function(x) {inherits(x, "element_blank")}

prtct_zlen <- function(x) {
  if (length(x) == 0) return(NULL) else x
}
