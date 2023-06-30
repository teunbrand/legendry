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
  if (!is_missing(arg) && inherits(arg, class)) {
    return(invisible())
  }
  if (is_missing(arg)) {
    friendly <- "absent"
  } else {
    friendly <- "{.obj_type_friendly {arg}}"
  }
  class <- paste0("{.cls ", class, "}")
  class <- glue_collapse(class, sep = ", ", last = " or ")
  msg <- paste0(
    "{.arg {arg_nm}} must be of the class ", class, ", not ", friendly, "."
  )
  cli::cli_abort(msg, call = error_call)
}

arg_range <- function(
  arg, range, allow_na = FALSE, arg_nm = caller_arg(arg),
  error_call = caller_env()
) {
  arg_class(arg, c("numeric"), arg_nm = arg_nm, error_call = error_call)

  oob  <- arg < range[1] | arg > range[2]
  isna <- is.na(arg)
  oob[isna] <- !allow_na

  if (!any(oob)) {
    return(arg)
  }
  n_oob <- sum(oob)
  n_na  <- sum(isna)

  abort_if(
    n_na == length(arg),
    "{.arg {arg_nm}} cannot be {.code NA}: it has {n_na} {.code NA}{?s}."
  )

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

is_discrete <- function(x) {
  inherits(x, "mapped_discrete") ||
    is.factor(x) || is.character(x) || is.logical(x)
}

allow_lambda <- function(x) {
  if (is_formula(x)) {
    as_function(x)
  } else {
    x
  }
}

prtct_zlen <- function(x) {
  if (length(x) == 0) return(NULL) else x
}

#' @importFrom vctrs data_frame
data_frame0 <- function(...) {
  data_frame(..., .name_repair = "minimal")
}

combine_element_list <- function(elem, parent, type) {
  if (inherits(elem, type)) {
    elem <- list(elem)
  }
  lapply(elem, combine_elements, e2 = parent)
}

absolute_element <- function(element, ..., width, height) {
  gTree(
    children = gList(
      exec(
        element_grob,
        element = element,
        ...
      )
    ),
    width = width, height = height,
    xmin = NULL, ymin = NULL, vp = NULL,
    cl = "absoluteGrob"
  )
}

as_cli <- function(..., .envir = parent.frame()) {
  cli::cli_fmt(cli::cli_text(do.call(paste0, list(...)), .envir = .envir))
}

.trbl <- c("top", "right", "bottom", "left")



arg_null_or_match <- function(
  arg, values, arg_nm = caller_arg(arg), error_call = caller_env()
) {
  if (is.null(arg)) {
    return(arg)
  }
  arg_match0(arg, values, arg_nm, error_call)
}

abort_if <- function(test, ..., i = character(), .envir = parent.frame()) {
  if (!test) {
    return(invisible())
  }
  cli::cli_abort(
    c(as_cli(..., .envir = .envir),
      setNames(i, rep("i", length(i)))),
    .envir = .envir
  )
}

eval_aes <- function(data, mapping, valid,
                     call = caller_env(),
                     mapping_arg = caller_arg(mapping),
                     data_arg = caller_arg(data)) {
  if (!inherits(mapping, "uneval")) {
    cli::cli_abort(
      "{.arg {mapping_arg}} must be an aesthetic mapping created by {.fn aes}."
    )
  }
  arg_class(data, "data.frame", data_arg, call)

  evaled <- lapply(mapping, eval_tidy, data = data)
  sizes  <- list_sizes(evaled)
  evaled <- evaled[sizes > 0]
  extra_nms <- setdiff(names(evaled), valid)
  if (length(extra_nms) > 0) {
    cli::cli_warn(
      "Ignoring unknown aesthetics: {.field {extra_nms}}.",
      call = call
    )
  }
  evaled <- evaled[intersect(names(evaled), valid)]
  sizes <- list_sizes(evaled)
  if (length(sizes) == 0) {
    cli::cli_warn(
      "No valid data found with {.arg {mapping_arg}} in {.arg {data_arg}}.",
      call = call
    )
    return(data_frame0())
  }
  data_frame(
    !!!evaled, .size = max(sizes),
    .name_repair = "minimal", .error_call = call
  )
}

# Updating ----------------------------------------------------------------

update_element <- function(element, ..., .envir = caller_env()) {
  if (is_blank(element)) {
    return(element)
  }
  args <- match.call(expand.dots = FALSE)$`...`
  update_list(element, args = args, .envir = .envir)
}

update_with_defaults <- function(list = list(), ..., .envir = caller_env()) {
  args <- match.call(expand.dots = FALSE)$`...`
  update_list(list, args = args, .envir = .envir)
}

update_list <- function(list = list(), args = list(), .envir = caller_env()) {
  nms <- names(args)
  nms <- nms[!is.na(nms) & nms != ""]
  for (name in nms) {
    value <- list[[name]] %||% eval(args[[name]], envir = .envir)
    if (is.null(value)) {
      next
    }
    list[[name]] <- value
  }
  list
}

# Run length encoding utilities -------------------------------------------

new_rle <- function(x = NULL, lengths = NULL, alt = NULL) {

  if (!is.null(lengths)) {
    new_rcrd(
      list(
        group  = seq_along(lengths),
        length = as.integer(lengths)
      ),
      n     = length(lengths),
      class = "vctrs_group_rle"
    )
  } else if (!is.null(x)) {
    vec_group_rle(x)
  } else {
    new_rcrd(
      list(
        group  = 1L,
        length = alt
      ),
      n     = 1L,
      class = "vctrs_group_rle"
    )
  }
}

rle_end <- function(rle) {
  cumsum(field(rle, "length"))
}

rle_start <- function(rle) {
  c(1L, rle_end(rle)[-length(rle)] + 1L)
}

rle_inv <- function(rle) {
  rep.int(field(rle, "group"), field(rle, "length"))
}
