


resolve_key <- function(x) {
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
  if (inherits(x, "key")) {
    return(x)
  }
  cli::cli_abort("Unknown key specification: {x}.")
}

transform_key <- function(key, position, coord, panel_params) {
  other <- switch(position, bottom = , left = , theta.sec = -Inf, Inf)
  key$x <- key$x %||% other
  key$y <- key$y %||% other
  transformed <- coord$transform(key, panel_params)

  if (!position %in% c("theta", "theta.sec")) {
    return(transformed)
  }

  add <- if (position == "theta.sec") pi else 0
  transformed$theta <- transformed$theta + add

  if ("xend" %in% names(key)) {
    key <- rename(key, c("x", "xend"), rev)
  } else if ("yend" %in% names(key)) {
    key <- rename(key, c("y", "yend"), rev)
  } else {
    return(transformed)
  }
  key <- coord$transform(key, panel_params)
  transformed$thetaend <- key$theta + add
  transformed
}
