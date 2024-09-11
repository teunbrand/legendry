
new_params <- function(...) {
  required <- list(
    title = waiver(),
    theme = NULL,
    name  = character(),
    position = waiver(),
    direction = NULL,
    order = 0L,
    hash = character(0)
  )
  dots <- list2(...)
  for (i in names(dots)) required[i] <- dots[i]
  required
}

.trbl <- c("top", "right", "bottom", "left")
.trblt <- c(.trbl, c("theta", "theta.sec"))

standard_extract_key <- function(scale, aesthetic, key, ...) {
  key <- resolve_key(key %||% "auto")
  if (inherits(key, "key_guide") && !inherits(key, "key_standard")) {
    return(key) # probably not a standard key, no need to treat
  }
  if (is.function(key)) {
    key <- key(scale, aesthetic)
  }
  if (is_empty(key)) {
    return(key)
  }
  if ("aesthetic" %in% names(key)) {
    key$aesthetic <-
      scale_transform(key$aesthetic, scale, map = TRUE, "aesthetic")
    key$.value <-
      scale_transform(key$.value, scale, map = FALSE, "value")
  }
  key <- rename(key, "aesthetic", aesthetic)
  key <- validate_key_types(key)

  if (is.numeric(key$.value)) {
    range <- scale$continuous_range %||% scale$get_limits()
    key <- vec_slice(key, is.finite(oob_censor_any(key$.value, range)))
  }

  key
}
