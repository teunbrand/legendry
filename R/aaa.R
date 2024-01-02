
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
