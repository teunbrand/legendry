# Helper function for extracting breaks from scales.
scale_grid_extract <- function(major = waiver(), minor = waiver(), scale) {

  default_major <- scale$get_breaks()
  default_minor <- scale$get_breaks_minor()

  if (scale$is_discrete()) {

    major  <- major %|W|% default_major
    minor  <- minor %|W|% default_minor
    limits <- scale$get_limits()

    if (!is.function(major) && !is.numeric(major)) {
      new_scale <- ggproto(
        NULL, scale$scale,
        breaks = major
      )
      major <- new_scale$get_breaks()
    } else if (is.function(major)) {
      major <- major(limits)
    }
    if (is.function(minor)) {
      minor <- minor(limits)
    }
  } else {
    inverse <- scale$scale$trans$inverse

    major <- major %|W|% inverse(default_major)
    minor <- minor %|W|% inverse(default_minor)

    new_scale <- ggproto(
      NULL, scale$scale,
      breaks = major, minor_breaks = minor,
      limits = scale$continuous_range,
      get_breaks_minor = minor_no_discard
    )

    major <- new_scale$get_breaks() %|W|% default_major
    minor <- new_scale$get_breaks_minor() %|W|% default_minor
  }

  major <- scale$map(major)
  minor <- scale$map(minor)

  major <- major[!is.na(major)]
  minor <- minor[!is.na(minor)]

  list(
    major = major,
    minor = setdiff(minor, major)
  )
}

# This is a replacement for ScaleContinuous$get_breaks_minor.
# The only difference is that at the end, it doesn't `discard()` out-of-bounds
# values.
minor_no_discard <- function(self, n = 2, b = self$break_position(),
                             limits = self$get_limits()) {
  if (scales::zero_range(as.numeric(limits))) {
    return()
  }
  if (is.null(self$minor_breaks)) {
    return(NULL)
  }
  if (identical(self$minor_breaks, NA)) {
    cli::cli_abort(paste0(
      "Invalid {.arg minor_breaks} specification. Use {.val NULL}, ",
      "not {.val NA}"
    ))
  }
  if (inherits(self$minor_breaks, "waiver")) {
    if (is.null(b)) {
      breaks <- NULL
    } else {
      breaks <- self$trans$minor_breaks(b, limits, n)
    }
  } else if (is.function(self$minor_breaks)) {
    breaks <- self$minor_breaks(self$trans$inverse(limits))
    breaks <- self$trans$transform(breaks)
  } else {
    breaks <- self$trans$transform(self$minor_breaks)
  }
  breaks
}
