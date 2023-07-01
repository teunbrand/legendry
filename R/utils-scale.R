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
      get_breaks_minor = minor_no_discard,
      get_breaks = major_early_exit
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

# Exits early if break function returns waiver
major_early_exit = function(self, limits = self$get_limits()) {

  if (self$is_empty()) {
    return(numeric())
  }
  domain <- suppressWarnings(self$trans$transform(self$trans$domain))
  domain <- sort(domain)
  if (length(domain) == 2 && !scales::zero_range(domain)) {
    limits <- scales::oob_squish(limits, domain)
  }
  limits <- self$trans$inverse(limits)
  if (is.null(self$breaks)) {
    return(NULL)
  }
  if (identical(self$breaks, NA)) {
    cli::cli_abort(
      "Invalid {.arg breaks} specification. Uses {.val NULL}, not {.val NA}."
    )
  }
  if (scales::zero_range(as.numeric(limits))) {
    breaks <- limits[1]
  } else if (inherits(self$breaks, "waiver")) {
    if (!is.null(self$n.breaks) &&
        ("n" %in% names(formals(self$trans$breaks)))) {
      breaks <- self$trans$breaks(limits, self$n.breaks)
    } else {
      if (!is.null(self$n.breaks)) {
        cli::cli_warn(paste0(
          "Ignoring {.arg n.breaks}. Use a {.cls trans} object that supports ",
          "setting number of breaks."
        ))
      }
      breaks <- self$trans$breaks(limits)
    }
  } else if (is.function(self$breaks)) {
    breaks <- self$breaks(limits)
    if (inherits(breaks, "waiver")) {
      return(breaks)
    }
  } else {
    breaks <- self$breaks
  }

  breaks <- self$trans$transform(breaks)
  breaks <- scales::censor(breaks, self$trans$transform(limits),
                           only.finite = FALSE)
  breaks
}

