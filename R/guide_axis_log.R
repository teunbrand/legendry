# Public constructor ------------------------------------------------------

#' Axis guide with ticks for logarithmic breaks
#'
#' This axis guide makes log tick marks with diminishing spacing. This way of
#' showing tick marks probably best suit log10 transformations only.
#'
#' @param base A `numeric(1)` setting the base of the log transform. This
#'   defaults to `10` and needs to be set to a different number for log
#'   transformations other than log10. Since tick mark calculations work best
#'   for the log10 transform, it is probably left as-is.
#' @param pre_scaled A `logical(1)` which if `FALSE` (default), assumes the
#'   scale hosts the log transformation. Set this to `TRUE` if the values
#'   are already log transformed before seen by the scale.
#' @param minor_size,mini_size A `numeric(1)` giving the relative size of the
#'   minor axis ticks, and 'mini' axis ticks respectively, in relation to the
#'   plot's `axis.ticks.length.{x/y}.{position}` theme setting.
#' @inheritParams guide_axis_ext
#'
#' @inherit guide_axis_vanilla return
#' @export
#' @family axis variants
#'
#' @details Due to the way log transformation are implemented, it can happen
#'   that automatically calculated breaks fall in between powers. This might
#'   look a bit awkward, but can be prevented by providing appropriate breaks.
#'
#' @examples
#' # The axis is paired directly with a log scale
#' ggplot(pressure, aes(temperature, pressure)) +
#'   geom_line() +
#'   scale_y_continuous(trans = "log10", guide = "axis_log")
#'
#' # You can use `pre_scaled = TRUE` when the log transformation happens
#' # outside the scale
#' ggplot(pressure, aes(temperature, log10(pressure))) +
#'   geom_line() +
#'   scale_y_continuous(guide = guide_axis_log(pre_scaled = TRUE))
#'
#' # It makes less sense to use this guide with log transformations where
#' # the base is not 10.
#' ggplot(pressure, aes(temperature, pressure)) +
#'   geom_line() +
#'   scale_y_continuous(trans = "log2", guide = guide_axis_log(base = 2))
guide_axis_log <- function(
  base       = 10,
  pre_scaled = FALSE,
  minor_size = 0.75,
  mini_size  = 0.50,
  major_size = NULL,
  ...
) {
  guide_axis_ext(
    minor_size = minor_size,
    mini_size  = mini_size,
    major_size = major_size,
    pre_scaled = pre_scaled,
    base       = base,
    ...,
    super = GuideAxisLog
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideAxisLog <- ggproto(
  "GuideAxisLog", GuideAxisExt,

  calc_mini = function(self, breaks, scale, size) {
    self$params$logticks
  },

  calc_minor = function(self, breaks, scale, size) {
    limits <- scale$continuous_range
    base   <- self$params$base

    ticks_per_base <- base - 1
    minpow = floor(limits[1])
    maxpow = ceiling(limits[2])

    reps <- maxpow - minpow
    tick_nums <- rep(seq(1, base - 1, length.out = ticks_per_base), reps)
    powers    <- rep(seq(minpow, maxpow - 1), each = ticks_per_base)

    ticks <- c(tick_nums * base ^ powers, base ^ maxpow)

    cycle_idx <- tick_nums - 1
    minor_idx <- floor(ticks_per_base / 2)
    minor_idx <- which(cycle_idx == minor_idx)

    if (!self$params$pre_scaled) {
      ticks <- scale$scale$trans$transform(ticks)
    } else {
      ticks <- log(ticks, base = base)
    }
    oob <- ticks <= limits[1] | ticks >= limits[2]
    self$params$logticks <- ticks[!oob]
    return(ticks[setdiff(minor_idx, which(oob))])
  }
)



