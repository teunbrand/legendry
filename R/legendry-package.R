#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import ggplot2
#' @import grid
#' @import rlang
#' @import vctrs
#' @import gtable
#' @import scales
## usethis namespace: end
NULL

#' @name gguidance_extensions
#' @title `ggproto` objects in \pkg{gguidance}
#'
#' @description The \pkg{gguidance} package relies on an extension system of
#'   \pkg{ggplot2} through [`ggproto`][ggplot2::ggproto] class objects, which
#'   allow cross-package inheritance of objects such as geoms, stats, facets,
#'   scales and coordinate systems. For the purpose of making plots, users are
#'   invited to wholly ignore these objects, since interacting with these
#'   objects is preferred through various constructor functions. The
#'   \pkg{gguidance} package introduces a new `<Guide>` ggproto class to support
#'   variations on axes, legends and colourbars.
#'
#' @seealso The documentation over at [`ggproto`][ggplot2::ggproto()].
#' @keywords internal
NULL

#' @name common_parameters
#' @title common parameters in \pkg{gguidance}
#'
#' @description
#' This is a collection of common parameters so they needn't be re-documented
#' each time.
#'
#' @param title
#' A `<character[1]>` or `<expression[1]>` indicating the title of
#' the guide. If `NULL`, the title is not shown. The default,
#' [`waiver()`][ggplot2::waiver()], takes the name of the scale object or
#' the name specified in [`labs()`][ggplot2::labs] as the title.
#'
#' @param theme
#' A [`<theme>`][ggplot2::theme] object to style the guide individually or
#' differently from the plot's theme settings. The `theme` argument in the
#' guide overrides and is combined with the plot's theme.
#'
#' @param position
#' A `<character[1]>` giving the location of the guide. Can be one of `"top"`,
#' `"bottom"`, `"left"` or `"right"`.
#'
#' @param order
#' A positive `<integer[1]>` that specifies the order of this guide among
#' multiple guides. This controls in which order guides are merged if there
#' are multiple guides for the same position. If `0` (default), the order is
#' determined by a hashing indicative settings of a guide.
#'
#' @param available_aes
#' A `<character>` vector listing the aesthetics for which this guide can
#' be build.
#'
#' @param direction
#' A `<character[1]>` indicating the direction of the guide. Can be on of
#' `"horizontal"` or `"vertical"`.
#'
#' @param angle
#' A specification for the text angle. Compared to setting the `angle` argument
#' in [`element_text()`][ggplot2::element_text], this argument uses some
#' heuristics to automatically pick the `hjust` and `vjust` that you
#' probably want. Can be one of the following:
#' * `NULL` to take angles and justification settings directly from the theme.
#' * [`waiver()`][ggplot2::waiver] to allow reasonable defaults in special
#'   cases.
#' * A `<numeric[1]>` between -360 and 360 for the text angle in degrees.
#'
#' @keywords internal
NULL
