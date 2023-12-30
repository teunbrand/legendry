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
NULL
