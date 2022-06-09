#' @keywords internal
"_PACKAGE"

#' @import ggplot2
#' @import grid
#' @import rlang
NULL

## usethis namespace: start
#' @importFrom glue glue
#' @importFrom glue glue_collapse
#' @importFrom gtable gtable
#' @importFrom gtable gtable_add_cols
#' @importFrom gtable gtable_add_grob
#' @importFrom gtable gtable_add_rows
#' @importFrom gtable gtable_col
#' @importFrom gtable gtable_height
#' @importFrom gtable gtable_row
#' @importFrom gtable gtable_width
#' @importFrom lifecycle deprecated
#' @importFrom scales oob_censor_any
#' @importFrom scales oob_squish
#' @importFrom scales regular_minor_breaks
#' @importFrom scales rescale
#' @importFrom stats setNames
#' @importFrom utils head
#' @importFrom vctrs new_data_frame
#' @importFrom vctrs vec_assert
#' @importFrom vctrs vec_interleave
#' @importFrom vctrs vec_ptype_full
#' @importFrom vctrs vec_slice
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
