# Constructor -------------------------------------------------------------

#' 'Plus' grid guide
#'
#' This guide draws a panel grid, but only displays the grid lines at
#' points where they intersect. These grid lines then look like the '+' symbol.
#'
#' @param length A `numeric(1)` for the length of each arm of the plus symbol
#'   in millimetres. Can be `Inf` to display regular lines.
#' @param length_major,length_minor Inherits from the `length` argument to
#'   specify the lengths for major and minor grid lines.
#' @param length_x_major,length_y_major Inherits from the `length_major`
#'   argument to specify the major x- and y-grid lines.
#' @param length_x_minor,length_y_minor Inherits from the `length_minor`
#'   argument to specify the minor x- and y-grid lines.
#' @param edge A `logical(1)` whether to draw grid lines at the panel
#'   edge (`TRUE`, default) or not (`FALSE`).
#' @param edge_x,edge_y Inherits from the `edge` argument for the x- and
#'   y-gridlines specifically.
#' @inheritDotParams guide_grid
#'
#' @return A `<Guide>` ggproto object that can be given to the
#'   [`guides()`][ggplot2::guides()] function, or set as the `guide` argument
#'   in [`coord_guided()`].
#' @export
#' @family grid guide variants
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy)) +
#'   geom_point()
#'
#' # A 'plus' grid
#' p + coord_guided(guide = "grid_plus")
#'
#' # Setting larger plusses
#' p + coord_guided(guide = guide_grid_plus(length = 4))
#'
#' # Drawing one direction as regular lines
#' p + coord_guided(guide = guide_grid_plus(length_y_major = Inf))
#'
#' # Omitting the ticks at the edge
#' p + coord_guided(guide = guide_grid_plus(edge = FALSE))
#'
#' # Turning off the minor gridlines
#' p + coord_guided(guide = guide_grid_plus(minor_breaks = NULL))
guide_grid_plus <- function(
  length = 1,
  length_major = NULL,
  length_minor = NULL,
  length_x_major = NULL,
  length_x_minor = NULL,
  length_y_major = NULL,
  length_y_minor = NULL,
  edge   = TRUE,
  edge_x = NULL,
  edge_y = NULL,
  ...
) {

  length_x_major <- length_x_major %||% length_major %||% length
  length_y_major <- length_y_major %||% length_major %||% length
  length_x_minor <- length_x_minor %||% length_minor %||% length
  length_y_minor <- length_y_minor %||% length_minor %||% length

  check_number_decimal(length_x_major)
  check_number_decimal(length_y_major)
  check_number_decimal(length_x_minor)
  check_number_decimal(length_y_minor)

  edge_x <- edge_x %||% edge
  edge_y <- edge_y %||% edge

  check_bool(edge_x)
  check_bool(edge_y)

  guide_grid(
    length_x_major = length_x_major,
    length_y_major = length_y_major,
    length_x_minor = length_x_minor,
    length_y_minor = length_y_minor,
    edge_x = edge_x,
    edge_y = edge_y,
    ...,
    super = GuideGridPlus
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideGridPlus <- ggproto(
  NULL, GuideGrid,

  params = c(GuideGrid$params, list(
    length_x_major = 2,
    length_y_major = 2,
    length_x_minor = 2,
    length_y_minor = 2,
    edge_x = TRUE,
    edge_y = TRUE
  )),

  build_ticks = function(key, elements, params, position = params$position) {

    all_x <- union(key$x_major$x, key$x_minor$x)
    all_y <- union(key$y_major$y, key$y_minor$y)

    if (params$edge_x) {
      all_y <- unique(c(0, 1, all_y))
    }
    if (params$edge_y) {
      all_x <- unique(c(0, 1, all_x))
    }

    x_major <- plus_grid(elements$x_major, key$x_major$x, all_y,
                         params$length_x_major, aes = "x")
    x_minor <- plus_grid(elements$x_minor, key$x_minor$x, all_y,
                         params$length_x_minor, aes = "x")
    y_major <- plus_grid(elements$y_major, key$y_major$y, all_x,
                         params$length_y_major, aes = "y")
    y_minor <- plus_grid(elements$y_minor, key$y_minor$y, all_x,
                         params$length_y_minor, aes = "y")

    grobTree(x_major, x_minor, y_major, y_minor)
  }
)

plus_grid <- function(element, main, alt, length, aes = "x") {

  if (is_blank(element) || length(main) == 0 || length(alt) == 0) {
    return(NULL)
  }
  if (is.infinite(length)) {
    alt <- c(0, 1)
  }
  orth <- setdiff(c("x", "y"), aes)
  pos <- list(
    main = rep(main, each = length(alt)),
    alt  = rep(alt, times = length(main))
  )
  if (is.finite(length)) {
    lengths  <- rep(2, length(pos$main))
    pos$main <- unit(rep(pos$main, each = 2), "npc")
    pos$alt  <- unit(rep(pos$alt, each = 2), "npc") +
      unit(rep(c(-1, 1) * length), "mm")
  } else {
    lengths <- rep(2, length(main))
  }
  pos <- list2(
    !!aes  := pos$main,
    !!orth := pos$alt
  )
  element_grob(element, x = pos$x, y = pos$y, id.lengths = lengths)
}
