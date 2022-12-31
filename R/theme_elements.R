#' List of theme elements
#'
#' This is a convenience function for constructing a list of
#' [text theme elements][ggplot2::element_text()]. It can take vector input for
#' arguments and pass on the \ifelse{html}{\out{i<sup>th</sup>}}{\eqn{i^{th}}}
#' value of the vector to the \ifelse{html}{\out{i<sup>th</sup>}}{\eqn{i^{th}}}
#' call of a function.
#'
#' @inheritParams ggplot2::element_text
#'
#' @details `NA` and `NULL` values will be silently dropped. If you want to pass
#'   on a transparent `colour` argument, it is recommended to use
#'   `"transparent"` instead. The dropping of `NA`s serve the purpose that you
#'   can skip providing an argument to the text element at the positions where
#'   `NA`s occur.
#'
#' @return A `list` of `<element_text>` objects.
#' @export
#'
#' @examples
#' # Simply have different colours for text
#' elements_text(colour = c("blue", "red"))
#'
#' # More complicated case
#' elements_text(
#'   # 2nd text will *not* have a family set
#'   family = c("mono", NA, "sans"),
#'   # Arguments that don't take scalars can take lists.
#'   margin = list(NULL, margin(t = 5))
#' )
elements_text <- function(
  family        = NULL,
  face          = NULL,
  colour        = NULL,
  size          = NULL,
  hjust         = NULL,
  vjust         = NULL,
  angle         = NULL,
  lineheight    = NULL,
  color         = NULL,
  margin        = NULL,
  debug         = NULL,
  inherit.blank = FALSE
) {
  distribute_args(
    family        = family,
    face          = face,
    colour        = colour,
    size          = size,
    hjust         = hjust,
    vjust         = vjust,
    angle         = angle,
    lineheight    = lineheight,
    color         = color,
    margin        = margin,
    debug         = debug,
    inherit.blank = inherit.blank
  )
}

distribute_args <- function(..., .fun = element_text, .cull = TRUE) {
  # Intersect with formals of function
  args <- list(...)
  fun_args <- names(formals(.fun))
  if (.cull) {
    args <- args[intersect(names(args), fun_args)]
  }

  # Trim zero-length arguments
  args <- args[lengths(args) > 0]

  # Early exit if no arguments
  if (length(args) == 0) {
    return(.fun())
  }

  # Measure arguments
  nms  <- names(args)
  len  <- length(args)
  lens <- lengths(args)

  # Format args as matrix of arguments
  m <- matrix(list(NULL), nrow = len, ncol = max(lens))
  idx <- cbind(
    rep(seq_len(len), lens),
    unlist(lapply(lens, seq), use.names = FALSE)
  )
  m[idx] <- unlist(lapply(args, as.list), FALSE, FALSE)

  # Replace NAs with NULLs
  is_vec <- vapply(m, is.vector, logical(1))
  m <- clapply(m, is_vec, function(x) {
    if (any(is.na(x))) NULL else x
  })

  # Distribute arguments to function by column
  apply(m, 2, function(arg) {
    arg <- setNames(arg, nms)
    arg <- arg[lengths(arg) > 0]
    do.call(.fun, arg)
  })
}

# Theme unions ------------------------------------------------------------

.text_or_blank <- c("element_text", "element_blank")
.line_or_blank <- c("element_line", "element_blank")
.rect_or_blank <- c("element_rect", "element_blank")

