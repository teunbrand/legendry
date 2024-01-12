# Constructor -------------------------------------------------------------

#' Subtitle with coloured phrases
#'
#' This guides formats a piece of text with colours determined from the scale.
#'
#' @param title A `<character[1]>` containing text to use as subtitle. Text
#'   formatting is discussed in details.
#' @param open,close A `<character[1]>` delimiter indicating where colour
#'   formatting begins (`open`) and ends (`close`).
#' @inheritParams common_parameters
#'
#' @details
#' ## Text formatting
#' Chunks of text are formatted as follows: the title is chopped up into
#' pieces split by the `open` and `close` delimiters. When the `open` delimiter
#' is followed by an integer, that integer is matched up against the scale's
#' breaks. The piece of text between that `open` delimiter and the associated
#' `close` delimiter is that coloured with the break's colour.
#'
#' For example, in the title
#' `"A {.1 quick} brown {.3 fox} jumps over the {.2 lazy dog}."`, the word
#' `"quick"` is given the scale's first colour, `"fox"` is given the third
#' colour and `"lazy dog"` is given the second colour. The first space after
#' the integer index gets trimmed.
#'
#' When there is no text inside the braces other than the index, the scale's
#' labels are inserted as text.
#'
#' While implemented as a legend guide, it takes style options from the
#' `plot.subtitle` theme element. As it is not a true subtitle, there might
#' be complications if other guides co-occupy the `"top"` legend position.
#'
#' ## Right-to-left scripts
#' The typesetting of this guide is primitive and only concerns itself with
#' placing text pieces, not individual glyphs. The only consideration given
#' to right-to-left script is that a line is converted from LtR to RtL when
#' all pieces of text on a line contain characters from RtL character sets.
#' Bidirectional text is given no consideration within this function. Glyphs
#' that should have ligatures in normal text, but are in separate pieces will
#' probably not render as ligatures. The task of actually rendering what is
#' within pieces of text, is handled by the graphics device, which have varying
#' degrees of modern text feature support. The author of this function,
#' who only knows LtR natural languages, profusely apologises for this
#' inconvenience.
#'
#' @return A `<Guide>` object.
#' @export
#' @family standalone guides
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
#'   geom_point()
#'
#' # Typical use case
#' p + scale_colour_discrete(
#'   name = "Cars with {.1 four}, {.2 five}, {.3 six} or {.4 eight} cylinders.",
#'   guide = "subtitle"
#' )
#'
#' # If there is no text in between the delimiters, the scale's `labels` are
#' # substituted.
#' p + scale_colour_discrete(
#'   labels = c("FOUR", "5", "Six", "foobar"),
#'   name   = "Cars with {.1}, {.2}, {.3} or {.4} cylinders.",
#'   guide  = "subtitle"
#' )
#'
#' # Using different text delimiters
#' p + guides(colour = guide_subtitle(
#'   "Cars with <1 four>, <2 five>, <3 six> or <4 eight> cylinders.",
#'   open = "<", close = ">"
#' ))
#'
#' # # For use with \{ggtext\}, mix html with the open and closing delimiters.
#' # # Leave out the '\' before quotation marks, it didn't document without
#' # # these backslashes :(
#' # p +
#' #   scale_colour_discrete(
#' #     name  = \"Cars with {.1}, <b>{.2 five}</b>, {.3} or {.4} <i>cylinders</i>.\",
#' #     guide = "subtitle"
#' #   ) +
#' #   theme(plot.subtitle = ggtext::element_markdown())
guide_subtitle <- function(
  title = waiver(),
  open  = "{.",
  close = "}",
  theme = NULL
) {
  check_string(open)
  check_string(close)

  new_guide(
    title = title,
    open  = open,
    close = close,
    available_aes = c("colour", "fill"),
    order = 1,
    position = "top",
    theme = theme,
    super = GuideSubtitle
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GuideSubtitle <- ggproto(
  "GuideSubtitle", Guide,

  params = new_params(open = "{.", close = "}"),

  elements = list(title = "plot.subtitle", spacing = "legend.box.spacing"),

  extract_params = function(scale, params, title = waiver(), ...) {
    params$title <- scale$make_title(params$title %|W|% scale$name %|W|% title)
    check_string(params$title, allow_null = TRUE, arg = "title")
    params
  },

  setup_elements = function(params, elements, theme) {
    theme <- theme + params$theme
    title <- calc_element("plot.subtitle", theme)
    spacing <- calc_element("legend.box.spacing", theme)
    i <- match(opposite_position(params$position), .trbl)
    title$margin[i] <- title$margin[i] - spacing
    title
  },

  draw = function(self, theme, position = NULL, direction = NULL,
                  params = self$params) {

    direction <- params$direction %||% direction %||%
      switch(position, top = , bottom = "horizontal", "vertical")

    params <- replace_null(params, position = position, direction = direction)
    elem   <- self$setup_elements(params, self$elements, theme)

    # Extract parameters
    open  <- params$open
    close <- params$close
    key   <- params$key

    # Process string
    chunks <- chunk_string(params$title, open, close)
    string <- chunks$string

    # Remove open and close tokens
    i <- chunks$depth > 0
    string[i] <- clean_tokens(string[i], open, close)

    # Match chunks to key
    i <- !duplicated(chunks$group) & chunks$depth > 0
    matches <- rep(0, nrow(chunks))
    matches[i] <- as.integer(matched_pattern(string[i], "^[0-9]+", "0"))
    matches <- vec_ave(matches, chunks$group, max)
    matches[matches > nrow(params$key)] <- NA

    # Clean matching integers from string
    string[i] <- gsub("^[0-9]+[ ]{0,1}", "", string[i])

    # Replace empty strings with key label
    empty <- i & !nzchar(string)
    string[empty] <- key$.label[matches[empty]]
    matches[is.na(matches)] <- 0

    # Setup text parameters
    colours <- elem$colour
    colours <- c(colours, key$colour %||% key$fill %||% rep(colours, nrow(key)))
    colours <- colours[matches + 1]

    chunks$matches <- matches
    chunks$colour  <- colours
    chunks$string  <- string

    if (inherits(elem, c("element_markdown", "element_textbox"))) {
      # We just let ggtext take care of this
      grob <- element_grob(elem, label = format_ggtext(chunks, elem))
    } else {
      grob <- typeset_chunks(chunks, elem)
    }

    gt <- switch(
      direction,
      horizontal =
        gtable(widths = unit(1, "npc"), heights = unit(height_cm(grob), "cm")),
      vertical =
        gtable(widths = unit(width_cm(grob), "cm"), heights = unit(1, "npc"))
    )
    gt <- gtable_add_grob(gt, grob, t = 1, l = 1, clip = "off", name = "title_guide")
    gt
  }
)

# Helpers -----------------------------------------------------------------

typeset_chunks <- function(chunks, element) {
  if (element$angle != 0) {
    cli::cli_warn("{.fn guide_subtitle} cannot typeset angled text.")
  }

  hjust <- element$hjust
  vjust <- element$vjust

  margin <- width_cm(element$margin[c(2, 4)])
  width  <- get_text_dim_cm(chunks$string, element, "width")

  xmax <- vec_ave(width, chunks$line, cumsum)
  xmin <- vec_ave(xmax,  chunks$line, function(x) c(0, x[-length(x)]))

  width_line <- vec_ave(xmax, chunks$line, max)
  width_max  <- max(width_line)

  rtl <- grepl(rtl_charsets, chunks$string)
  rtl <- vec_ave(rtl, chunks$line, all)

  x <- (xmax + xmin) / 2 + (width_max - width_line) * hjust
  x[rtl] <- width_line[rtl] - x[rtl]
  x <- margin[2] * (1 - hjust) - (margin[1] + width_max) * hjust + x
  x <- unit(hjust, "npc") + unit(x, "cm")

  max_lines <- max(chunks$line)
  ymin <- chunks$line - 1L
  ymax <- max_lines - chunks$line
  y <- ymax * (1 - vjust) - ymin * vjust
  y <- unit(vjust, "npc") + unit(y, "lines")

  element_grob(
    element, label = chunks$string,
    x = x, y = y,
    vjust = vjust, hjust = 0.5,
    lineheight = element$lineheight * 1.2,
    colour = chunks$colour,
    margin_x = FALSE, margin_y = TRUE
  )
}

chunk_string <- function(string, open = "{.", close = "}") {

  lines  <- strsplit(string, "\n", fixed = TRUE)[[1]]
  nchars <- nchar(lines)

  chunks <- character()
  line_index <- integer()

  # Find positions of opening and closing tokens
  open_list  <- gregexpr(open,  lines, fixed = TRUE)
  close_list <- gregexpr(close, lines, fixed = TRUE)

  close_list <- lapply(close_list, function(x) x + x %@% "match.length" - 1L)

  for (line in seq_along(lines)) {
    # Get opening and closing positions for this line
    opening <- .subset2(open_list,  line)
    opening <- opening[opening > 0]
    closing <- .subset2(close_list, line)
    closing <- closing[closing > 0]

    # Include true line starts and ends as well
    start <- sort(c(1, opening, closing + 1L))
    end   <- sort(c(opening - 1L, closing, nchars[line]))

    # Split line into parts
    parts <- substr(rep(lines[line], length(start)), start, end)
    parts <- parts[nzchar(parts)]

    # Append parts to earlier parts and add line index
    chunks <- c(chunks, parts)
    line_index <- c(line_index, rep(line, length(parts)))
  }

  # Annotate for every part whether it has a opening or closing token
  has_open  <- startsWith(chunks, open)
  has_close <- endsWith(chunks, close)
  # start <- seq_along(chunks) * 2L - 1L

  # Calculate nesting depths:
  # increase after every opening, decrease after every closing
  # cumulative sum gives depth
  depth <- vec_interleave(as.numeric(has_open), as.numeric(has_close) * -1)
  depth <- vec_ave(depth, rep(line_index, each = 2), cumsum)

  # Compute groups based on depth
  # Chunks have the same group if interrupted by higher depth and groups are
  # broken up by lower depths. For example if we have the following sequence
  # of depths:
  # 1 2 3 2 3 2 1 2
  # We have the following groups
  # 1 2 4 2 5 2 1 3
  group <- rep(0, length(depth))
  max_group <- 0
  for (i in sort(unique(depth))) {
    run <- rle(depth >= i)
    run$values[run$values] <- seq_along(run$values[run$values]) + max_group
    group <- group + rep(run$values, run$lengths)
    max_group <- max(group)
  }
  group <- match_self(group)
  start <- seq_along(chunks) * 2L - 1L

  data_frame0(
    string = chunks,
    line   = line_index,
    depth  = depth[start],
    group  = group[start]
  )
}

clean_tokens <- function(x, open, close) {
  i <- startsWith(x, open)
  x[i] <- substr(x[i], nchar(open) + 1, nchar(x[i]))
  i <- endsWith(x, close)
  n <- nchar(x)
  x[i] <- substr(x[i], 1, n[i] - nchar(close))
  x
}

matched_pattern <- function(x, pattern, nomatch = "") {
  found <- regexpr(pattern, x)
  matched <- regmatches(x, found)
  out <- rep(nomatch, length(x))
  out[found > 0] <- matched
  out
}

format_ggtext <- function(chunks, element) {
  colours <- chunks$colour
  strings <- chunks$string
  main_colour <- element$colour
  i <- chunks$depth > 0 & chunks$matches > 0 & colours != main_colour
  strings[i] <- paste0("<span style='color:", colours[i], "'>", strings[i], "</span>")
  text <- vapply(split(strings, chunks$line), paste0, "", collapse = "")
  paste0(text, collapse = "<br>")
}
