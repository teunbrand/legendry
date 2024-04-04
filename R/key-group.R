# Group keys --------------------------------------------------------------

#' Group keys
#'
#' @description
#' These functions are helper function for working with grouped data as keys in
#' guides. They all share the goal of creating a guide key, but have different
#' methods.
#'
#' * `key_group_auto()` is a function factory whose functions make an attempt
#'   to infer groups from the scale's labels.
#' * `key_group_lut()` is a function factory whose functions use a look up table
#'   to sort out group membership.
#'
#' @param sep A `<character[1]>` giving a [regular expression][base::regex] to
#'   use for splitting labels provided by the scale using
#'   [`strsplit()`][base::strsplit]. Defaults to splitting on any
#'   non-alphanumeric character.
#' @param reverse A `<logical[1]>` which if `FALSE` (default) treats the first
#'   part of the split string as groups, and if `TRUE` treats the last part
#'   as groups.
#' @param members A vector including the scale's `breaks` values.
#' @param group A vector parallel to `members` giving the group of every member.
#' @param ungrouped A `<character[1]>` giving a group label to assign to the
#'   scale's `breaks` that match no values in the `members` argument.
#'
#' @details
#' The resulting key is always sorted by group.
#' The `key_group_auto()` does *not* work with expression labels.
#'
#' @name key_group
#' @family keys
#' @return
#' For `key_group_auto()` and `key_group_lut()`, a function.
#'
#' @examples
#' # Example scale
#' values <- c("group A:value 1", "group A:value 2", "group B:value 1")
#' template <- scale_colour_discrete(limits = values)
#'
#' # Treat the 'group X' part as groups
#' auto <- key_group_auto(sep = ":")
#' auto(template)
#'
#' # Treat the 'value X' part as groups
#' auto <- key_group_auto(sep = ":", reverse = TRUE)
#' auto(template)
#'
#' # Example scale
#' template <- scale_colour_discrete(limits = msleep$name[c(1, 7, 9, 23, 24)])
#'
#' # A lookup table can have more entries than needed
#' lut <- key_group_lut(msleep$name, msleep$order)
#' lut(template)
#'
#' # Or less entries than needed
#' lut <- key_group_lut(
#'   msleep$name[23:24], msleep$order[23:24],
#'   ungrouped = "Other animals"
#' )
#' lut(template)
NULL

#' @rdname key_group
#' @export
key_group_auto <- function(sep = "[^[:alnum:]]+", reverse = FALSE) {
  check_string(sep)
  check_bool(reverse)
  force(sep)
  force(reverse)
  call <- current_call()
  function(scale, aesthetic = NULL) {
    group_from_label(
      scale = scale, aesthetic = aesthetic,
      sep = sep, reverse = reverse,
      call = call
    )
  }
}

#' @rdname key_group
#' @export
key_group_lut <- function(members, group, ungrouped = "Other") {
  check_string(ungrouped)
  check_unique(members)
  if (length(group) != length(members)) {
    cli::cli_abort(c(
      "{.arg group} must have the same length as {.arg members}.",
      i = "{.arg group} has length {length(group)}.",
      i = "{.arg members} has length {length(members)}."
    ))
  }
  lut <- vec_split(members, group)

  function(scale, aesthetic = NULL) {
    group_from_lut(
      scale = scale, aesthetic = aesthetic,
      lut = lut, ungrouped = ungrouped
    )
  }
}

# Helpers -----------------------------------------------------------------

group_from_lut <- function(scale, aesthetic, lut, ungrouped = "Other") {
  aesthetic <- aesthetic %||% scale$aesthetics[1]
  key <- Guide$extract_key(scale, aesthetic)
  group <- lut$key[match_list(key$.value, lut$val)] %|NA|% ungrouped
  if (!is.factor(group)) {
    group <- factor(group, unique(group))
  }
  key$.group <- group
  vec_slice(key, order(group))
}

group_from_label <- function(scale, aesthetic, sep = "[^[:alnum:]]+",
                             reverse = FALSE, call = call) {
  # Extract a standard key from the scale
  aesthetic <- aesthetic %||% scale$aesthetics[1]
  key <- Guide$extract_key(scale, aesthetic)

  # Reject expressions, as we cannot split these
  if (!is.character(key$.label)) {
    type <- obj_type_friendly(key$.label)
    cli::cli_abort(
      c("Cannot split the guide's {.field label}.",
        i = "It must be a {.cls character} vector, not {type}."),
      call = call
    )
  }

  labels <- strsplit(key$.label, sep)
  if (isTRUE(reverse)) {
    i <- lengths(labels)
  } else {
    i <- rep(1L, length(labels))
  }

  groups <- vec_c(!!!Map(vec_slice, i = i, x = labels))
  labels <- lapply(Map(vec_slice, i = -i, x = labels), paste0, collapse = " ")
  labels <- vec_c(!!!labels)

  key$.label <- labels
  key$.group <- factor(groups, unique(groups))
  vec_slice(key, order(key$.group))
}
