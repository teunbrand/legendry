# Group keys --------------------------------------------------------------

#' Group keys
#'
#' @description
#' These functions are helper functions for working with grouped data as keys in
#' guides. They all share the goal of creating a guide key, but have different
#' methods.
#'
#' * `key_group_split()` is a function factory whose functions make an attempt
#'   to infer groups from the scale's labels.
#' * `key_group_lut()` is a function factory whose functions use a look up table
#'   to sort out group membership.
#'
#' @param sep A `<character[1]>` giving a [regular expression][base::regex] to
#'   use for splitting labels provided by the scale using the
#'   [`strsplit()`][base::strsplit] function. By defaults, labels are splitted
#'   on any non-alphanumeric character.
#' @param reverse A `<logical[1]>` which if `FALSE` (default) treats the first
#'   part of the split string as groups and later parts as members. If `TRUE`,
#'   treats the last part as groups.
#' @param members A vector including the scale's `breaks` values.
#' @param group A vector parallel to `members` giving  the group of each member.
#' @param ungrouped A `<character[1]>` giving a group label to assign to the
#'   scale's `breaks` that match no values in the `members` argument.
#'
#' @name key_group
#' @family keys
#' @return
#' A function to use as the `key` argument in a guide.
#'
#' @examples
#' # Example scale
#' values <- c("group A:value 1", "group A:value 2", "group B:value 1")
#' template <- scale_colour_discrete(limits = values)
#'
#' # Treat the 'group X' part as groups
#' key <- key_group_split(sep = ":")
#' key(template)
#'
#' # Treat the 'value X' part as groups
#' key <- key_group_split(sep = ":", reverse = TRUE)
#' key(template)
#'
#' # Example scale
#' template <- scale_colour_discrete(limits = msleep$name[c(1, 7, 9, 23, 24)])
#'
#' # A lookup table can have more entries than needed
#' key <- key_group_lut(msleep$name, msleep$order)
#' key(template)
#'
#' # Or less entries than needed
#' key <- key_group_lut(
#'   msleep$name[23:24], msleep$order[23:24],
#'   ungrouped = "Other animals"
#' )
#' key(template)
NULL

#' @rdname key_group
#' @export
key_group_split <- function(sep = "[^[:alnum:]]+", reverse = FALSE) {
  check_string(sep)
  check_bool(reverse)
  call <- current_call()
  function(scale, aesthetic = NULL) {
    group_from_split_label(
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

group_from_split_label <- function(scale, aesthetic, sep = "[^[:alnum:]]+",
                                   reverse = FALSE, call = caller_env()) {

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

  # Split labels
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
  key$.group <- factor(groups, unique0(groups))
  vec_slice(key, order(key$.group))

}

group_from_lut <- function(scale, aesthetic, lut, ungrouped = "Other") {

  # Extract a standard key from the scale
  aesthetic <- aesthetic %||% scale$aesthetics[1]
  key <- Guide$extract_key(scale, aesthetic)

  group <- lut$key[match_list(key$.value, lut$val)] %|NA|% ungrouped
  if (!is.factor(group)) {
    group <- factor(group, c(setdiff(unique(group), ungrouped), ungrouped))
  }
  key$.group <- group
  vec_slice(key, order(group))
}
