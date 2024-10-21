
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
