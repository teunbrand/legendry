
key_segments_manual <- function(value, oppo, value_end = value,
                                oppo_end = oppo, ...) {
  df <- data_frame0(
    value = value, oppo = oppo,
    value_end = value_end, oppo_end = oppo_end,
    !!!label_args(...)
  )
  check_columns(df, c("value", "oppo"))
  class(df) <- c("key_segment", "key_guide", class(df))
  df
}

key_segment_map <- function(data, ..., .call = caller_env()) {

  mapping <- enquos(...)
  mapping <- Filter(Negate(quo_is_missing), mapping)
  mapping <- new_aes(mapping, env = .call)

  params <- c("colour", "linewidth", "linetype")

  df <- eval_aes(
    data, mapping,
    required = c("value", "oppo"),
    optional = c("value_end", "oppo_end", params),
    call = .call, arg_mapping = "mapping", arg_data = "data"
  )

  df <- rename(df, params, paste0(".", params))
  df$colour <- df$color
  df$color <- NULL
  class(df) <- c("key_segment", "key_guide", class(df))
  df

}

key_dendro <- function(dendro = NULL, type = "rectangle") {
  force(dendro)
  function(scale, aesthetic = NULL, ...) {
    extract_dendro(scale$hclust %||% dendro, type = type)
  }
}

# Dendrogram utilities ----------------------------------------------------

# Simplified version of `stats:::plotNode`.
# It only looks for the segments and ignores labels and most other attributes.
extract_dendro <- function(tree, type = "rectangle") {

  # Check arguments
  whole_tree <- tree <- try_fetch(
    as.dendrogram(tree),
    error = function(cnd) {
      cli::cli_abort("Could not find or coerce {.arg dendro} argument.", parent = cnd)
    }
  )
  type <- arg_match0(type, c("rectangle", "triangle"))

  # Initialise stuff
  depth <- 0
  llimit <- list()
  x1 <- i <- 1
  x2 <- number_of_members(tree)
  KK <- kk <- integer()

  n_obs <- stats::nobs(tree)
  n_segments <- switch(type, triangle = 2 * n_obs - 2, 4 * n_obs - 4)

  mtx <- matrix(NA_real_, n_segments, ncol = 4)
  colnames(mtx) <- c("value", "oppo", "value_end", "oppo_end")

  repeat {
    depth <- depth + 1
    inner <- !stats::is.leaf(tree) && x1 != x2

    node <- node_limit(x1, x2, tree)
    llimit[[depth]] <- node$limit

    ymax <- attr(tree, 'height')
    xmax <- node$x

    if (inner) {
      for (k in seq_along(tree)) {
        child <- tree[[k]]

        ymin <- attr(child, "height") %||% 0
        xmin <- node$limit[k] + (attr(child, "midpoint") %||% 0)

        # Update segments
        if (type == "triangle") {
          mtx[i, ] <- c(xmax, ymax, xmin, ymin)
          i <- i + 1
        } else {
          mtx[i + 0:1, ] <- c(xmax, xmin, ymax, ymax, xmin, xmin, ymax, ymin)
          i <- i + 2
        }
      }
      if (length(tree) > 0) {
        KK[depth] <- length(tree)
        kk[depth] <- 1L
        x1 <- node$limit[1L]
        x2 <- node$limit[2L]
        tree <- tree[[1]]
      }
    } else {
      repeat {
        depth <- depth - 1L
        if (!depth || kk[depth] < KK[depth]) {
          break
        }
      }
      if (!depth) {
        break
      }
      length(kk) <- depth
      kk[depth] <- k <- kk[depth] + 1L
      x1 <- llimit[[depth]][k]
      x2 <- llimit[[depth]][k + 1L]
      tree <- whole_tree[[kk]]
    }
  }
  as.data.frame(mtx)
}

# Copy of `stats:::.memberDend()`
number_of_members <- function(tree) {
  attr(tree, "x.member") %||% attr(tree, "members") %||% 1L
}

# Simplified version of `stats:::plotNodeLimit`,
# It has `center = FALSE` build-in.
node_limit <- function(x1, x2, subtree) {
  inner <- !stats::is.leaf(subtree) && x1 != x2
  if (inner) {
    K <- length(subtree)
    limit <- integer(K)
    xx1 <- x1
    for (k in 1L:K) {
      xx1 <- xx1 + number_of_members(subtree[[k]])
      limit[k] <- xx1
    }
  } else {
    limit <- x2
  }
  limit <- c(x1, limit)
  mid <- attr(subtree, "midpoint")
  center <- inner && !is.numeric(mid)
  x <- if (center) mean(c(x1, x2)) else x1 + (mid %||% 0)
  list(x = x, limit = limit)
}

