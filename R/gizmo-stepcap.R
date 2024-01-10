# Constructor -------------------------------------------------------------

#' Guide gizmo: capped colour steps
#'
#' This guide displays a binned variant of the colour bar with optional caps at
#' either ends of the bar.
#'
#' @param key A [bins key][key_bins] specificiation. Defaults to
#'   [`key_bins(even.steps = FALSE, show.limits = NULL)`]. Changing the
#'   arguments to `key_bins()` is fine, but changing the key type is not
#'   advised.
#' @inheritParams gizmo_barcap
#'
#' @return A `GizmoStepcap` object.
#' @family gizmos
#' @export
#'
#' @examples
#' # A standard plot
#' p <- ggplot(mpg, aes(displ, hwy, colour = cty)) +
#'   geom_point()
#'
#' # Just some recangles
#' p + scale_colour_viridis_c(guide = gizmo_stepcap())
#'
#' # Caps show up when there is data outside the limits
#' p + scale_colour_viridis_c(
#'   limits = c(10, 30),
#'   guide = gizmo_stepcap()
#' )
#'
#' # The scale's out-of-bounds handler determines cap colour
#' p + scale_colour_viridis_c(
#'   limits = c(10, 30), oob = scales::oob_squish,
#'   guide = gizmo_stepcap()
#' )
#'
#' # Customising the display of the guide
#' p +
#'   scale_colour_viridis_c(
#'     oob = scales::oob_squish,
#'     guide = gizmo_stepcap(
#'       shape = "round", show = c(FALSE, TRUE),
#'       size = unit(1, "cm"),
#'       theme = theme(legend.key.height = unit(4, "cm"))
#'     )
#'   ) +
#'   theme(
#'     legend.frame = element_rect(colour = "black"),
#'     legend.key.width = unit(0.5, "cm")
#'   )
gizmo_stepcap <- function(key = "bins", shape = "triangle", size = NULL, show = NA,
                          alpha = NA, oob = "keep", theme = NULL,
                          position = waiver(), direction = NULL) {
  check_number_decimal(
    alpha, min = 0, max = 1,
    allow_infinite = FALSE, allow_na = TRUE
  )
  check_logical(show)
  check_length(show, exact = 1:2)
  show <- rep(show, length.out = 2)

  check_unit(size, allow_null = TRUE)
  shape <- resolve_cap_shape(shape)
  oob <- resolve_oob(oob)

  new_guide(
    key   = key,
    shape = shape,
    size  = size,
    show  = show,
    alpha = alpha,
    oob = oob,
    theme = theme,
    position = position,
    direction = direction,
    available_aes = c("colour", "fill"),
    super = GizmoStepcap
  )
}

# Class -------------------------------------------------------------------

#' @export
#' @rdname gguidance_extensions
#' @format NULL
#' @usage NULL
GizmoStepcap <- ggproto(
  "GizmoStepcap", GizmoBarcap,

  elements = list(
    frame  = "legend.frame",
    width  = "legend.key.width",
    height = "legend.key.height"
  ),

  params = new_params(alpha = NA, shape = NULL, show = NA, key = "steps",
                      size = NULL, oob = oob_keep),

  extract_params = function(scale, params, ...) {

    params$position <- params$position %|W|% NULL
    limits <- scale$get_limits()
    range  <- scale$range$range
    aes <- params$aesthetic
    key <- params$key

    lower_oob <- range[1] < limits[1]
    upper_oob <- range[2] > limits[2]

    params$show[1] <- !isFALSE(params$show[1] %|NA|% lower_oob)
    params$show[2] <- !isFALSE(params$show[2] %|NA|% upper_oob)
    add <- diff(limits) / 1000

    if (params$show[1]) {
      val <- params$oob(limits[1] - add, limits)
      limits <- range(limits, val)
      key <- data_frame0(
        !!aes := c(scale$map(val), key[[aes]]),
        min    = c(-Inf, key$min),
        max    = c(key$min[1], key$max),
        .label = c(NA, key$.label),
        .value = c(NA, key$.value)
      )
    }
    if (params$show[2]) {
      val <- params$oob(limits[2] + add, limits)
      limits <- range(limits, val)
      n <- max(which(!is.na(key[[aes]])))
      if (n == nrow(key)) {
        key <- data_frame0(
          !!aes := c(key[[aes]], scale$map(val)),
          min    = c(key$min, key$max[nrow(key)]),
          max    = c(key$max, Inf),
          .label = c(key$.label, NA),
          .value = c(key$.value, NA)
        )
      } else {
        n <- n + 1
        key[[aes]][n] <- scale$map(val)
        key$min[n] <- key$max[n - 1]
        key$max[n] <- Inf
      }
    }
    if (grepl("color|colour|fill", params$aesthetic)) {
      key[[aes]] <- alpha(key[[aes]], alpha = params$alpha)
    }
    params$limits <- limits
    params$key <- key
    params
  },

  setup_params = function(params) {
    key <- params$key
    key <- vec_slice(key, !is.na(key[[params$aesthetic]]))
    min <- guide_rescale(key$min, params$limits)
    max <- guide_rescale(key$max, params$limits)
    key$mid <- (max + min) / 2
    key$height <- abs(max - min)
    params$key <- key
    params
  },

  fill_frame = function(key, grobs = NULL, elements, params) {
    if (!any(c("colour", "fill") %in% names(key))) {
      return(grobs)
    }
    check_device("clippingPaths")

    lower <- grobs$lower
    upper <- grobs$upper

    min <- unit(0, "npc") + lower
    max <- unit(1, "npc") + upper
    delta <- unit(1, "npc") - (lower + upper)
    n <- nrow(key)

    args <- list(
      x = 0.5, width = 1,
      y = unit.c(
        unit(0, "npc"), min + key$mid[-c(1, n)] * delta, unit(1, "npc")
      ),
      height = unit.c(
        lower + key$height[1] * delta,
        key$height[-c(1, n)]  * delta,
        upper + key$height[n] * delta
      ),
      vjust = c(0, rep(0.5, nrow(key) - 2L), 1),
      gp = gpar(fill = key[[params$aesthetic]], col = NA)
    )

    if (params$direction == "horizontal") {
      args <- flip_names(args)
    }
    args$vp <- viewport(clip = grobs$grob)
    grobs$grob <- editGrob(grobs$grob, gp = gpar(fill = NA))
    grobs$grob <- gTree(children = gList(inject(rectGrob(!!!args)), grobs$grob))
    grobs
  }
)

