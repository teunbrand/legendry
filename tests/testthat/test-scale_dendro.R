test_clust <- function(n) {
  m <- matrix(NA_real_, nrow = n, ncol = n)
  m[lower.tri(m)] <- seq_len(sum(lower.tri(m)))
  hclust(as.dist(m), method = "ave")
}

test_that("scale_xy_dendro throw appropriate error messages", {

  clust <- test_clust(3)

  expect_error(
    scale_x_dendro(NULL),
    "argument should be convertable"
  )
  expect_error(
    scale_y_dendro(clust, limits = 1:5),
    "it is derived from the labels"
  )
  expect_error(
    scale_x_dendro(clust, palette = 1:5),
    "requires fixed spacing"
  )

})

test_that("scale_xy_dendro looks correct", {

  xclust <- test_clust(3)
  yclust <- test_clust(4)

  base <- ggplot() +
    scale_x_dendro(xclust) +
    scale_y_dendro(yclust) +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.line = element_line()
    )

  p <- base +
    coord_cartesian(xlim = c(0, 4), ylim = c(1, 4)) +
    guides(
      x.sec = guide_axis_dendro(key = key_dendro(type = "triangle")),
      y.sec = guide_axis_dendro(space = rel(5), ticks = "ticks", axis_line = "line")
    )

  vdiffr::expect_doppelganger("scale_dendro cartesian", p)

  p <- base +
    expand_limits(x = c(0, 4), y = c(1, 4)) +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    guides(
      r.sec = guide_axis_dendro(key = key_dendro(type = "triangle"), ticks = "ticks", axis_line = "line"),
      theta.sec = guide_axis_dendro(ticks = "ticks", axis_line = "line")
    )

  vdiffr::expect_doppelganger("scale_dendro radial", p)

})
