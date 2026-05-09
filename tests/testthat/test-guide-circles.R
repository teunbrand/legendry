test_that("label placement is ok regardless of hjust or vjust", {

  p <- ggplot(data.frame(x = c(4.0, 12.0, 25.0))) +
    geom_point(aes(x, x, size = x)) +
    scale_size_area(
      limits = c(0.0, 25.0),
      breaks = c(0.0, 3.0, 12.0, 25.0),
      max_size = 20L,
      guide = guide_circles()
    )

  build  <- ggplot_build(p)
  guide  <- build$plot$guides$get_guide("size")
  params <- build$plot$guides$get_params("size")
  params[c("position", "direction")] <- list("right", "vertical")

  grid <- vec_expand_grid(hjust = c(0.0, 0.5, 1.0), vjust = c(0.0, 0.5, 1.0))

  grobs <- lapply(vec_seq_along(grid), function(i) {
    tmp <- params
    tmp[c("hjust", "vjust")] <- as.list(grid[i, ])
    guide$draw(
      theme_get() + theme(legend.text.position = "ontop"),
      params = tmp
    )
  })

  gt <- gtable(unit(rep(1.0, 3L), "null"), unit(rep(1.0, 3L), "null"))
  gt <- gtable_add_grob(
    gt, grobs,
    t = grid$vjust * 2.0 + 1.0,
    l = grid$hjust * 2.0 + 1.0
  )

  vdiffr::expect_doppelganger(
    "guide_circles text placement",
    gt
  )

  grid <- data_frame0(
    text  = c("top", "right", "bottom", "left"),
    hjust = c(1.0, 0.5, 0.0, 0.5),
    vjust = c(0.5, 0.0, 0.5, 1.0)
  )

  grobs <- lapply(vec_seq_along(grid), function(i) {
    tmp <- params
    tmp[c("text_position", "hjust", "vjust")] <- as.list(grid[i, ])
    guide$draw(
      theme_get(),
      params = tmp
    )
  })

  gt <- gtable(unit(rep(1.0, 2.0), "null"), unit(rep(1.0, 2.0), "null"))
  gt <- gtable_add_grob(
    gt, grobs,
    t = c(1L, 2L, 1L, 2L),
    l = c(1L, 1L, 2L, 2L)
  )

  vdiffr::expect_doppelganger(
    "guide_circles text locations",
    gt
  )
})

test_that("guide_circles handles override.aes properly", {
  p <- guide_circles(override.aes = list(color = "blue", pch = 19L))
  expect_identical(p$params$override.aes, list(colour = "blue", shape = 19L))

  expect_snapshot_warning(
    p <- guide_circles(override.aes = list(colour = "black", color = "red"))
  )
  expect_identical(p$params$override.aes, list(colour = "black", colour = "red"))
})
