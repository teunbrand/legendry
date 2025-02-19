test_that("label placement is ok regardless of hjust/vjust", {

  p <- ggplot(data.frame(x = c(4, 12, 25))) +
    geom_point(aes(x, x, size = x)) +
    scale_size_area(
      limits = c(0, 25),
      breaks = c(0, 3, 12, 25),
      max_size = 20,
      guide = guide_circles()
    )

  build  <- ggplot_build(p)
  guide  <- build$plot$guides$get_guide("size")
  params <- build$plot$guides$get_params("size")
  params[c("position", "direction")] <- list("right", "vertical")

  grid <- vec_expand_grid(hjust = c(0, 0.5, 1), vjust = c(0, 0.5, 1))

  grobs <- lapply(vec_seq_along(grid), function(i) {
    tmp <- params
    tmp[c("hjust", "vjust")] <- as.list(grid[i, ])
    guide$draw(
      theme_get() + theme(legend.text.position = "ontop"),
      params = tmp
    )
  })

  gt <- gtable(unit(rep(1, 3), "null"), unit(rep(1, 3), "null"))
  gt <- gtable_add_grob(
    gt, grobs,
    t = grid$vjust * 2 + 1,
    l = grid$hjust * 2 + 1
  )

  vdiffr::expect_doppelganger(
    "guide_circles text placement",
    gt
  )

  grid <- data.frame(
    text  = c("top", "right", "bottom", "left"),
    hjust = c(1, 0.5, 0, 0.5),
    vjust = c(0.5, 0, 0.5, 1)
  )

  grobs <- lapply(vec_seq_along(grid), function(i) {
    tmp <- params
    tmp[c("text_position", "hjust", "vjust")] <- as.list(grid[i, ])
    guide$draw(
      theme_get(),
      params = tmp
    )
  })

  gt <- gtable(unit(rep(1, 2), "null"), unit(rep(1, 2), "null"))
  gt <- gtable_add_grob(
    gt, grobs,
    t = c(1, 2, 1, 2),
    l = c(1, 1, 2, 2)
  )

  vdiffr::expect_doppelganger(
    "guide_circles text locations",
    gt
  )
})

test_that("guide_circles handles override.aes properly", {
  p <- guide_circles(override.aes = list(color = "blue", pch = 19))
  expect_equal(p$params$override.aes, list(colour = "blue", shape = 19))

  expect_snapshot_warning(
    p <- guide_circles(override.aes = list(colour = "black", color = "red"))
  )
  expect_equal(p$params$override.aes, list(colour = "black", colour = "red"))
})
