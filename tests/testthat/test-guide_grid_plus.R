
test_that("guide_grid_plus looks correct", {

  p <- ggplot(mtcars, aes(disp, mpg)) +
    geom_point() +
    coord_guided() +
    theme(
      panel.grid.major.x = element_line(colour = 'tomato'),
      panel.grid.major.y = element_line(colour = "cornflowerblue"),
      panel.grid.minor.x = element_line(colour = "limegreen"),
      panel.grid.minor.y = element_line(colour = "orchid")
    )

  vdiffr::expect_doppelganger(
    "guide_grid_plus with long arms",
    p + guides(grid = guide_grid_plus(length = 3))
  )

  vdiffr::expect_doppelganger(
    "guide_grid_plus y lines no edge",
    p + guides(grid = guide_grid_plus(length_y_major = Inf, edge = FALSE))
  )
})

