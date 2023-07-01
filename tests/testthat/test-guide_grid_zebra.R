
test_that("guide_grid_zebra looks correct", {

  p <- ggplot(mtcars, aes(disp, mpg)) +
    geom_point() +
    coord_guided() +
    scale_x_continuous(breaks = scales::breaks_width(50)) +
    theme(
      panel.grid.major.x = element_line(colour = 'tomato'),
      panel.grid.major.y = element_line(colour = "cornflowerblue"),
      panel.grid.minor.x = element_line(colour = "limegreen"),
      panel.grid.minor.y = element_line(colour = "orchid")
    )

  vdiffr::expect_doppelganger(
    "guide_grid_zebra x stripes",
    p + guides(grid = guide_grid_zebra("x"))
  )

  vdiffr::expect_doppelganger(
    "guide_grid_zebra y stripes",
    p + guides(grid = guide_grid_zebra("y"))
  )

  vdiffr::expect_doppelganger(
    "guide_grid_zebra gingham pattern",
    p + guides(grid = guide_grid_zebra(
      "both", at = "both",
      rect_x = element_rect(fill = alpha("tomato", 0.5)),
      rect_y = element_rect(fill = alpha("cornflowerblue", 0.5))
    ))
  )

  p <- ggplot(mpg, aes(class, displ)) +
    geom_boxplot() +
    theme(
      panel.grid.major.x = element_line(colour = 'tomato'),
      panel.grid.major.y = element_line(colour = "cornflowerblue"),
      panel.grid.minor.x = element_line(colour = "limegreen"),
      panel.grid.minor.y = element_line(colour = "orchid")
    ) +
    coord_guided(guide_grid_zebra())

  vdiffr::expect_doppelganger(
    "automatic discrete stripes",
    p
  )
})

