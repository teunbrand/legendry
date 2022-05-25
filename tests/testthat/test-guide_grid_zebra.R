test_that("guide_grid_zebra respects just", {

  p <- ggplot() +
    geom_blank(aes(x = 1:2, y = 1:2)) +
    scale_x_continuous(expand = c(0, 0))

  gt <- ggplotGrob(p + coord_guided(guide = guide_grid_zebra(just = 1)))
  gt <- gt$grobs[gt$layout$name == "panel"][[1]]

  grob <- grid::getGrob(gt, gPath("grill"), grep = TRUE)
  grob <- grob$children[[2]]

  expect_equal(as.numeric(grob$x), c(0, 0.375, 0.875))

  gt <- ggplotGrob(p + coord_guided(guide = guide_grid_zebra(just = 0)))
  gt <- gt$grobs[gt$layout$name == "panel"][[1]]

  grob <- grid::getGrob(gt, gPath("grill"), grep = TRUE)
  grob <- grob$children[[2]]

  expect_equal(as.numeric(grob$x), c(0.125, 0.625, 1))

  gt <- ggplotGrob(p + coord_guided(guide = guide_grid_zebra(just = 0.5)))
  gt <- gt$grobs[gt$layout$name == "panel"][[1]]

  grob <- grid::getGrob(gt, gPath("grill"), grep = TRUE)
  grob <- grob$children[[2]]

  expect_equal(as.numeric(grob$x), c(0.0625, 0.5, 0.9375))
})
