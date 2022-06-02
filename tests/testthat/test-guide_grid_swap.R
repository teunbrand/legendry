test_that("guide_grid_swap() swaps gridlines", {

  p <- ggplot(mpg, aes(class)) +
    geom_bar() +
    theme(
      panel.grid.major = element_line(colour = "red"),
      panel.grid.minor = element_line(colour = "blue")
    )

  vdiffr::expect_doppelganger(
    "typical swap",
    p + coord_guided(guide_grid_swap())
  )

  vdiffr::expect_doppelganger(
    "no force minor",
    p + coord_guided(guide_grid_swap(force_minor = FALSE))
  )

})
