test_that("guide_grid_swap() swaps gridlines", {

  p <- ggplot(mpg, aes(class)) +
    geom_bar()

  vdiffr::expect_doppelganger(
    "typical swap",
    p + coord_guided(guide_grid_swap())
  )

  vdiffr::expect_doppelganger(
    "no force minor",
    p + coord_guided(guide_grid_swap(force_minor = FALSE))
  )

})
