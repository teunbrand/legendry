
test_that("guide_grid_vanilla recapitulates coord_cartesian", {

  p <- ggplot(mapping = aes(x = 1:2, y = 1:2)) +
    geom_point()

  vdiffr::expect_doppelganger(
    "vanilla grid",
    p
  )

  vdiffr::expect_doppelganger(
    "vanilla grid",
    p + coord_guided(guide = guide_grid_vanilla())
  )

})
