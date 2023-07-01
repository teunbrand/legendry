test_that("guide_grid works as expected", {

  guide <- guide_grid(x_breaks = breaks_between(), x_minor_breaks = seq_along)

  p <- ggplot(mpg, aes(x = class, y = displ)) +
    geom_boxplot() +
    coord_guided(guide = guide) +
    theme_gray()

  vdiffr::expect_doppelganger(
    "guide_grid swapped major x", p
  )

  guide <- guide_grid(breaks = NULL, minor_breaks = NULL)

  p <- ggplot(mpg, aes(x = class, y = displ)) +
    geom_boxplot() +
    coord_guided(guide = guide) +
    theme_gray()

  vdiffr::expect_doppelganger(
    "guide_grid with no breaks", p
  )
})
