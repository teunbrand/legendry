
test_that("primitive_ticks works as axis", {

  base <- ggplot(mpg, aes(displ, hwy)) +
    geom_blank() +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.line = element_line()
    )

  p <- base +
    guides(
      x     = primitive_ticks(),
      x.sec = primitive_ticks(theme = theme(axis.ticks.length = unit(-2, "mm"))),
      y     = primitive_ticks(),
      y.sec = primitive_ticks(theme = theme(axis.ticks = element_line(colour = "red")))
    )

  vdiffr::expect_doppelganger("primitive_ticks cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, donut = 0.5) +
    guides(
      theta     = primitive_ticks(),
      theta.sec = primitive_ticks(theme = theme(axis.ticks.length = unit(-2, "mm"))),
      r         = primitive_ticks(),
      r.sec = primitive_ticks(theme = theme(axis.ticks = element_line(colour = "red")))
    )

  vdiffr::expect_doppelganger("primitive_ticks radial", p)

})

test_that("primitive_ticks works as legend", {

  p <- ggplot(mtcars) +
    aes(
      x = disp, y = mpg,
      colour = hp,
      fill = hp
    ) +
    geom_point() +
    guides(
      colour = primitive_ticks(key = key_manual(c(100, 300))),
      fill = primitive_ticks(position = "bottom")
    )

  vdiffr::expect_doppelganger("primitive_ticks legend", p)

})
