
test_that("guide_ticks works as axis", {

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
      x     = "ticks",
      x.sec = guide_ticks(theme = theme(axis.ticks.length = unit(-2, "mm"))),
      y     = "ticks",
      y.sec = guide_ticks(theme = theme(axis.ticks = element_line(colour = "red")))
    )

  vdiffr::expect_doppelganger("guide_ticks cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, donut = 0.5) +
    guides(
      theta     = "ticks",
      theta.sec = guide_ticks(theme = theme(axis.ticks.length = unit(-2, "mm"))),
      r         = "ticks",
      r.sec = guide_ticks(theme = theme(axis.ticks = element_line(colour = "red")))
    )

  vdiffr::expect_doppelganger("guide_ticks radial", p)

})
