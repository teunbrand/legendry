
test_that("guide_labels works as axis", {

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
      x     = "labels",
      x.sec = guide_labels(n.dodge = 2),
      y     = guide_labels(angle = 45),
      y.sec = guide_labels(theme = theme(axis.text = element_text(colour = "red")))
    )

  vdiffr::expect_doppelganger("guide_labels cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, donut = 0.5) +
    guides(
      theta     = "labels",
      theta.sec = guide_labels(angle = 0),
      r         = guide_labels(n.dodge = 2),
      r.sec     = guide_labels(theme = theme(axis.text = element_text(colour = "red")))
    )

  vdiffr::expect_doppelganger("guide_labels radial", p)

})
