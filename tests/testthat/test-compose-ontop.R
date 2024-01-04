test_that("compose_ontop works as axis line", {

  base <- ggplot(mpg, aes(displ, hwy)) +
    geom_blank() +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.ticks = element_line(colour = "dodgerblue"),
      axis.ticks.length = unit(0.5, "cm"),
      axis.line = element_line(colour = "dodgerblue")
    )

  top <- compose_stack(
    primitive_spacer(unit(0.25, "cm")),
    guide_axis_custom(
      key_manual(c(2.5, 3.5, 4.5, 5.5, 6.5, 15, 25, 35)),
      theme = theme(
        axis.ticks = element_line(colour = "tomato"),
        axis.ticks.length = unit(0.5, "cm"),
        axis.line = element_line(colour = "tomato")
      )
    ),
    theme = theme(gguidance.guide.spacing = unit(0, "cm"))
  )

  ontop <- compose_ontop(
    guide_axis_custom(), top
  )

  p <- base +
    guides(
      x = ontop, x.sec = ontop,
      y = ontop, y.sec = ontop
    )

  vdiffr::expect_doppelganger("compose_ontop cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, donut = 0.5) +
    guides(
      theta = ontop, theta.sec = ontop,
      r = ontop, r.sec = ontop
    )

  vdiffr::expect_doppelganger("compose_ontop radial", p)

})
