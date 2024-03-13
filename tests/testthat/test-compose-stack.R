
test_that("compose_stack works as axis line", {

  base <- ggplot(mpg, aes(displ, hwy)) +
    geom_blank() +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.line = element_line()
    )

  spacer <- primitive_spacer(space = unit(2.25, "pt"))
  stack <- compose_stack(
    "line", "ticks", "labels", spacer, "ticks", "line",
    side.titles = c("", "A", "", "", "B", ""),
    theme = theme(gguidance.guide.spacing = unit(0, "pt"))
  )

  p <- base +
    guides(x = stack, x.sec = stack, y = stack, y.sec = stack)

  vdiffr::expect_doppelganger("compose_stack cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    guides(theta = stack, theta.sec = stack, r = stack, r.sec = stack)

  vdiffr::expect_doppelganger("compose_stack radial", p)

})
