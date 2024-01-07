
test_that("primitive_spacer works in guide_axis_stack()", {

  base <- ggplot(mpg, aes(displ, hwy)) +
    geom_blank() +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.line = element_line()
    )

  spacer <- primitive_spacer(unit(0.5, "cm"))
  stack  <- guide_axis_stack("axis", spacer, "axis")

  p <- base + guides(
    x = stack, x.sec = stack,
    y = stack, y.sec = stack
  )

  vdiffr::expect_doppelganger("primitive_spacer cartesian", p)

  theta <- guide_axis_stack("axis_theta", spacer, "axis_theta")

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, donut = 0.5) +
    guides(theta = theta, theta.sec = theta, r = stack, r.sec = stack)

  vdiffr::expect_doppelganger("primitive_spacer radial", p)
})

test_that("primitive_spacer works as legend", {

  p <- ggplot(mtcars) +
    aes(
      x = disp, y = mpg,
      colour = hp,
      fill = hp
    ) +
    geom_point() +
    guides(
      colour = primitive_spacer(space = unit(2, "cm")),
      fill = primitive_spacer(position = "bottom", space = unit(2, "cm"))
    )

  vdiffr::expect_doppelganger("primitive_spacer legend", p)

})
