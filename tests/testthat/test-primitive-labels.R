
test_that("primitive_labels works as axis", {

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
      x     = primitive_labels(),
      x.sec = primitive_labels(n.dodge = 2),
      y     = primitive_labels(angle = 45),
      y.sec = primitive_labels(theme = theme(axis.text = element_text(colour = "red")))
    )

  vdiffr::expect_doppelganger("primitive_labels cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, donut = 0.5) +
    guides(
      theta     = primitive_labels(),
      theta.sec = primitive_labels(angle = 0),
      r         = primitive_labels(n.dodge = 2),
      r.sec     = primitive_labels(theme = theme(axis.text = element_text(colour = "red")))
    )

  vdiffr::expect_doppelganger("primitive_labels radial", p)

})

test_that("primitive_labels works as legend", {

  p <- ggplot(mtcars) +
    aes(
      x = disp, y = mpg,
      colour = hp,
      fill = hp
    ) +
    geom_point() +
    guides(
      colour = primitive_labels(),
      fill = primitive_labels(key = key_manual(c(200, 300)), position = "bottom")
    )

  vdiffr::expect_doppelganger("primitive_labels legend", p)

})
