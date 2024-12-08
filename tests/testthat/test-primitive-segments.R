
test_that("primitive_segments works as axis", {

  base <- ggplot(mpg, aes(displ, hwy)) +
    geom_blank() +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.line = element_line()
    )

  hkey <- key_segment_manual(
    value     = c(2, 4, 6, 1.6, 1.6),
    value_end = c(2, 4, 6, 7.0, 7.0),
    oppo      = c(0, 0, 0, 1, 2),
    oppo_end  = c(3, 3, 3, 1, 2)
  )

  vkey <- key_segment_manual(
    value     = c(20, 30, 30, 40),
    value_end = c(30, 40, 20, 30),
    oppo      = 0, oppo_end  = 1
  )

  p <- base +
    guides(
      x     = primitive_segments(key = hkey),
      y     = primitive_segments(key = vkey),
      x.sec = primitive_segments(key = hkey),
      y.sec = primitive_segments(key = vkey)
    )

  vdiffr::expect_doppelganger("primitive_segments cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    guides(
      theta     = primitive_segments(key = hkey),
      r         = primitive_segments(key = vkey),
      theta.sec = primitive_segments(key = hkey),
      r.sec     = primitive_segments(key = vkey)
    )

  vdiffr::expect_doppelganger("primitive_segments radial", p)

})

test_that("primitive_segments works as a legend", {

  key <- key_segment_manual(
    value     = c(100, 200, 200, 300),
    value_end = c(200, 300, 100, 200),
    oppo = 0, oppo_end = 1
  )

  p <- ggplot(mtcars) +
    aes(x = disp, y = mpg, colour = hp, fill = hp) +
    geom_point() +
    guides(
      colour = primitive_segments(key = key),
      fill = primitive_segments(key = key, position = "bottom")
    )

  vdiffr::expect_doppelganger("primitive_segments legend", p)
})
