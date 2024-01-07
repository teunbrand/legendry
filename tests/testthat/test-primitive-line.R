
test_that("primitive_line works as axis line", {

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
      x     = primitive_line(),
      x.sec = primitive_line(cap = "both"),
      y     = primitive_line(cap = c(15, 25, 35, 40)),
      y.sec = primitive_line(cap = function(breaks, limits) {
        c(min(breaks, na.rm = TRUE), limits[2])
      })
    )

  vdiffr::expect_doppelganger("primitive_line cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, donut = 0.5) +
    guides(
      theta.sec = primitive_line(),
      theta     = primitive_line(cap = "both"),
      r         = primitive_line(cap = c(15, 25, 35, 40)),
      r.sec     = primitive_line(cap = function(breaks, limits) {
        c(min(breaks, na.rm = TRUE), limits[2])
      })
    )

  vdiffr::expect_doppelganger("primitive_line radial", p)

})

test_that("primitive_line works as legend", {

  p <- ggplot(mtcars) +
    aes(
      x = disp, y = mpg,
      colour = hp,
      fill = hp
    ) +
    geom_point() +
    guides(
      colour = primitive_line(),
      fill = primitive_line(position = "bottom")
    )


  vdiffr::expect_doppelganger("primitive_line legend", p)

})
