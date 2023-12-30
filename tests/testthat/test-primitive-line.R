
test_that("guide_line works as axis line", {

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
      x     = "line",
      x.sec = guide_line(cap = "both"),
      y     = guide_line(cap = c(15, 25, 35, 40)),
      y.sec = guide_line(cap = function(breaks, limits) {
        c(min(breaks, na.rm = TRUE), limits[2])
      })
    )

  vdiffr::expect_doppelganger("guide_line cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, donut = 0.5) +
    guides(
      theta.sec = "line",
      theta     = guide_line(cap = "both"),
      r         = guide_line(cap = c(15, 25, 35, 40)),
      r.sec     = guide_line(cap = function(breaks, limits) {
        c(min(breaks, na.rm = TRUE), limits[2])
      })
    )

  vdiffr::expect_doppelganger("guide_line radial", p)

})

