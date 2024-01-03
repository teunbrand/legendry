
test_that("guide_axis_custom looks good as axis", {

  base <- ggplot(msleep, aes(bodywt, awake)) +
    geom_blank() +
    scale_x_continuous(trans = "log10") +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.line = element_line(),
      axis.ticks.length = unit(5.5, "pt")
    )

  p <- base + guides(
    x = guide_axis_custom("log", angle = 0),
    x.sec = guide_axis_custom("minor"),
    y = guide_axis_custom(key = key_manual(c(5, 6, 7))),
    y.sec = guide_axis_custom(key = key_manual(c(5, 20, 15),
                                               label = c("A", "B", "C")))
  )

  vdiffr::expect_doppelganger("guide_axis_custom cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, donut = 0.5) + guides(
      theta = guide_axis_custom("log", angle = 0),
      theta.sec = guide_axis_custom("minor"),
      r = guide_axis_custom(key = key_manual(c(5, 7, 9)), angle = 0),
      r.sec = guide_axis_custom(key = key_manual(c(5, 20, 15),
                                                 label = c("A", "B", "C")))
    )

  vdiffr::expect_doppelganger("guide_axis_custom radial", p)
})
