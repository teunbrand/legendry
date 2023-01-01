test_that("guide_axis_minor can be placed at every position", {

  p <- ggplot(mtcars, aes(mpg, disp)) +
    geom_blank() +
    guides(
      x = guide_axis_minor(minor_size = -0.5),
      y = guide_axis_minor(subtitle = "I have minor ticks"),
      y.sec = guide_axis_minor(cap_lower = min, cap_upper = max,
                               minor_size = 0.5),
      x.sec = guide_axis_minor(minor_size = 2)
    ) +
    theme_test() +
    theme(
      panel.border = element_blank(),
      axis.line = element_line(),
      axis.ticks.length.y.right = unit(-1, "cm")
    )

  vdiffr::expect_doppelganger(
    "all sides",
    p
  )
})
