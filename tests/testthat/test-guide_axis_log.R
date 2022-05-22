test_that("guide_axis_log can be placed at every position", {

  p <- ggplot(data.frame(x = c(1, 100)), aes(x, log10(x))) +
    geom_point() +
    scale_x_log10() +
    guides(
      x = guide_axis_log(),
      x.sec = guide_axis_log(minor_size = 1.5, mini_size = 0.75),
      y = guide_axis_log(base = 10, pre_scaled = TRUE),
      y.sec = guide_axis_log(base = 5, pre_scaled = TRUE)
    ) +
    theme_test() +
    theme(
      axis.ticks.length.x.bottom = unit(-0.5, "cm"),
      axis.ticks.length.y.right = unit(-0.5, "cm"),
      axis.ticks.length.y.left = unit(0.5, "cm"),
      axis.ticks.length.x.top = unit(0.5, "cm")
    )

  vdiffr::expect_doppelganger(
    "all sides",
    p
  )



})
