test_that("guide_axis_ext can be placed at every position", {

  p <- ggplot(mtcars, aes(mpg, disp)) +
    geom_blank() +
    guides(
      x = guide_axis_extend(subtitle = "miles per gallon"),
      y = guide_axis_extend(subtitle = "engine displacement", colour = "red",
                            subtitle.theme = element_text(colour = "blue")),
      x.sec = guide_axis_extend(minor_size = 0.8, subtitle = "Don't show me",
                                subtitle.theme = element_blank()),
      y.sec = guide_axis_extend(cap_lower = min, cap_upper = max,
                                title = "truncated axis")
    ) +
    theme_test() +
    theme(
      panel.border = element_blank(),
      axis.line = element_line()
    )

  vdiffr::expect_doppelganger(
    "all sides",
    p
  )
})


