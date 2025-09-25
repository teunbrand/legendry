
test_that("guide_axis_base contains all primitive parameters", {
  fmls <- fn_fmls_names(guide_axis_base)
  expect_in(fn_fmls_names(primitive_line), fmls)
  expect_in(fn_fmls_names(primitive_labels), fmls)
  expect_in(fn_fmls_names(primitive_ticks), fmls)
})

test_that("guide_axis_base looks good as axis", {

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
    x = guide_axis_base("log", angle = 0),
    x.sec = guide_axis_base("minor", subtitle = "subtitle"),
    y = guide_axis_base(key = key_manual(c(5, 6, 7))),
    y.sec = guide_axis_base(key = key_manual(c(5, 20, 15),
                                             label = c("A", "B", "C")))
  )

  vdiffr::expect_doppelganger("guide_axis_base cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) + guides(
      theta = guide_axis_base("log", angle = 0),
      theta.sec = guide_axis_base("minor"),
      r = guide_axis_base(key = key_manual(c(5, 7, 9)), angle = 0),
      r.sec = guide_axis_base(key = key_manual(c(5, 20, 15),
                                                 label = c("A", "B", "C")))
    )

  vdiffr::expect_doppelganger("guide_axis_base radial", p)
})
