test_that("guide_axis_plot() can be rendered", {

  x_plot <- ggplot(mtcars, aes(disp, factor(cyl), colour = factor(cyl))) +
    geom_boxplot() +
    guides(y.sec = "axis")

  y_plot <- ggplot(mtcars, aes(y = mpg, fill = factor(cyl))) +
    geom_histogram(bins = 10, na.rm = TRUE) +
    guides(x.sec = "axis")

  p <- ggplot(mtcars, aes(disp, mpg, colour = factor(cyl))) +
    geom_point() +
    guides(
      x     = guide_axis_plot(x_plot),
      x.sec = guide_axis_plot(x_plot, reposition = FALSE),
      y     = guide_axis_plot(y_plot + labs(title = "Title")),
      y.sec = guide_axis_plot(y_plot + labs(caption = "Caption"))
    )

  vdiffr::expect_doppelganger("guide_axis_plot", p)
})

test_that("guide_axis_plot() rejects incompatible plots", {

  expect_snapshot(
    guide_axis_plot(plot = "foobar"),
    error = TRUE
  )

  plot <- ggplot(mtcars, aes(disp, mpg)) + geom_point()

  expect_snapshot(
    guide_axis_plot(plot + coord_radial()),
    error = TRUE
  )

  expect_snapshot(
    guide_axis_plot(plot + facet_wrap(~ cyl)),
    error = TRUE
  )

  wrong_plot <- plot +
    coord_transform(x = "sqrt") +
    scale_x_continuous(guide = guide_axis_plot(plot))

  expect_snapshot(
    ggplot_build(wrong_plot),
    error = TRUE
  )
})
