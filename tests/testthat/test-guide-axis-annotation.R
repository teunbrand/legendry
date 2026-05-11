
test_that("annotate_* family looks allright", {

  p <- ggplot(mpg, aes(drv, displ)) +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank()
    )

  p <- p +
    annotate_bottom(1.5, "Bottom", face = "bold") +
    annotate_top(
      "f", "forward",
      line_colour = "blue"
    ) +
    annotate_right(
      4, colour = "red"
    ) +
    annotate_left(
      c(3, 4, 5), c("three", "four", "five"),
      text_colour = c("forestgreen", "purple", "orange")
    )

  vdiffr::expect_doppelganger("annotate family", p)
})

test_that("gudie_axis_annotation fits in unusual places", {

  p <- ggplot(mtcars, aes(disp, mpg, colour = wt)) +
    geom_point(shape = NA, na.rm = TRUE) +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank()
    )

  p <- p + guides(
    theta = guide_axis_annotation(250, "Outside"),
    theta.sec = guide_axis_annotation(c(200, 400), c("Inside A", "Inside B")),
    r = guide_axis_annotation(15, face = "bold"),
    r.sec = guide_axis_annotation(25, text_colour = "red"),
    colour = guide_colbar(
      second_guide = guide_axis_annotation(3.5, theme = theme_gray(ink = "limegreen"))
    )
  )

  # Suppressing vdiffr's lack of support for gradients from the colbar
  suppressWarnings({
    vdiffr::expect_doppelganger("guide_axis_annotation", p)
  })
})
