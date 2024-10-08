
test_that("compose_stack works as axis line", {

  base <- ggplot(mpg, aes(displ, hwy)) +
    geom_blank() +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.line = element_line()
    )

  spacer <- primitive_spacer(space = unit(2.25, "pt"))
  stack <- compose_stack(
    "line", "ticks", "labels", spacer, "ticks", "line",
    side.titles = c("", "A", "", "", "B", ""),
    theme = theme(legendry.guide.spacing = unit(0, "pt"))
  )

  p <- base +
    guides(x = stack, x.sec = stack, y = stack, y.sec = stack)

  vdiffr::expect_doppelganger("compose_stack cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    guides(theta = stack, theta.sec = stack, r = stack, r.sec = stack)

  vdiffr::expect_doppelganger("compose_stack radial", p)

})

test_that("get_side_titles throws appropriate warning", {
  expect_error(
    get_side_titles("", list()),
    "Must have a number"
  )
})

test_that("theta_side_titles does not error", {

  elements <- list(
    side_titles = theme_gray()$text,
    side_position = "left"
  )

  test <- withr::with_pdf(
    tempfile(fileext = '.pdf'),
    theta_side_titles(
      label = c("A", "B"),
      elements,
      ranges = list(c(0, 0.4), c(0, 0.4)),
      params = list(
        position = "theta",
        sides = data.frame(
          theta = c(5.5, 0.8),
          r     = c(0.4, 0.4),
          position = c("left", "left"),
          group = 1:2,
          x = c(0.2, 0.6),
          y = c(0.9, 0.75),
          side = c("left", "left")
        )
      )
    )

  )

  expect_s3_class(test, "titleGrob")
})
