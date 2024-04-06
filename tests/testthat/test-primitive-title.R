
test_that("primitive_title works as axis", {

  base <- ggplot(mpg, aes(displ, hwy)) +
    geom_blank() +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.line = element_line()
    )

  red_text <- theme(axis.title.y.right = element_text(colour = "red", vjust = 0.5))

  p <- base +
    guides(
      x     = primitive_title(c("foo", "bar", "baz")),
      x.sec = primitive_title("horizontal title"),
      y     = primitive_title(c("qux", "egg"), angle = 45),
      y.sec = primitive_title("foobar", theme = red_text)
    )

  vdiffr::expect_doppelganger("primitive_title cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    guides(
      theta.sec = primitive_title(c("foo", "bar", "baz"), angle = 0),
      theta = primitive_title("horizontal title"),
      r     = primitive_title(c("qux", "egg"), angle = 90),
      r.sec = primitive_title("foobar", theme = red_text)
    )

  vdiffr::expect_doppelganger("primitive_title radial", p)
})

test_that("primitive_title works as legend", {

  p <- ggplot(mtcars) +
    aes(
      x = disp, y = mpg,
      colour = hp,
      fill = hp
    ) +
    geom_point() +
    guides(
      colour = primitive_title(title = "right title"),
      fill = primitive_title(title = "bottom title", position = "bottom")
    )

  vdiffr::expect_doppelganger("primitive_title legend", p)

})

test_that("draw_theta_title works", {

  elements <- list(
    title = theme_gray()$text,
    offset = 0
  )

  params <- list(
    position = "theta",
    donut = c(0.2, 0.4),
    arc = c(0, 1),
    bbox = list(x = c(0, 1), y = c(0, 1)),
    angle = 0
  )

  test <- withr::with_pdf(
    tempfile(fileext = ".pdf"),
    draw_theta_title("Foobar", elements, params)
  )

  expect_equal(test$children[[1]]$rot, 331, tolerance = 1)

  params$position <- "theta.sec"
  params$angle <- NULL

  test <- withr::with_pdf(
    tempfile(fileext = ".pdf"),
    draw_theta_title("Foobar", elements, params)
  )

  expect_equal(test$children[[1]]$rot, 0, tolerance = 1)
})
