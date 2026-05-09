
test_that("primitive_labels works as axis", {

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
      x     = primitive_labels(),
      x.sec = primitive_labels(n.dodge = 2L),
      y     = primitive_labels(angle = 45.0),
      y.sec = primitive_labels(
        theme = theme(axis.text = element_text(colour = "red"))
      )
    )

  vdiffr::expect_doppelganger("primitive_labels cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    guides(
      theta     = primitive_labels(),
      theta.sec = primitive_labels(angle = 0.0),
      r         = primitive_labels(n.dodge = 2L),
      r.sec     = primitive_labels(
        theme = theme(axis.text = element_text(colour = "red"))
      )
    )

  vdiffr::expect_doppelganger("primitive_labels radial", p)

})

test_that("primitive_labels works as legend", {

  p <- ggplot(mtcars) +
    aes(
      x = disp, y = mpg,
      colour = hp,
      fill = hp
    ) +
    geom_point() +
    guides(
      colour = primitive_labels(),
      fill = primitive_labels(
        key = key_manual(c(200.0, 300.0)), position = "bottom"
      )
    )

  vdiffr::expect_doppelganger("primitive_labels legend", p)

})

# Helper tests ------------------------------------------------------------

test_that("draw_labels sets priorities", {

  elem <- theme_gray()$text
  key <- data_frame0(
    x = c(0.0, 0.25, 0.5, 0.75, 1.0),
    y = 1.0,
    .label = c("A", "B", "C", "D", "E")
  )

  test <- withr::with_pdf(
    tempfile(fileext = ".pdf"),
    draw_labels(key, elem, 0.0, 0.0, "bottom", check_overlap = TRUE)
  )
  # Expect outer labels first
  expect_identical(test$children[[1L]]$label, c("A", "E", "C", "B", "D"))

})

test_that("draw_labels can draw theta labels", {

  elem <- theme_gray()$text

  key <- data_frame0(
    x = c(0.0, 0.25, 0.5, 0.75, 1.0),
    y = 1.0,
    .label = c("A", "B", "C", "D", "E")
  )
  key$theta <- key$x
  key$r <- 0.4

  test <- withr::with_pdf(
    tempfile(fileext = ".pdf"),
    draw_labels(key, elem, 0.0, 0.0, "theta")
  )
  expect_s3_class(test, "titleGrob")
  expect_equal(attr(test, "size"), 0.364, tolerance = 1e-3)

})

test_that("label angles works", {

  grid <- expand.grid(angle = seq(0.0, 360.0, by = 45.0), position = .trbl)
  result <- Map(
    angle_labels,
    angle = grid$angle, position = grid$position,
    MoreArgs = list(element = theme_gray()$text)
  )

  grid$hjust <- map_dbl(result, `[[`, i = "hjust")
  grid$vjust <- map_dbl(result, `[[`, i = "vjust")
  expect_snapshot(grid)
})
