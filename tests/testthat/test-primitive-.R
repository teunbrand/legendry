test_that("new_params include default params", {
  # This test acts as a sentinel when ggplot2 changes default parameters
  expect_equal(new_params(), Guide$params)
})

test_that("primitive grob makes grobs for theta positions", {

  grob <- pointsGrob()

  test <- primitive_grob(grob, unit(2, "cm"), "theta", "grob")
  expect_s3_class(test, "gTree")

  grob <- list(grob, grob)
  test <- primitive_grob(grob, unit(2, "cm"), "theta.sec", "grob")
  expect_s3_class(test, "gTree")

})

test_that("primitive_setup_elements sets up elements", {

  elements <- list(
    position = list(title = "axis.title", text = "axis.text", ticks_length = "axis.ticks.length"),
    legend   = list(title = "legend.title", text = "legend.text", ticks_length = "legend.ticks.length")
  )

  theme <- theme_gray()

  params <- list(position = "right", aesthetic = "y", direction = "vertical")

  test <- primitive_setup_elements(params, elements, theme)
  if (inherits(element_text, "S7_class")) {
    expect_s7_class(test$title, element_text)
    expect_s7_class(test$text, element_text)
  } else {
    expect_s3_class(test$title, "element_text")
    expect_s3_class(test$text, "element_text")
  }
  expect_s3_class(test$ticks_length, "unit")

  params$aesthetic <- "colour"

  test <- primitive_setup_elements(params, elements, theme)
  if (inherits(element_text, "S7_class")) {
    expect_s7_class(test$title, element_text)
    expect_s7_class(test$text, element_text)
  } else {
    expect_s3_class(test$title, "element_text")
    expect_s3_class(test$text, "element_text")
  }
  expect_s3_class(test$ticks_length, "unit")
})

test_that("primitive_setup_params sets up non-position parameters", {

  params <- list(
    position = "right", direction = "vertical", aesthetic = "colour",
    limits = c(0, 10),
    key = data.frame(.value = c(2, 8)),
    decor = data.frame(y = 0.5)
  )

  test <- primitive_setup_params(params)
  expect_equal(test$key$x, c(0, 0))
  expect_equal(test$key$y, c(0.2, 0.8))
  expect_in(names(test$decor), c("x", "y"))

  params <- list(aesthetic = "x")
  test <- primitive_setup_params(params)
  expect_equal(params, test)
})
