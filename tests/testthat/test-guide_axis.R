
test_that("GuideAxis can be trained", {

  brks <- seq(0, 10, by = 2)
  sc <- scale_x_continuous(breaks = brks)
  sc$train(c(0, 10))
  sc <- ggplot2:::view_scale_primary(sc)

  gu <- guide_axis_vanilla()

  gu <- guide_train(gu, sc)

  expect_equal(gu$key$x,      brks)
  expect_equal(gu$key$.value, brks)
  expect_equal(gu$key$.label, as.character(brks))

})

test_that("GuideAxis warns when positions mismatch", {
  expect_error(guide_axis_vanilla(position = "bamboozled"))

  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    scale_y_continuous(guide = guide_axis_vanilla(position = "top"))
  built <- expect_silent(ggplot_build(plot))
  expect_warning(ggplot_gtable(built), "Position guide is perpendicular")

  # When somehow managed to get invalid position
  brks <- seq(0, 10, by = 2)
  sc <- scale_x_continuous(breaks = brks)
  sc$train(c(0, 10))
  sc <- ggplot2:::view_scale_primary(sc)

  gu <- guide_axis_vanilla()
  gu <- guide_train(gu, sc)
  gu$position <- "bamboozled"
  expect_null(check_position(gu))


})

test_that("GuideAxis does not warn about duplicate breaks", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    scale_y_continuous(breaks = c(20, 20), guide = guide_axis_vanilla())
  built <- expect_silent(ggplot_build(plot))
  expect_silent(ggplot_gtable(built))
})

test_that("a warning is generated when >1 guide is drawn at position", {
  plot <- ggplot(mpg, aes(class, hwy)) +
    geom_point() +
    guides(
      y     = guide_axis_vanilla(position = "left"),
      y.sec = guide_axis_vanilla(position = "left")
    )
  built <- expect_silent(ggplot_build(plot))
  suppressWarnings(
    expect_warning(ggplot_gtable(built), "Discarding guide")
  )
})

# Helpers -----------------------------------------------------------------

test_that("unlanguage formats language", {

  expect_equal(unlanguage("foo"), "foo")
  expect_equal(unlanguage(list(b = call("round", 10.5))),
               expression(b = round(10.5)))
  expect_equal(unlanguage(list("foo", "bar")), c("foo", "bar"))
})

test_that("clapply conditionally lapplies", {
  x <- list(c(1:2), c(3:4), c(5:6))
  ans <- clapply(x, c(TRUE, FALSE, TRUE), `*`, 2)
  expect_equal(ans, list(c(2, 4), c(3, 4), c(10, 12)))
})

test_that("list_has_name checks for names", {
  x <- list(a = list(b = 1, c = 2), d = list(e = 3, f = 4))
  expect_equal(list_has_name(x, "e"), c(FALSE, TRUE))
})

test_that("absGrob accepts gtables", {
  x <- gtable(widths = unit(c(1, 2), "cm"), heights = unit(c(3, 4), "inch"))
  ans <- absGrob(x)
  expect_equal(unitType(ans$width), "cm")
  expect_equal(as.numeric(ans$width), 3)
  expect_equal(unitType(ans$height), "inches")
  expect_equal(as.numeric(ans$height), 7)
})

