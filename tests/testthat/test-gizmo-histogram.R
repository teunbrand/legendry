test_that("gizmo_histogram can compute histograms in various ways", {

  values <- mtcars$mpg
  target <- hist(mtcars$mpg, breaks = 10L, plot = FALSE)
  scale  <- scale_colour_gradient(limits = range(values))
  scale$train(values)
  fields <- c("breaks", "counts")

  guide <- gizmo_histogram(hist = target)
  expect_identical(guide$params$hist[fields], target[fields])

  guide <- gizmo_histogram(hist = values, hist.args = list(breaks = 10L))
  params <- guide$train(guide$params, scale, "colour")
  expect_identical(params$decor[fields], target[fields])

  p <- ggplot(mtcars, aes(drat, wt, colour = mpg)) + geom_point() +
    guides(colour = gizmo_histogram(hist.args = list(breaks = 10L)))
  b <- ggplot_build(p)
  result <- b$plot$guides$params[[1L]]$decor

  expect_identical(result$x, rep(target$breaks, each = 2L))
  expect_identical(
    result$y,
    c(0.0, rep(rescale_max(target$counts, to = c(0.0, 0.9)), each = 2L), 0.0)
  )

})

test_that("check_histogram throws appropriate errors", {

  expect_silent(
    check_histogram(hist(mtcars$mpg, plot = FALSE))
  )

  expect_error(
    check_histogram(arg = "x"),
    "cannot be missing"
  )
  expect_error(
    check_histogram(list(foo = 1L, bar = 2L)),
    "must have named"
  )
  expect_error(
    check_histogram(list(breaks = 1L, counts = 1L)),
    "should be exactly 1 longer"
  )
  expect_error(
    check_histogram(list(breaks = 1L, counts = integer())),
    "more than or equal to 2"
  )
})

test_that("hist(plot = TRUE) is suppressed", {
  skip_if_not_installed("ragg")

  # Normally hist should shade pixels in ragg's buffer
  cap <- ragg::agg_capture()
  hist(mtcars$mpg, plot = TRUE)
  img <- cap()
  dev.off()
  expect_gt(length(unique(as.vector(img))), 1L)

  # But using it in `gizmo_histogram()` should suppress that
  cap <- ragg::agg_capture()
  gizmo_histogram(hist = hist(mtcars$mpg, plot = TRUE))
  img <- cap()
  dev.off()
  expect_identical(unique(as.vector(img)), "white")
})
