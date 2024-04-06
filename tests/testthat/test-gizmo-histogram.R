test_that("gizmo_histogram can compute histograms in various ways", {

  values <- mtcars$mpg
  target <- hist(mtcars$mpg, breaks = 10, plot = FALSE)
  scale  <- scale_colour_continuous(limits = range(values))
  scale$train(values)
  fields <- c("breaks", "counts")

  guide <- gizmo_histogram(hist = target)
  expect_equal(guide$params$hist[fields], target[fields])

  guide <- gizmo_histogram(hist = values, hist.args = list(breaks = 10))
  params <- guide$train(guide$params, scale, "colour")
  expect_equal(params$decor[fields], target[fields])

  p <- ggplot(mtcars, aes(drat, wt, colour = mpg)) + geom_point() +
    guides(colour = gizmo_histogram(hist.args = list(breaks = 10)))
  b <- ggplot_build(p)
  result <- b$plot$guides$params[[1]]$decor

  expect_equal(result$x, rep(target$breaks, each = 2))
  expect_equal(result$y, c(0, rep(rescale_max(target$counts, to = c(0, 0.9)), each = 2), 0))

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
    check_histogram(list(foo = 1, bar = 2)),
    "must have named"
  )
  expect_error(
    check_histogram(list(breaks = 1, counts = 1)),
    "should be exactly 1 longer"
  )
  expect_error(
    check_histogram(list(breaks = 1, counts = integer())),
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
  expect_gt(length(unique(as.vector(img))), 1)

  # But using it in `gizmo_histogram()` should suppress that
  cap <- ragg::agg_capture()
  gizmo_histogram(hist = hist(mtcars$mpg, plot = TRUE))
  img <- cap()
  dev.off()
  expect_equal(unique(as.vector(img)), "white")
})
