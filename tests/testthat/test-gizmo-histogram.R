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
