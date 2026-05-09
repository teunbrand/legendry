
test_that("gizmo_density can compute density in various ways", {

  values <- mtcars$mpg
  target <- density(mtcars$mpg, n = 10L)
  scale  <- scale_colour_gradient(limits = range(values))
  scale$train(values)

  guide <- gizmo_density(density = target)
  expect_identical(guide$params$density[c("x", "y")], target[c("x", "y")])

  guide <- gizmo_density(density = values, density.args = list(n = 10L))
  params <- guide$train(guide$params, scale, "colour")
  expect_identical(params$decor[c("x", "y")], target[c("x", "y")])

  p <- ggplot(mtcars, aes(drat, wt, colour = mpg)) + geom_point() +
    guides(colour = gizmo_density(density.args = list(n = 10L)))
  b <- ggplot_build(p)
  result <- b$plot$guides$params[[1L]]$decor

  expect_identical(result$x, target$x)
  expect_identical(result$y, rescale_max(target$y, to = c(0.0, 0.9)))

})

test_that("gizmo_density can handle sequential and bin keys", {

  scale <- scale_colour_viridis_c(limits = c(10.0, 30.0))
  scale$train(c(10.0, 30.0))
  colours <- scale$map(seq(10.0, 30.0, length.out = 5L))

  guide  <- gizmo_density(key = key_sequence(n = 5L))
  params <- guide$train(guide$params, scale, "colour")

  expect_identical(params$key$.value, seq(10.0, 30.0, by = 5L))
  expect_identical(params$key$colour, colours)

  scale <- scale_colour_viridis_c(limits = c(10.0, 30.0))
  scale$train(c(0.0, 40.0))

  params <- guide$train(guide$params, scale, "colour")
  expect_identical(params$key$.value, c(9.98, seq(10.0, 30.0, by = 5L), 30.02))
  expect_identical(params$key$colour, c("#7F7F7F", colours, "#7F7F7F"))

  scale <- scale_colour_viridis_c(limits = c(10.0, 30.0))
  scale$train(c(10.0, 30.0))
  colours <- scale$map(seq(12.5, 27.5, length.out = 4L))

  guide  <- gizmo_density(key = key_bins())
  params <- guide$train(guide$params, scale, "colour")

  expect_identical(params$key$min, c(10.0, 15.0, 20.0, 25.0, NA))
  expect_identical(params$key$max, c(15.0, 20.0, 25.0, 30.0, NA))
  expect_identical(params$key$colour, c(colours, NA))

  scale <- scale_colour_viridis_c(limits = c(10.0, 30.0))
  scale$train(c(0.0, 40.0))

  params <- guide$train(guide$params, scale, "colour")
  key <- vec_slice(params$key, !is.na(params$key$colour))

  expect_identical(key$colour, c("#7F7F7F", colours, "#7F7F7F"))
  expect_identical(key$min, c(-Inf, 10.0, 15.0, 20.0, 25.0, 30.0))
  expect_identical(key$max, c(10.0, 15.0, 20.0, 25.0, 30.0, Inf))

})

test_that("check_density throws appropriate errors", {

  expect_silent(
    check_density(density(mtcars$mpg))
  )

  expect_error(
    check_density(arg = "x"),
    "cannot be missing"
  )
  expect_error(
    check_density(list(foo = 1.0, bar = 2.0)),
    "must have named"
  )
  expect_error(
    check_density(list(x = 1.0, y = 1.0)),
    "at least length 2"
  )
  expect_error(
    check_density(list(x = 1L:3L, y = 1L:2L)),
    "must be of equal length"
  )
})
