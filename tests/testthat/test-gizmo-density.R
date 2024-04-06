
test_that("gizmo_density can compute density in various ways", {

  values <- mtcars$mpg
  target <- density(mtcars$mpg, n = 10)
  scale  <- scale_colour_continuous(limits = range(values))
  scale$train(values)

  guide <- gizmo_density(density = target)
  expect_equal(guide$params$density[c("x", "y")], target[c("x", "y")])

  guide <- gizmo_density(density = values, density.args = list(n = 10))
  params <- guide$train(guide$params, scale, "colour")
  expect_equal(params$decor[c("x", "y")], target[c("x", "y")])

  p <- ggplot(mtcars, aes(drat, wt, colour = mpg)) + geom_point() +
    guides(colour = gizmo_density(density.args = list(n = 10)))
  b <- ggplot_build(p)
  result <- b$plot$guides$params[[1]]$decor

  expect_equal(result$x, target$x)
  expect_equal(result$y, rescale_max(target$y, to = c(0, 0.9)))

})

test_that("gizmo_density can handle sequential and bin keys", {

  scale <- scale_colour_viridis_c(limits = c(10, 30))
  scale$train(c(10, 30))
  colours <- scale$map(seq(10, 30, length.out = 5))

  guide  <- gizmo_density(key = key_sequence(n = 5))
  params <- guide$train(guide$params, scale, "colour")

  expect_equal(params$key$.value, seq(10, 30, by = 5))
  expect_equal(params$key$colour, colours)

  scale <- scale_colour_viridis_c(limits = c(10, 30))
  scale$train(c(0, 40))

  params <- guide$train(guide$params, scale, "colour")
  expect_equal(params$key$.value, c(9.98, seq(10, 30, by = 5), 30.02))
  expect_equal(params$key$colour, c("#7F7F7F", colours, "#7F7F7F"))

  scale <- scale_colour_viridis_c(limits = c(10, 30))
  scale$train(c(10, 30))
  colours <- scale$map(seq(12.5, 27.5, length.out = 4))

  guide  <- gizmo_density(key = key_bins())
  params <- guide$train(guide$params, scale, "colour")

  expect_equal(params$key$min, c(10, 15, 20, 25, NA))
  expect_equal(params$key$max, c(15, 20, 25, 30, NA))
  expect_equal(params$key$colour, c(colours, NA))

  scale <- scale_colour_viridis_c(limits = c(10, 30))
  scale$train(c(0, 40))

  params <- guide$train(guide$params, scale, "colour")
  key <- vec_slice(params$key, !is.na(params$key$colour))

  expect_equal(key$colour, c("#7F7F7F", colours, "#7F7F7F"))
  expect_equal(key$min, c(-Inf, 10, 15, 20, 25, 30))
  expect_equal(key$max, c(10, 15, 20, 25, 30, Inf))

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
    check_density(list(foo = 1, bar = 2)),
    "must have named"
  )
  expect_error(
    check_density(list(x = 1, y = 1)),
    "at least length 2"
  )
  expect_error(
    check_density(list(x = 1:3, y = 1:2)),
    "must be of equal length"
  )
})
