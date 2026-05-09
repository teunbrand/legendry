
test_that("gizmo_stepcap trains correctly with even.steps = FALSE", {

  na_value <- "#FF0000" # red
  make_scale <- function(limits = NULL, train = numeric()) {
    scale <- scale_colour_viridis_c(limits = limits, na.value = na_value)
    scale$train(train)
    scale
  }

  guide <- gizmo_stepcap(key = key_bins(even.steps = FALSE, show.limits = TRUE))

  scale  <- make_scale(train = c(10.0, 30.0))
  params <- guide$train(guide$params, scale, "colour")

  # Test all key columns
  expect_identical(params$limits, c(10.0, 30.0))
  expect_identical(dim(params$key), c(5L, 5L))
  expect_identical(
    params$key$colour[c(1L, 4L, 5L)],
    c("#452F73", "#B0DA45", NA)
  )
  expect_identical(params$key$min, c(10.0, 15.0, 20.0, 25.0, NA))
  expect_identical(params$key$max, c(15.0, 20.0, 25.0, 30.0, NA))
  expect_identical(params$key$.label, c("10", "15", "20", "25", "30"))
  expect_identical(params$key$.value, c(10.0, 15.0, 20.0, 25.0, 30.0))

  # Upper out of bounds
  scale  <- make_scale(c(10.0, 30.0), c(10.0, 40.0))
  params <- guide$train(guide$params, scale, "colour")

  expect_identical(params$limits, c(10.0, 30.02))
  expect_identical(dim(params$key), c(5L, 5L)) # still 5 because last row was empty
  expect_identical(
    params$key$colour[c(1L, 4L, 5L)],
    c("#452F73", "#B0DA45", na_value)
  )
  expect_identical(params$key$max, c(15.0, 20.0, 25.0, 30.0, Inf))

  # Lower out of bounds
  scale  <- make_scale(c(10.0, 30.0), c(0.0, 30.0))
  params <- guide$train(guide$params, scale, "colour")

  expect_identical(params$limits, c(9.98, 30.0))
  expect_identical(dim(params$key), c(6L, 5L)) # row insertion
  expect_identical(
    params$key$colour[c(1L, 2L, 5L, 6L)],
    c(na_value, "#452F73", "#B0DA45", NA)
  )
  expect_equal(params$key$min, c(-Inf, 10.0, 15.0, 20.0, 25.0, NA))

  # Both out of bounds
  scale <- make_scale(c(10.0, 30.0), c(0.0, 40.0))
  params <- guide$train(guide$params, scale, "colour")

  expect_identical(params$limits, c(9.98, 30.02))
  expect_identical(dim(params$key), c(6L, 5L)) # lower row insertion
  expect_identical(
    params$key$colour[c(1L, 2L, 5L, 6L)],
    c(na_value, "#452F73", "#B0DA45", na_value)
  )
  expect_identical(params$key$min, c(-Inf, 10.0, 15.0, 20.0, 25.0, 30.0))
  expect_identical(params$key$max, c(10.0, 15.0, 20.0, 25.0, 30.0, Inf))

})

test_that("gizmo_stepcap trains correctly with even.steps = TRUE", {

  na_value <- "#FF0000" # red
  colours  <- c("#440154", "#21908C", "#FDE725")
  breaks   <- c(15.0, 20.0)

  make_scale <- function(limits = NULL, train = numeric()) {
    scale <- scale_colour_viridis_b(
      limits = limits, na.value = na_value, breaks = breaks,
      oob = oob_censor
    )
    scale$train(train)
    scale
  }

  guide <- gizmo_stepcap(key = key_bins(even.steps = TRUE))

  scale  <- make_scale(train = c(10.0, 40.0))
  params <- guide$train(guide$params, scale, "colour")

  # Test all key columns
  expect_identical(params$limits, c(10.0, 40.0))
  expect_identical(dim(params$key), c(4L, 5L))
  expect_identical(params$key$colour, c(colours, NA))
  expect_identical(params$key$min, c(10.0, 20.0, 30.0, NA))
  expect_identical(params$key$max, c(20.0, 30.0, 40.0, NA))
  expect_identical(params$key$.label, c(NA, "15", "20", NA))
  expect_identical(params$key$.value, c(NA, 20.0, 30.0, NA))

  # Upper out of bounds
  scale  <- make_scale(limits = c(10.0, 40.0), c(10.0, 50.0))
  params <- guide$train(guide$params, scale, "colour")

  expect_identical(params$limits, c(10.0, 40.03))
  expect_identical(dim(params$key), c(4L, 5L)) # still 4 because last row was empty
  expect_identical(params$key$colour, c(colours, na_value))
  expect_identical(params$key$max, c(20.0, 30.0, 40.0, Inf))

  # Lower out of bounds
  scale <- make_scale(limits = c(10.0, 40.0), train = c(0.0, 40.0))
  params <- guide$train(guide$params, scale, "colour")

  expect_identical(params$limits, c(9.97, 40.0))
  expect_identical(dim(params$key), c(5L, 5L)) # row insertion
  expect_identical(params$key$colour, c(na_value, colours, NA))
  expect_identical(params$key$min, c(-Inf, 10.0, 20.0, 30.0, NA))

  # Both out of bounds
  scale <- make_scale(limits = c(10.0, 40.0), train = c(0.0, 50.0))
  params <- guide$train(guide$params, scale, "colour")

  expect_identical(params$limits, c(9.97, 40.03))
  expect_identical(dim(params$key), c(5L, 5L)) # lower row insertion
  expect_identical(params$key$colour, c(na_value, colours, na_value))
  expect_identical(params$key$min, c(-Inf, 10.0, 20.0, 30.0, 40.0))
  expect_identical(params$key$max, c(10.0, 20.0, 30.0, 40.0, Inf))

})

test_that("gizmo_stepcap can use show.limits correctly", {


  make_scale <- function(limits = NULL, breaks = numeric()) {
    scale <- scale_colour_viridis_b(
      limits = limits, breaks = breaks, oob = oob_censor
    )
    scale$train(limits)
    scale
  }
  scale <- make_scale(c(10.0, 30.0), breaks = c(15.0, 20.0))

  # For even steps = FALSE
  guide <-
    gizmo_stepcap(key = key_bins(even.steps = FALSE, show.limits = FALSE))
  params <- guide$train(guide$params, scale, "colour")

  expect_identical(params$key$.label, c(NA, "15", "20", NA))
  expect_identical(params$key$.value, c(NA, 15.0, 20.0, NA))

  guide <-
    gizmo_stepcap(key = key_bins(even.steps = FALSE, show.limits = TRUE))
  params <- guide$train(guide$params, scale, "colour")

  expect_identical(params$key$.label, c("10", "15", "20", "30"))
  expect_identical(params$key$.value, c(10.0, 15.0, 20.0, 30.0))

  scale <- make_scale(c(10.0, 40.0), breaks = c(15.0, 20.0))

  # For even steps = TRUE
  guide <-
    gizmo_stepcap(key = key_bins(even.steps = TRUE, show.limits = FALSE))
  params <- guide$train(guide$params, scale, "colour")

  expect_identical(params$key$.value, c(NA, 20.0, 30.0, NA))
  expect_identical(params$key$.label, c(NA, "15", "20", NA))

  guide <-
    gizmo_stepcap(key = key_bins(even.steps = TRUE, show.limits = TRUE))
  params <- guide$train(guide$params, scale, "colour")

  expect_identical(params$key$.value, c(10.0, 20.0, 30.0, 40.0))
  expect_identical(params$key$.label, c("10", "15", "20", "40"))
})
