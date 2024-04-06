
test_that("gizmo_stepcap trains correctly with even.steps = FALSE", {

  na_value <- "#FF0000" # red
  make_scale <- function(limits = NULL, train) {
    scale <- scale_colour_viridis_c(limits = limits, na.value = na_value)
    scale$train(train)
    scale
  }

  guide <- gizmo_stepcap(key = key_bins(even.steps = FALSE, show.limits = TRUE))

  scale  <- make_scale(train = c(10, 30))
  params <- guide$train(guide$params, scale, "colour")

  # Test all key columns
  expect_equal(params$limits, c(10, 30))
  expect_equal(dim(params$key), c(5, 5))
  expect_equal(params$key$colour[c(1,4,5)], c("#452F73", "#B0DA45", NA))
  expect_equal(params$key$min, c(10, 15, 20, 25, NA))
  expect_equal(params$key$max, c(15, 20, 25, 30, NA))
  expect_equal(params$key$.label, c("10", 15, 20, 25, 30))
  expect_equal(params$key$.value, c(10, 15, 20, 25, 30))

  # Upper out of bounds
  scale  <- make_scale(c(10, 30), c(10, 40))
  params <- guide$train(guide$params, scale, "colour")

  expect_equal(params$limits, c(10, 30.02))
  expect_equal(dim(params$key), c(5, 5)) # still 5 because last row was empty
  expect_equal(params$key$colour[c(1,4,5)], c("#452F73", "#B0DA45", na_value))
  expect_equal(params$key$max, c(15, 20, 25, 30, Inf))

  # Lower out of bounds
  scale  <- make_scale(c(10, 30), c(0, 30))
  params <- guide$train(guide$params, scale, "colour")

  expect_equal(params$limits, c(9.98, 30))
  expect_equal(dim(params$key), c(6, 5)) # row insertion
  expect_equal(params$key$colour[c(1,2,5,6)], c(na_value, "#452F73", "#B0DA45", NA))
  expect_equal(params$key$min, c(-Inf, 10, 15, 20, 25, NA))

  # Both out of bounds
  scale <- make_scale(c(10, 30), c(0, 40))
  params <- guide$train(guide$params, scale, "colour")

  expect_equal(params$limits, c(9.98, 30.02))
  expect_equal(dim(params$key), c(6, 5)) # lower row insertion
  expect_equal(params$key$colour[c(1,2,5,6)], c(na_value, "#452F73", "#B0DA45", na_value))
  expect_equal(params$key$min, c(-Inf, 10, 15, 20, 25, 30))
  expect_equal(params$key$max, c(10, 15, 20, 25, 30, Inf))

})

test_that("gizmo_stepcap trains correctly with even.steps = TRUE", {

  na_value <- "#FF0000" # red
  colours  <- c("#440154", "#21908C", "#FDE725")
  breaks   <- c(15, 20)

  make_scale <- function(limits = NULL, train) {
    scale <- scale_colour_viridis_b(
      limits = limits, na.value = na_value, breaks = breaks,
      oob = oob_censor
    )
    scale$train(train)
    scale
  }

  guide <- gizmo_stepcap(key = key_bins(even.steps = TRUE))

  scale  <- make_scale(train = c(10, 40))
  params <- guide$train(guide$params, scale, "colour")

  # Test all key columns
  expect_equal(params$limits, c(10, 40))
  expect_equal(dim(params$key), c(4, 5))
  expect_equal(params$key$colour, c(colours, NA))
  expect_equal(params$key$min, c(10, 20, 30, NA))
  expect_equal(params$key$max, c(20, 30, 40, NA))
  expect_equal(params$key$.label, c(NA, "15", 20, NA))
  expect_equal(params$key$.value, c(NA, 20, 30, NA))

  # Upper out of bounds
  scale  <- make_scale(limits = c(10, 40), c(10, 50))
  params <- guide$train(guide$params, scale, "colour")

  expect_equal(params$limits, c(10, 40.03))
  expect_equal(dim(params$key), c(4, 5)) # still 4 because last row was empty
  expect_equal(params$key$colour, c(colours, na_value))
  expect_equal(params$key$max, c(20, 30, 40, Inf))

  # Lower out of bounds
  scale <- make_scale(limits = c(10, 40), train = c(0, 40))
  params <- guide$train(guide$params, scale, "colour")

  expect_equal(params$limits, c(9.97, 40))
  expect_equal(dim(params$key), c(5, 5)) # row insertion
  expect_equal(params$key$colour, c(na_value, colours, NA))
  expect_equal(params$key$min, c(-Inf, 10, 20, 30, NA))

  # Both out of bounds
  scale <- make_scale(limits = c(10, 40), train = c(0, 50))
  params <- guide$train(guide$params, scale, "colour")

  expect_equal(params$limits, c(9.97, 40.03))
  expect_equal(dim(params$key), c(5, 5)) # lower row insertion
  expect_equal(params$key$colour, c(na_value, colours, na_value))
  expect_equal(params$key$min, c(-Inf, 10, 20, 30, 40))
  expect_equal(params$key$max, c(10, 20, 30, 40, Inf))

})

test_that("gizmo_stepcap can use show.limits correctly", {


  make_scale <- function(limits = NULL, breaks) {
    scale <- scale_colour_viridis_b(
      limits = limits, breaks = breaks, oob = oob_censor
    )
    scale$train(limits)
    scale
  }
  scale <- make_scale(c(10, 30), breaks = c(15, 20))

  # For even steps = FALSE
  guide <- gizmo_stepcap(key = key_bins(even.steps = FALSE, show.limits = FALSE))
  params <- guide$train(guide$params, scale, "colour")

  expect_equal(params$key$.label, c(NA, "15", 20, NA))
  expect_equal(params$key$.value, c(NA, 15, 20, NA))

  guide <- gizmo_stepcap(key = key_bins(even.steps = FALSE, show.limits = TRUE))
  params <- guide$train(guide$params, scale, "colour")

  expect_equal(params$key$.label, c(10, "15", 20, 30))
  expect_equal(params$key$.value, c(10, 15, 20, 30))

  scale <- make_scale(c(10, 40), breaks = c(15, 20))

  # For even steps = TRUE
  guide <- gizmo_stepcap(key = key_bins(even.steps = TRUE, show.limits = FALSE))
  params <- guide$train(guide$params, scale, "colour")

  expect_equal(params$key$.value, c(NA, 20, 30, NA))
  expect_equal(params$key$.label, c(NA, "15", 20, NA))

  guide <- gizmo_stepcap(key = key_bins(even.steps = TRUE, show.limits = TRUE))
  params <- guide$train(guide$params, scale, "colour")

  expect_equal(params$key$.value, c(10, 20, 30, 40))
  expect_equal(params$key$.label, c("10", 15, 20, 40))
})
