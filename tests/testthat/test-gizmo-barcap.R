
test_that("gizmo_barcap trains correctly", {

  scale <- scale_colour_viridis_c(na.value = "#FF0000")
  scale$train(c(10.0, 30.0))

  guide <- gizmo_barcap(key = key_sequence(11L))

  # No out-of-bounds
  params <- guide$train(guide$params, scale, "colour")
  expect_identical(params$limits, c(10.0, 30.0))
  expect_identical(params$key$.value, seq(10.0, 30.0, length.out = 11L))
  expect_identical(params$key$colour[c(1L, 11L)], c("#440154", "#FDE725"))

  # Upper out-of-bounds
  scale <- scale_colour_viridis_c(limits = c(10.0, 30.0), na.value = "#FF0000")
  scale$train(c(10.0, 40.0))

  params <- guide$train(guide$params, scale, "colour")
  expect_identical(params$limits, c(10.0, 30.0391), tolerance = 1e-3)
  expect_identical(nrow(params$key), 12L)
  expect_identical(params$key$colour[c(1L, 12L)], c("#440154", "#FF0000"))

  # Lower out-of-bounds
  scale <- scale_colour_viridis_c(limits = c(10.0, 30.0), na.value = "#FF0000")
  scale$train(c(0.0, 30.0))

  params <- guide$train(guide$params, scale, "colour")
  expect_identical(params$limits, c(9.98, 30.0), tolerance = 1e-3)
  expect_identical(nrow(params$key), 12L)
  expect_identical(params$key$colour[c(1L, 12L)], c("#FF0000", "#FDE725"))

  # Both out of bounds
  scale <- scale_colour_viridis_c(limits = c(10.0, 30.0), na.value = "#FF0000")
  scale$train(c(0.0, 40.0))

  params <- guide$train(guide$params, scale, "colour")
  expect_identical(params$limits, c(9.98, 30.02), tolerance = 1e-3)
  expect_identical(nrow(params$key), 13L)
  expect_identical(params$key$colour[c(1L, 13L)], c("#FF0000", "#FF0000"))

  # Can ignore out of bounds by setting upper/lower to FALSE
  params <- guide$params
  params$show <- c(FALSE, FALSE)
  params <- guide$train(params, scale, "colour")
  expect_identical(params$limits, c(10.0, 30.0))
  expect_identical(nrow(params$key), 11L)
  expect_identical(params$key$colour[c(1L, 11L)], c("#440154", "#FDE725"))

  # Can include caps even when all in bounds
  scale <- scale_colour_viridis_c(na.value = "#FF0000")
  scale$train(c(10.0, 30.0))

  params <- guide$params
  params$show <- c(TRUE, TRUE)
  params <- guide$train(params, scale, "colour")
  expect_identical(params$limits, c(9.98, 30.02), tolerance = 1e-3)
  expect_identical(nrow(params$key), 13L)
  expect_identical(params$key$colour[c(1L, 13L)], c("#FF0000", "#FF0000"))

  # Can also squish into limits
  params <- guide$params
  params$show <- c(TRUE, TRUE)
  params$oob <- oob_squish
  params <- guide$train(params, scale, "colour")
  expect_identical(params$limits, c(10.0, 30.0))
  expect_identical(nrow(params$key), 13L)
  expect_identical(params$key$colour[c(1L, 13L)], c("#440154", "#FDE725"))
})

test_that("gizmo_barcap shows caps correctly", {
  p <- ggplot(mpg, aes(displ, hwy, colour = cty)) + geom_point() +
    scale_colour_viridis_c(
      oob = oob_squish,
      guide = gizmo_barcap(show = TRUE)
    ) +
    theme(legend.frame = element_rect(colour = "black"),
          legend.key.size = unit(1.0, "cm"))
  b <- ggplot_build(p)

  theme <- b@plot@theme
  theme$legend.key.width  <- theme$legend.key.size
  theme$legend.key.height <- theme$legend.key.size

  guide <- b$plot$guides
  old_params <- guide$params[[1L]]
  guide <- guide$guides[[1L]]

  draw_guide <- function(guide, params, theme) {
    hor <-
      guide$draw(theme, position = "right", direction = "horizontal", params)
    ver <-
      guide$draw(theme, position = "right", direction = "vertical", params)
    gt <- gtable(width = unit(c(1.0, 1.0), "null"), heights = unit(1.0, "null"))
    gtable_add_grob(gt, list(hor, ver), t = 1L, l = 1L:2L)
  }

  params <- old_params
  gt <- draw_guide(guide, params, theme)
  vdiffr::expect_doppelganger("triangle-cap", gt)

  params$size <- unit(2.0, "cm")
  gt <- draw_guide(guide, params, theme)
  vdiffr::expect_doppelganger("triangle-cap-long", gt)

  params$size <- NULL
  params$shape <- cap_round(n = 30L)
  gt <- draw_guide(guide, params, theme)
  vdiffr::expect_doppelganger("round-cap", gt)

  params$size <- unit(2.0, "cm")
  params$shape <- cap_round(n = 30L)
  gt <- draw_guide(guide, params, theme)
  vdiffr::expect_doppelganger("round-cap-long", gt)

  params$size <- NULL
  params$shape <- cap_arch(n = 30L)
  gt <- draw_guide(guide, params, theme)
  vdiffr::expect_doppelganger("arch-cap", gt)

  params$size <- unit(2.0, "cm")
  params$shape <- cap_arch(n = 30L)
  gt <- draw_guide(guide, params, theme)
  vdiffr::expect_doppelganger("arch-cap-long", gt)

  params$size <- NULL
  params$shape <- cap_ogee(n = 30L)
  gt <- draw_guide(guide, params, theme)
  vdiffr::expect_doppelganger("ogee-cap", gt)

  params$size <- unit(2.0, "cm")
  params$shape <- cap_ogee(n = 30L)
  gt <- draw_guide(guide, params, theme)
  vdiffr::expect_doppelganger("ogee-cap-long", gt)

})
