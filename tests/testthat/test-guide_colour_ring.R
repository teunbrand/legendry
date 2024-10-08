test_that("guide_colring looks as it should", {

  # vdiffr's device doesn't support clipping paths yet, so we just ignore
  # warnings for now
  expect_doppelganger <- function(...) {
    suppressWarnings(vdiffr::expect_doppelganger(...))
  }

  p <- ggplot(mtcars, aes(disp, mpg, colour = drat)) +
    geom_point() +
    theme(
      legend.frame = element_rect(colour = "tomato"),
      legend.background = element_rect(colour = "limegreen")
    )

  outline <- compose_stack("axis_base", primitive_line(theme = theme(
    legend.axis.line = element_line(colour = "dodgerblue")
  )), theme = theme(gguidance.guide.spacing = unit(0, "cm")))

  standard_ring <- guides(colour = guide_colring(
    nbin = 15, outer_guide = outline, inner_guide = outline
  ))

  pring <- p + standard_ring

  expect_doppelganger(
    "standard ring", pring
  )

  pring <- p + guides(colour = guide_colring(
    nbin = 15, outer_guide = outline, inner_guide = outline, show_labels = "inner"
  ))

  expect_doppelganger(
    "inner labels", pring
  )

  pring <- p + standard_ring + theme(
    legend.key.size = unit(2, "lines"),
    legend.key.width = unit(5, "mm")
  )

  expect_doppelganger(
    "resized ring", pring
  )

  pring <- p + standard_ring + theme(legend.key.width = rel(2.5))

  expect_doppelganger(
    "conical", pring
  )

  pring <- p + guides(colour = guide_colring(
    nbin = 15, outer_guide = outline, inner_guide = outline,
    start = 0.25 * pi, end = 1.75 * pi
  ))

  expect_doppelganger(
    "open ring", pring
  )
})

test_that("ring_margin calculates margins correctly", {

  # Full ring should have outer margins everywhere
  test <- ring_margin(c(0, 2) * pi, outer = 2, inner = 1)
  expect_equal(as.numeric(test), c(2, 2, 2, 2))

  test <- ring_margin(c(0, 1) * pi, outer = 2, inner = 1)
  expect_equal(as.numeric(test), c(2, 2, 2, 0))

  test <- ring_margin(c(0.5, 1.5) * pi, outer = 2, inner = 1)
  expect_equal(as.numeric(test), c(0, 2, 2, 2))

  test <- ring_margin(c(1, 2) * pi, outer = 2, inner = 1)
  expect_equal(as.numeric(test), c(2, 0, 2, 2))

  test <- ring_margin(c(1.5, 0.5) * pi, outer = 2, inner = 1)
  expect_equal(as.numeric(test), c(2, 2, 0, 2))

  s2 <- sqrt(2)

  test <- ring_margin(c(0.25, 0.75) * pi, outer = 2, inner = 1)
  expect_equal(as.numeric(test), c(s2, 2, s2, s2 / 2))

  test <- ring_margin(c(0.75, 1.25) * pi, outer = 2, inner = 1)
  expect_equal(as.numeric(test), c(s2 / 2, s2, 2, s2))

  test <- ring_margin(c(1.25, 1.75) * pi, outer = 2, inner = 1)
  expect_equal(as.numeric(test), c(s2, s2 / 2, s2, 2))

  test <- ring_margin(c(1.75, 0.25) * pi, outer = 2, inner = 1)
  expect_equal(as.numeric(test), c(2, s2, s2 / 2, s2))

})
