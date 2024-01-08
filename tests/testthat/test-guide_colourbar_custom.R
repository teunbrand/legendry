test_that("guide_colourbar_custom works in all positions", {

  base <- ggplot(mtcars, aes(disp, mpg, colour = cyl, fill = wt)) +
    geom_point(shape = 21) +
    theme(
      legend.frame = element_rect(colour = "black"),
      legend.ticks = element_line(colour = "black", arrow = arrow(length = unit(2, "mm"))),
    )

  p <- base +
    scale_colour_viridis_c(guide = guide_colourbar_custom(position = "right")) +
    scale_fill_viridis_c(guide = guide_colourbar_custom(position = 'bottom'))

  suppressWarnings(
    expect_snapshot_warning(
      vdiffr::expect_doppelganger("bottom right position", p),
    )
  )

  p <- base +
    scale_colour_viridis_c(guide = guide_colourbar_custom(position = "top")) +
    scale_fill_viridis_c(guide = guide_colourbar_custom(position = 'left'))

  suppressWarnings(
    expect_snapshot_warning(
      vdiffr::expect_doppelganger("top left position", p),
    )
  )
})
