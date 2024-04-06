test_that("guide_colourbar_custom works in all positions", {

  key <- key_bins(show.limits = TRUE)
  base <- ggplot(mtcars, aes(disp, mpg, colour = cyl)) +
    geom_point(shape = 21) +
    scale_colour_viridis_c(
      oob = oob_squish,
      guide = compose_stack(
        guide_coloursteps_custom(show = c(FALSE, FALSE)),
        guide_coloursteps_custom(show = c(TRUE,  FALSE)),
        guide_coloursteps_custom(show = c(FALSE, TRUE)),
        guide_coloursteps_custom(show = c(TRUE,  TRUE))
      )
    ) +
    theme(
      legend.frame = element_rect(colour = "black"),
      legend.ticks = element_line(colour = "black"),
      legend.box.just = "center"
    )

  suppressWarnings({
    vdiffr::expect_doppelganger("right position",  base + theme(legend.position = "right"))
    vdiffr::expect_doppelganger("left position",   base + theme(legend.position = "left"))
    vdiffr::expect_doppelganger("bottom position", base + theme(legend.position = "bottom"))
    vdiffr::expect_doppelganger("top position",    base + theme(legend.position = "top"))
  })
})
