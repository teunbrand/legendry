test_that("guide_colbar works in all positions", {

  base <- ggplot(mtcars, aes(disp, mpg, colour = cyl)) +
    geom_point(shape = 21) +
    scale_colour_viridis_c(
      oob = oob_squish,
      guide = compose_stack(
        guide_colbar(show = c(FALSE, FALSE)),
        guide_colbar(show = c(TRUE,  FALSE)),
        guide_colbar(show = c(FALSE, TRUE)),
        guide_colbar(show = c(TRUE,  TRUE))
      )
    ) +
    theme(
      legend.frame = element_rect(colour = "black"),
      legend.ticks = element_line(colour = "black"),
      legend.box.just = "center"
    )

  suppressWarnings({
    vdiffr::expect_doppelganger("right position", base + theme(legend.position = "right"))
    vdiffr::expect_doppelganger("left position", base + theme(legend.position = "left"))
    vdiffr::expect_doppelganger("bottom position", base + theme(legend.position = "bottom"))
    vdiffr::expect_doppelganger("top position", base + theme(legend.position = "top"))
  })
})

test_that("guide_colbar can display side titles", {

  p <- ggplot(mtcars, aes(disp, mpg, colour = cyl)) +
    geom_point() +
    scale_colour_viridis_c(
      guide = guide_colbar(
        suppress_labels = FALSE,
        first_guide  = compose_stack("axis_base", side.titles = "foo"),
        second_guide = compose_stack("axis_base", side.titles = "bar")
      )
    )

  suppressWarnings({
    vdiffr::expect_doppelganger("colbar side titles", p)
  })
})
