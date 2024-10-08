test_that("compose_sandwich can compose a legend", {

  sandwich <- compose_sandwich(
    middle = guide_colourbar(theme = theme(text = element_text(colour = "limegreen"))),
    text   = guide_axis_base(theme = theme(text = element_text(colour = "tomato"))),
    opposite = primitive_bracket(key = key_range_manual(
      c(10, 20), c(25, 30), c("A", "B")
    ), theme = theme(text = element_text(colour = "dodgerblue")))
  )


  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_point(aes(colour = cty)) +
    scale_colour_viridis_c(
      guide = sandwich
    )

  vdiffr::expect_doppelganger(
    "vertical sandwich", p + theme(
      legend.text.position = "right",
      legend.position = "right"
    )
  )

  vdiffr::expect_doppelganger(
    "vertical sandwich flipped", p + theme(
      legend.text.position = "left",
      legend.position = "right"
    )
  )

  vdiffr::expect_doppelganger(
    "horizontal sandwich", p + theme(
      legend.text.position = "bottom",
      legend.position = "bottom"
    )
  )

  vdiffr::expect_doppelganger(
    "horizontal sandwich flipped", p + theme(
      legend.text.position = "top",
      legend.position = "bottom"
    )
  )
})
