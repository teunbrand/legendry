test_that("compose_crux can compose a legend", {

  colours <- c("dodgerblue", "limegreen", "tomato", "goldenrod")

  themes <- lapply(colours, function(col) {
    theme(
      legend.text = element_text(colour = col),
      legend.axis.line = element_line(colour = col),
      legend.ticks = element_line(colour = col)
    )
  })

  guides <- lapply(colours, function(col) {
    theme <- theme(
      legend.text = element_text(colour = col),
      legend.axis.line = element_line(colour = col),
      legend.ticks = element_line(colour = col),
      legend.title = element_text(colour = col)
    )
    compose_stack(
      guide_axis_custom,
      primitive_title(title = col),
      theme = theme
    )
  })

  crux <- compose_crux(
    key = key_auto(),
    centre = gizmo_grob(circleGrob(r = unit(2, "cm"))),
    top = guides[[1]], bottom = guides[[2]],
    left = guides[[3]], right = guides[[4]]
  )


  p <- ggplot(mpg, aes(displ, hwy, colour = cty)) +
    geom_point() +
    guides(colour = crux)

  vdiffr::expect_doppelganger("compose-crux legend", p)
})
