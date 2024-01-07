test_that("gizmo_grob works for positions", {

  gizmo <- circleGrob(r = unit(1, "cm"))

  p <- ggplot(mpg, aes(displ, hwy)) +
    geom_blank() +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.line = element_line()
    ) +
    guides(
      x = gizmo_grob(gizmo, hjust = 0.75),
      x.sec = gizmo_grob(gizmo, hjust = 0.25),
      y = gizmo_grob(gizmo, vjust = 0.25),
      y.sec = gizmo_grob(gizmo, vjust = 0.75)
    )

  vdiffr::expect_doppelganger("gizmo_grob cartesian", p)
})

test_that("gizmo_grob works for legends", {

  circle <- circleGrob(r = unit(1, "cm"))
  rect   <- rectGrob()

  p <- ggplot(mtcars, aes(disp, mpg, shape = factor(cyl), colour = cyl)) +
    geom_point() +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.line = element_line()
    ) +
    guides(
      colour = gizmo_grob(
        rect, width = unit(1, "cm"), height = unit(2, "cm"),
        position = "bottom"
      ),
      shape = gizmo_grob(circle)
    )

  vdiffr::expect_doppelganger("gizmo_grob legends", p)
})
