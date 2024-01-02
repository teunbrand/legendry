
test_that("guide_box works as axis", {

  base <- ggplot(mpg, aes(displ, interaction(drv, year))) +
    geom_blank() +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.line = element_line()
    )

  p <- base +
    guides(
      y = guide_box(),
      y.sec = guide_box(
        min_size = 1,
        key = key_range_auto(reverse = TRUE), drop_zero = FALSE
      ),
      x = guide_box(
        key = key_range_manual(start = c(2, 4), end = c(5, 7), name = c("A\nA", "B\nB"))
      ),
      x.sec = guide_box(
        key = key_range_manual(
          start = c(2, 4, 3), end = c(5, 7, 6), level = c(1, 2, 3)
        )
      )
    )

  vdiffr::expect_doppelganger("guide_box cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, donut = 0.5) +
    guides(
      r = guide_box(),
      r.sec = guide_box(
        key = key_range_auto(reverse = TRUE), drop_zero = FALSE
      ),
      theta = guide_box(
        key = key_range_manual(start = c(2, 4), end = c(5, 7), name = c("A\nA", "B\nB"))
      ),
      theta.sec = guide_box(
        key = key_range_manual(
          start = c(2, 4, 3), end = c(5, 7, 6),
          level = c(1, 2, 3)
        )
      )
    )

  vdiffr::expect_doppelganger("guide_box radial", p)
})
