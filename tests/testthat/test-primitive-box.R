
test_that("primitive_box works as axis", {

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
      y = primitive_box(),
      y.sec = primitive_box(
        min_size = 1,
        key = key_range_auto(reverse = TRUE), drop_zero = FALSE
      ),
      x = primitive_box(
        key = key_range_manual(start = c(2, 4), end = c(5, 7), name = c("A\nA", "B\nB"))
      ),
      x.sec = primitive_box(
        key = key_range_manual(
          start = c(2, 4, 3), end = c(5, 7, 6), level = c(1, 2, 3)
        )
      )
    )

  vdiffr::expect_doppelganger("primitive_box cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, donut = 0.5) +
    guides(
      r = primitive_box(),
      r.sec = primitive_box(
        key = key_range_auto(reverse = TRUE), drop_zero = FALSE
      ),
      theta = primitive_box(
        key = key_range_manual(start = c(2, 4), end = c(5, 7), name = c("A\nA", "B\nB"))
      ),
      theta.sec = primitive_box(
        key = key_range_manual(
          start = c(2, 4, 3), end = c(5, 7, 6),
          level = c(1, 2, 3)
        )
      )
    )

  vdiffr::expect_doppelganger("primitive_box radial", p)
})

test_that("primitive_box works as legend", {

  p <- ggplot(mtcars) +
    aes(
      x = disp, y = mpg,
      colour = hp,
      fill = hp
    ) +
    geom_point() +
    guides(
      colour = primitive_box(key = key_range_manual(
        c(100, 200), c(250, 300), c("A", "B")
      )),
      fill = primitive_box(key = key_range_manual(
        c(100, 150, 200), c(300, 300, 300), c("A", "B", "C")
      ), position = "bottom")
    )

  vdiffr::expect_doppelganger("primitive_box legend", p)

})
