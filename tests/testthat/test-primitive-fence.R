
test_that("primitive_fence works as axis", {

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
      y = primitive_fence(rail = "both"),
      y.sec = primitive_fence(
        rail = "none",
        key = key_range_auto(reverse = TRUE), drop_zero = FALSE
      ),
      x = primitive_fence(
        rail = "inner",
        key = key_range_manual(start = c(2, 4), end = c(5, 7), name = c("A\nA", "B\nB")),
        levels_post = list(NULL, element_line("red"))
      ),
      x.sec = primitive_fence(
        rail = "outer",
        key = key_range_manual(
          start = c(2, 4, 3), end = c(5, 7, 6), name = c("1", "2", "3"),
          level = c(1, 2, 3)
        )
      )
    )

  vdiffr::expect_doppelganger("primitive_fence cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    guides(
      r = primitive_fence(rail = "both"),
      r.sec = primitive_fence(
        angle = 0, rail = "none",
        key = key_range_auto(reverse = TRUE), drop_zero = FALSE
      ),
      theta = primitive_fence(
        rail = "inner",
        key = key_range_manual(start = c(2, 4), end = c(5, 7), name = c("A\nA", "B\nB")),
        levels_post = list(NULL, element_line("red"))
      ),
      theta.sec = primitive_fence(
        rail = "outer",
        key = key_range_manual(
          start = c(2, 4, 3), end = c(5, 7, 6), name = c("1", "2", "3"),
          level = c(1, 2, 3)
        )
      )
    )

  vdiffr::expect_doppelganger("primitive_fence radial", p)
})


test_that("primitive_bracket works as legend", {

  p <- ggplot(mtcars) +
    aes(
      x = disp, y = mpg,
      colour = hp,
      fill = hp
    ) +
    geom_point() +
    guides(
      colour = primitive_fence(key = key_range_manual(
        c(100, 200), c(250, 300), c("A", "B")
      ), rail = "outer"),
      fill = primitive_fence(key = key_range_manual(
        c(100, 150, 200), c(300, 300, 300), c("A", "B", "C")
      ), position = "bottom", rail = "inner")
    ) +
    theme(
      legend.box.just = "center"
    )

  vdiffr::expect_doppelganger("primitive_facet legend", p)

})
