
test_that("primitive_bracket works as axis", {

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
      y = primitive_bracket(bracket = "curvy"),
      y.sec = primitive_bracket(
        bracket = "round",
        key = key_range_auto(reverse = TRUE),
        drop_zero = FALSE
      ),
      x = primitive_bracket(
        bracket = "square",
        key = key_range_manual(
          start = c(2L, 4L),
          end   = c(5L, 7L),
          name  = c("A\nA", "B\nB")
        )
      ),
      x.sec = primitive_bracket(
        bracket = "chevron",
        key = key_range_manual(
          start = c(2.0, 4.0, 3.0),
          end   = c(5.0, 7.0, 6.0),
          name  = c("1", "2", "3"),
          level = c(1L, 2L, 3L),
          line_color = c("tomato", "dodgerblue", 'limegreen')
        )
      )
    )

  vdiffr::expect_doppelganger("primitive_bracket cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    guides(
      r = primitive_bracket(bracket = "curvy"),
      r.sec = primitive_bracket(
        bracket = "round",
        angle = 0.0,
        key = key_range_auto(reverse = TRUE),
        drop_zero = FALSE
      ),
      theta = primitive_bracket(
        bracket = "chevron",
        key = key_range_manual(
          start = c(2.0, 4.0),
          end = c(5.0, 7.0),
          name = c("A\nA", "B\nB")
        )
      ),
      theta.sec = primitive_bracket(
        bracket = "square",
        key = key_range_manual(
          start = c(2.0, 4.0, 3.0),
          end = c(5.0, 7.0, 6.0),
          name = c("1", "2", "3"),
          level = c(1L, 2L, 3L),
          line_color = c("tomato", "dodgerblue", 'limegreen')
        )
      )
    )

  vdiffr::expect_doppelganger("primitive_bracket radial", p)
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
      colour = primitive_bracket(key = key_range_manual(
        c(100.0, 200.0), c(250.0, 300.0), c("A", "B")
      )),
      fill = primitive_bracket(key = key_range_manual(
        c(100.0, 150.0, 200.0), c(300.0, 300.0, 300.0), c("A", "B", "C")
      ), position = "bottom")
    ) +
    theme(
      legend.box.just = "center"
    )

  vdiffr::expect_doppelganger("primitive_bracket legend", p)

})
