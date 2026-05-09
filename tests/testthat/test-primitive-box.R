
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
        min_size = 1L,
        key = key_range_auto(reverse = TRUE),
        drop_zero = FALSE
      ),
      x = primitive_box(
        key = key_range_manual(
          start = c(2.0, 4.0),
          end = c(5.0, 7.0),
          name = c("A\nA", "B\nB")
        )
      ),
      x.sec = primitive_box(
        key = key_range_manual(
          start = c(2.0, 4.0, 3.0),
          end = c(5.0, 7.0, 6.0),
          level = c(1L, 2L, 3L)
        )
      )
    )

  vdiffr::expect_doppelganger("primitive_box cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    guides(
      r = primitive_box(),
      r.sec = primitive_box(
        key = key_range_auto(reverse = TRUE), drop_zero = FALSE
      ),
      theta = primitive_box(
        key = key_range_manual(
          start = c(2.0, 4.0),
          end = c(5.0, 7.0),
          name = c("A\nA", "B\nB")
        )
      ),
      theta.sec = primitive_box(
        key = key_range_manual(
          start = c(2.0, 4.0, 3.0), end = c(5.0, 7.0, 6.0),
          level = c(1L, 2L, 3L)
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
        c(100.0, 200.0), c(250.0, 300.0), c("A", "B")
      )),
      fill = primitive_box(key = key_range_manual(
        c(100.0, 150.0, 200.0), c(300.0, 300.0, 300.0), c("A", "B", "C")
      ), position = "bottom")
    )

  vdiffr::expect_doppelganger("primitive_box legend", p)

})

test_that("draw_box can draw theta boxes", {
  box <- draw_box(
    data.frame(x = c(0.0, 1.0), y = c(0.0, 1.0), group = 1L, theta = 0.0),
    element = theme_gray()$rect,
    position = "theta",
    size = 0.1, offset = 1.0
  )
  expect_s3_class(box, "polygon")
})
