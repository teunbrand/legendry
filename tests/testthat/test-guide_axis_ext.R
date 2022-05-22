test_that("guide_axis_ext can be placed at every position", {

  p <- ggplot(mtcars, aes(mpg, disp)) +
    geom_blank() +
    guides(
      x = guide_axis_ext(subtitle = "miles per gallon"),
      y = guide_axis_ext(subtitle = "engine displacement", colour = "red",
                         subtitle.theme = element_text(colour = "blue")),
      x.sec = guide_axis_ext(minor_size = 0.8, subtitle = "Don't show me",
                             subtitle.theme = element_blank()),
      y.sec = guide_axis_ext(trunc_lower = min, trunc_upper = max,
                             title = "truncated axis")
    ) +
    theme_test() +
    theme(
      panel.border = element_blank(),
      axis.line = element_line()
    )

  vdiffr::expect_doppelganger(
    "guide_axis_ext all sides",
    p
  )
})

test_that("guide_axis_ext doesn't accept faulty truncation", {
  q <- quote(guide_axis_ext(trunc_lower = 1:2, trunc_upper = 1))
  expect_error(eval(q), "equal number")
})

test_that("truncations can be simplified", {
  trunc <- simplify_trunc(NULL, "lower")
  expect_equal(unitType(trunc), "npc")
  expect_equal(as.numeric(trunc), 0)

  trunc <- simplify_trunc(NULL, "upper")
  expect_equal(unitType(trunc), "npc")
  expect_equal(as.numeric(trunc), 1)

  trunc <- simplify_trunc(~ .x + 1, "lower")
  expect_type(trunc, "closure")
  expect_equal(trunc(2), 3)
})
