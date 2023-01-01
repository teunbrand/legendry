test_that("guide_axis_trunc can be placed at every position", {

  p <- ggplot(mtcars, aes(mpg, disp)) +
    geom_blank() +
    guides(
      x = guide_axis_cap(
        cap_lower = 15,
        cap_upper = NULL
      ),
      y = guide_axis_cap(
        cap_lower = 450,
        cap_upper = NULL
      ),
      x.sec = guide_axis_cap(
        cap_lower = unit(0.1, "npc"),
        cap_upper = unit(0.9, "npc")
      ),
      y.sec = guide_axis_cap(
        cap_lower = ~ min(.x) + 50,
        cap_upper = function(x) {max(x) - 50}
      )
    ) +
    theme_test() +
    theme(
      panel.border = element_blank(),
      axis.line = element_line()
    )

  vdiffr::expect_doppelganger(
    "all sides",
    p
  )
})

test_that("guide_axis_ext doesn't accept faulty truncation", {
  q <- quote(guide_axis_extend(cap_lower = 1:2, cap_upper = 1))
  expect_error(eval(q), "equal number")
})

test_that("truncations can be simplified", {
  trunc <- simplify_cap(NULL, "lower")
  expect_equal(unitType(trunc), "npc")
  expect_equal(as.numeric(trunc), 0)

  trunc <- simplify_cap(NULL, "upper")
  expect_equal(unitType(trunc), "npc")
  expect_equal(as.numeric(trunc), 1)

  trunc <- simplify_cap(~ .x + 1, "lower")
  expect_type(trunc, "closure")
  expect_equal(trunc(2), 3)
})
