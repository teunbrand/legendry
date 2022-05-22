test_that("guide_axis_trunc can be placed at every position", {

  p <- ggplot(mtcars, aes(mpg, disp)) +
    geom_blank() +
    guides(
      x = guide_axis_trunc(
        trunc_lower = 15,
        trunc_upper = NULL
      ),
      y = guide_axis_trunc(
        trunc_lower = 450,
        trunc_upper = NULL
      ),
      x.sec = guide_axis_trunc(
        trunc_lower = unit(0.1, "npc"),
        trunc_upper = unit(0.9, "npc")
      ),
      y.sec = guide_axis_trunc(
        trunc_lower = ~ min(.x) + 50,
        trunc_upper = function(x) {max(x) - 50}
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
