
test_that("guide_axis_nested can be placed at all sides", {

  p <- ggplot(mpg, aes(interaction(cyl, year), class)) +
    geom_bin_2d() +
    guides(
      fill = "none",
      x = guide_axis_nested(
        bracket = "curvy",
        bracket_size = unit(0.5, "cm"),
        bracket_theme = element_line(colour = "dodgerblue")
      ),
      x.sec = guide_axis_nested(
        colour = "forestgreen",
        bracket = "round"
      ),
      y = guide_axis_nested(
        range_start = c(0.5, 3.5),
        range_end   = c(4.5, 6.5),
        range_name = c("A", "B"),
        bracket = "square",
        deep_text = elements_text(colour = c("tomato", "gold"), face = "bold")
      ),
      y.sec = guide_axis_nested(
        range_start = c("2seater", "midsize"),
        range_end   = c("minivan", 'suv'),
        range_name  = c("C", "D"),
        range_depth = 1,
        bracket = "atan"
      )
    )

  vdiffr::expect_doppelganger("all_sides", p)
})
