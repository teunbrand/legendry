test_that("guide_axis_table can be placed at all sides", {

  df <- expand.grid(letters[1:4], LETTERS[1:3])

  xdf <- data.frame(lab = letters[1:4], num = 1:4,
                    nm = c("foo", "bar", "baz", "qux"))
  ydf <- data.frame(lab = LETTERS[1:3], num = 100:102,
                    nm = c("ham", "spam", "green eggs"))


  p <- ggplot(df, aes(Var1, Var2)) +
    geom_point() +
    scale_x_discrete(expand = c(0, 1, 0, 2)) +
    scale_y_discrete(expand = c(0, 2, 0, 1)) +
    guides(
      x = guide_axis_table(xdf, lab),
      y = guide_axis_table(ydf, lab, uniform_size = TRUE),
      x.sec = guide_axis_table(xdf, lab, align_panel = FALSE),
      y.sec = guide_axis_table(ydf, lab, align_panel = FALSE)
    ) +
    theme_test()

  vdiffr::expect_doppelganger("all_sides", p)
})

