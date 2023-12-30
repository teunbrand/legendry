
test_that("guide_title works as axis", {

  base <- ggplot(mpg, aes(displ, hwy)) +
    geom_blank() +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank(),
      axis.line = element_line()
    )

  red_text <- theme(axis.title.y.right = element_text(colour = "red", vjust = 0.5))

  p <- base +
    guides(
      x     = guide_title(c("foo", "bar", "baz")),
      x.sec = guide_title("horizontal title"),
      y     = guide_title(c("qux", "egg"), angle = 45),
      y.sec = guide_title("foobar", theme = red_text)
    )

  vdiffr::expect_doppelganger("guide_title cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, donut = 0.5) +
    guides(
      theta.sec = guide_title(c("foo", "bar", "baz"), angle = 0),
      theta = guide_title("horizontal title"),
      r     = guide_title(c("qux", "egg"), angle = 90),
      r.sec = guide_title("foobar", theme = red_text)
    )

  vdiffr::expect_doppelganger("guide_title radial", p)
})
