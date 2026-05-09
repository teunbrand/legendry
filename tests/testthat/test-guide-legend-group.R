test_that("guide_legend_group can merge two legends", {

  df <- data.frame(x = 1L:3L, f = c("A,a", "A,b", "Z,x"))
  p <- ggplot(df, aes(x, x, color = f, shape = f)) +
    geom_point() +
    guides(
      color = "legend_group",
      shape = "legend_group"
    )
  gd <- get_guide_data(p, "color")
  expect_in(
    c("colour", "shape", ".group"),
    names(gd)
  )
  expect_identical(nrow(gd), 3L)
  expect_identical(gd$.group, factor(c("A", "A", "Z"), levels = c("A", "Z")))
})

test_that("guide_legend_group works in both direction with all subtitles", {

  df <- msleep[c(9L, 28L, 11L, 5L, 34L, 54L, 24L, 53L), ]

  base <- ggplot(df, aes(bodywt, awake)) +
    geom_point(aes(colour = paste0(order, ".", name))) +
    scale_colour_discrete(
      name = "Animals",
      guide = guide_legend_group(ncol = 2L)
    ) +
    theme_test() +
    theme(
      legend.key = element_rect(colour = NA, fill = "grey90"),
      legend.title.position = "top"
    )

  vdiffr::expect_doppelganger(
    "right-toptitle",
    base + theme(
      legend.position = "right",
      legendry.legend.subtitle.position = "top"
    )
  )

  vdiffr::expect_doppelganger(
    "right-lefttitle",
    base + theme(
      legend.position = "right",
      legendry.legend.subtitle.position = "left"
    )
  )

  vdiffr::expect_doppelganger(
    "right-righttitle",
    base + theme(
      legend.position = "right",
      legendry.legend.subtitle.position = "right"
    )
  )

  vdiffr::expect_doppelganger(
    "right-bottomtitle",
    base + theme(
      legend.position = "right",
      legendry.legend.subtitle.position = "bottom"
    )
  )

  vdiffr::expect_doppelganger(
    "bottom-toptitle",
    base + theme(
      legend.position = "bottom",
      legendry.legend.subtitle.position = "top"
    )
  )

  vdiffr::expect_doppelganger(
    "bottom-lefttitle",
    base + theme(
      legend.position = "bottom",
      legendry.legend.subtitle.position = "left",
      legendry.legend.subtitle = element_text(angle = 90.0, hjust = 1.0)
    )
  )

  vdiffr::expect_doppelganger(
    "bottom-righttitle",
    base + theme(
      legend.position = "bottom",
      legendry.legend.subtitle.position = "right",
      legendry.legend.subtitle = element_text(angle = 270.0)
    )
  )

  vdiffr::expect_doppelganger(
    "bottom-bottomtitle",
    base + theme(
      legend.position = "bottom",
      legendry.legend.subtitle.position = "bottom"
    )
  )
})
