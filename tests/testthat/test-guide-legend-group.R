test_that("guide_legend_group works in both direction with all subtitles", {

  df <- msleep[c(9, 28, 11, 5, 34, 54, 24, 53), ]

  base <- ggplot(df, aes(bodywt, awake)) +
    geom_point(aes(colour = paste0(order, ".", name))) +
    scale_colour_discrete(
      name = "Animals",
      guide = guide_legend_group(ncol = 2)
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
      legendry.legend.subtitle = element_text(angle = 90, hjust = 1)
    )
  )

  vdiffr::expect_doppelganger(
    "bottom-righttitle",
    base + theme(
      legend.position = "bottom",
      legendry.legend.subtitle.position = "right",
      legendry.legend.subtitle = element_text(angle = 270)
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
