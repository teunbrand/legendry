test_that("Legends without breaks return NULL upon training", {
  leg <- guide_legend_vanilla(ncol = 2)
  sc <- scale_colour_discrete(breaks = NULL)
  sc$train(c("Aa", "Bb", "Cc", "Dd"))

  expect_null(guide_train(leg, sc))
})

test_that("Legends with geom mismatch return NULL", {
  leg <- guide_legend_vanilla(ncol = 2)

  sc <- scale_colour_discrete()
  sc$train(c("Aa", "Bb", "Cc", "Dd"))

  leg <- guide_train(leg, sc, "colour")
  leg$title <- "Title"
  leg$direction <- "vertical"

  lay <- geom_point(aes(colour = c("Aa", "Bb", "Cc", "Dd")))
  lay$computed_mapping <- lay$mapping

  expect_s3_class(guide_geom(leg, list(lay), NULL), "GuideLegend")

  lay$show.legend <- FALSE

  expect_null(guide_geom(leg, list(lay), NULL), "GuideLegend")
})

test_that("All legend directions work", {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)

  leg <- guide_legend_vanilla(ncol = 2)

  sc <- scale_colour_discrete()
  sc$train(c("Aa", "Bb", "Cc", "Dd"))

  leg <- guide_train(leg, sc, "colour")
  leg$title <- "Title"
  leg$direction <- "vertical"

  lay <- geom_point(aes(colour = c("Aa", "Bb", "Cc", "Dd")))
  lay$computed_mapping <- lay$mapping


  leg <- guide_geom(leg, list(lay), NULL)

  theme <- theme_test() + theme(
    legend.key = element_rect(colour = "black"),
    legend.spacing.x = unit(0.25, "cm"),
    legend.spacing.y = unit(0.50, "cm")
  )

  test_grid <- expand.grid(
    label.position = c("top", "bottom", "left", "right"),
    title.position = c("top", "bottom", "left", "right"),
    stringsAsFactors = FALSE
  )

  for (i in seq_row(test_grid)) {
    leg$params$label.position <- l <- test_grid$label.position[i]
    leg$title.position <- t <- test_grid$title.position[i]

    grob <- guide_gengrob(leg, theme)

    vdiffr::expect_doppelganger(
      paste0("L:", substr(l, 1, 1), " T:", substr(t, 1, 1)),
      function() {grid.newpage(); grid.draw(grob)}
    )

  }

  unlink(tmp)
})

test_that("Legend layer inclusion works", {

  lay <- geom_point()
  lay$computed_mapping <- lay$mapping

  include_layer_in_guide(lay, "colour")

  lay$show.legend <- character()

  expect_warning(
    expect_false(
      include_layer_in_guide(lay, "colour")
    )
  )

  lay$show.legend <- logical()

  expect_true(include_layer_in_guide(lay, "colour"))

  lay$show.legend <- c("colour" = FALSE, "fill" = TRUE)

  expect_false(include_layer_in_guide(lay, "colour"))
  expect_true(include_layer_in_guide(lay, "fill"))

  lay$show.legend <- TRUE
  expect_true(include_layer_in_guide(lay, character()))
})



test_that("Legends don't accept invalid positions", {

  q <- quote(guide_legend_vanilla(title.position = "inside-out"))
  expect_error(eval(q))

  q <- quote(guide_legend_vanilla(label.position = c("upside-down")))
  expect_error(eval(q))

})
