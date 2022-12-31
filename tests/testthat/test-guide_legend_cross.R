test_that("legend cross labels can be placed anywhere", {

  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)

  leg <- guide_legend_cross(
    h_label.theme = element_text(colour = "red",  vjust = 0.5),
    v_label.theme = element_text(colour = "blue", vjust = 0.5)
  )

  sc <- scale_colour_discrete()
  sc$train(c("A:a", "B:a", "C:a", "A:b", "C:b"))

  guides <- ggplot2:::guides_list(list(colour = leg))
  guides <- guides$setup(list(sc))
  guides$train(list(sc), "vertical", list(colour = "Title"))

  lay <- geom_point(aes(colour = c("A:a", "B:a", "C:a", "A:b", "C:b")))
  lay$computed_mapping <- lay$mapping

  guides$process_layers(list(lay), NULL)

  params <- guides$get_params("colour")
  params$title.position <- "top"
  guide  <- guides$get_guide("colour")

  test_grid <- expand.grid(
    h_label.position = c("left", "right"),
    v_label.position = c("top", "bottom"),
    stringsAsFactors = FALSE
  )

  theme <- theme_test() + theme(
    legend.key = element_rect(colour = "black")
  )

  for (i in seq_row(test_grid)) {

    params$h_label.position <- hl <- test_grid$h_label.position[i]
    params$v_label.position <- vl <- test_grid$v_label.position[i]

    grob <- guide$draw(theme, params)

    vdiffr::expect_doppelganger(
      paste0("HL:", substr(hl, 1, 1), " VL:", substr(vl, 1, 1)),
      function() {grid.newpage(); grid.draw(grob)}
    )
  }
})

test_that("splitting labels returns appropriate errors", {

  test <- c("A:B", "C:D", "E:F")
  test <- split_labels(test, ":", fun_nm = "fun")
  expect_equal(
    test,
    list(c("A", "C", "E"), c("B", "D", "F"))
  )

  test <- c("A", "C:D", "E:F")
  expect_warning(
    test <- split_labels(test, ":", fun_nm = "fun"),
    "They have been padded"
  )
  expect_equal(
    test,
    list(c("A", "C", "E"), c("", "D", "F"))
  )

  test <- c("A:B", "C:D", "E:F:G")
  expect_warning(
    test <- split_labels(test, ":", fun_nm = "fun"),
    "Only the first 2 were taken"
  )
  expect_equal(
    test,
    list(c("A", "C", "E"), c("B", "D", "F"))
  )
})

test_that("guides can be merged", {
  guide <- guide_legend_cross(NULL, title = "My Title")
  p <- ggplot(mtcars, aes(mpg, disp)) +
    geom_point(aes(colour = factor(cyl), shape = factor(vs), fill = factor(vs))) +
    scale_colour_manual(values = c("red", "green", "blue")) +
    scale_shape_manual(values = c(21, 22)) +
    scale_fill_manual(values = c("purple", "orange")) +
    guides(colour = guide, shape = guide, fill = guide) +
    theme_test()

  vdiffr::expect_doppelganger(
    "merged guide",
    p
  )
})

test_that("invalid order will error", {
  expect_error(
    guide_legend_cross(label_order = c("row", "row")),
    "should be either"
  )
})

