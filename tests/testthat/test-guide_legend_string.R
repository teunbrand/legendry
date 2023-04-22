

test_that("legend string can be placed anywhere", {

  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)

  leg <- guide_legend_string(face = "bold", ncol = 2)

  sc <- scale_colour_discrete()
  sc$train(mpg$class)

  guides <- guides(colour = leg)
  guides <- guides$setup(list(sc))
  guides$train(list(sc), "vertical", list(colour = "Title"))

  params <- guides$get_params("colour")
  guide  <- guides$get_guide("colour")

  test_grid <- expand.grid(
    title.position = c("top", "bottom", "left", "right"),
    byrow = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  theme <- theme_test() +
    theme(legend.background = element_rect(fill = NA, colour = "black"))

  grobs <- lapply(seq_row(test_grid), function(i) {
    params$title.position <- test_grid$title.position[i]
    params$byrow <- test_grid$byrow[i]
    guide$draw(theme, params)
  })

  widths  <- matrix(width_cm(grobs), 2, 4)
  heights <- matrix(height_cm(grobs), 2, 4)

  widths <- apply(widths, 2, max)
  heights <- apply(heights, 1, max)

  i <- matrix(seq_along(grobs), 2, 4)

  gt <- gtable(widths = unit(widths, "cm"), heights = unit(heights, "cm"))
  gt <- gtable_add_grob(
    gt, grobs, t = as.vector(row(i)), l = as.vector(col(i))
  )
  gt <- gtable::gtable_add_row_space(gt, unit(0.5, "cm"))
  gt <- gtable::gtable_add_col_space(gt, unit(0.5, "cm"))

  vdiffr::expect_doppelganger(
    "guide_stringlegend",
    function() {grid.newpage(); grid.draw(gt)}
  )
})
