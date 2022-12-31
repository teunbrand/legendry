test_that("All directions and justifications work", {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)

  leg <- guide_colour_violin(mpg$displ, frame = element_rect(colour = "black"))
  sc  <- scale_colour_continuous()
  sc$train(range(mpg$displ))

  guides <- ggplot2:::guides_list(list(colour = leg))
  guides <- guides$setup(scales = list(sc))
  guides$train(list(sc), "vertical", list(colour = "Title"))

  params <- guides$params[[1]]
  params$title.position <- "top"

  thm <- theme_test()
  thm$legend.key.width <- thm$legend.key.height <- thm$legend.key.size

  test_grid <- expand.grid(
    direction = c("horizontal", "vertical"),
    just = c(0, 0.5, 1),
    stringsAsFactors = FALSE
  )

  for (i in seq_row(test_grid)) {
    params$direction <- d <- test_grid$direction[i]
    params$just <- j <- test_grid$just[i]
    params$label.position <- if (params$direction == "horizontal") "bottom" else "right"
    grob <- leg$draw(thm, params)
    vdiffr::expect_doppelganger(
      paste0("D:", substr(d, 1, 1), " J:", format(j, nsmall = 1)),
      function() {grid.newpage(); grid.draw(grob)}
    )
  }
})
