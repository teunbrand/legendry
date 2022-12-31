test_that("All directions and cap shapes work", {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)

  leg <- guide_colourbar_cap(nbin = 3)
  sc  <- scale_colour_continuous()
  sc$train(c(0, 10))

  guides <- ggplot2:::guides_list(list(leg))
  guides <- guides$setup(scales = list(sc))
  guides$train(list(sc), "vertical", list(colour = "Title"))


  params <- guides$params[[1]]
  params$title.position <- "top"

  thm <- theme_test()
  thm$legend.key.width <- thm$legend.key.height <- thm$legend.key.size

  test_grid <- expand.grid(
    direction = c("horizontal", "vertical"),
    shape     = c("triangle", "round", "arched"),
    stringsAsFactors = FALSE
  )
  test_grid$ends <- rep(c("lower", "upper", "both"))

  for (i in seq_row(test_grid)) {
    params$direction <- d <- test_grid$direction[i]
    params$cap_shape <- s <- test_grid$shape[i]
    params$cap_position <- list(
      "lower" = test_grid$ends[i] %in% c("lower", "both"),
      "upper" = test_grid$ends[i] %in% c("upper", "both")
    )
    params$label.position <- if (params$direction == "horizontal") "bottom" else "right"
    p <- test_grid$ends[i]
    grob <- leg$draw(thm, params)

    vdiffr::expect_doppelganger(
      paste0("D:", substr(d, 1, 1), " S:", substr(s, 1, 1),
             " P:", substr(p, 1, 1)),
      function() {grid.newpage(); grid.draw(grob)}
    )
  }
})
