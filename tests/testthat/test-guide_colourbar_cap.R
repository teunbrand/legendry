test_that("All directions and cap shapes work", {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)

  leg <- guide_colourbar_cap(nbins = 3)
  sc  <- scale_colour_continuous()
  sc$train(c(0, 10))

  leg <- guide_train(leg, sc, "colour")
  leg$title <- "Title"
  leg$title.position <- "top"
  leg$direction <- "vertical"

  thm <- theme_test()
  thm$legend.key.width <- thm$legend.key.height <- thm$legend.key.size

  test_grid <- expand.grid(
    direction = c("horizontal", "vertical"),
    shape     = c("triangle", "round", "arched"),
    stringsAsFactors = FALSE
  )
  test_grid$ends <- rep(c("lower", "upper", "both"))

  for (i in seq_row(test_grid)) {
    leg$direction <- d <- test_grid$direction[i]
    leg$params$cap_shape <- s <- test_grid$shape[i]
    leg$params$cap_position <- list(
      "lower" = test_grid$ends[i] %in% c("lower", "both"),
      "upper" = test_grid$ends[i] %in% c("upper", "both")
    )
    p <- test_grid$ends[i]
    grob <- guide_gengrob(leg, thm)

    vdiffr::expect_doppelganger(
      paste0("D:", substr(d, 1, 1), " S:", substr(s, 1, 1),
             " P:", substr(p, 1, 1)),
      function() {grid.newpage(); grid.draw(grob)}
    )
  }
})
