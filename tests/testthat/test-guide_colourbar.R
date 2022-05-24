test_that("All direction combinations work", {
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)

  leg <- guide_colourbar_vanilla()
  sc <- scale_colour_continuous()
  sc$train(c(0, 10))
  leg <- guide_train(leg, sc, "colour")
  leg$title <- "Title"
  leg$title.position <- "top"
  leg$direction <- "vertical"

  thm <- theme_test()
  thm$legend.key.width  <- thm$legend.key.size
  thm$legend.key.height <- thm$legend.key.size

  # Should error when incompatible direction / label.position
  leg$params$label.position <- "top"
  expect_snapshot_error(guide_gengrob(leg, thm))

  test_grid <- expand.grid(
    label.position = c("top", "bottom", "left", "right"),
    title.position = c("top", "bottom", "left", "right"),
    stringsAsFactors = FALSE
  )
  test_grid$direction <- ifelse(
    test_grid$label.position %in% c("top", "bottom"),
    yes = "horizontal", no = "vertical"
  )

  for (i in seq_row(test_grid)) {
    leg$params$label.position <- l <- test_grid$label.position[i]
    leg$title.position <- t <- test_grid$title.position[i]
    leg$direction <- d <- test_grid$direction[i]

    grob <- guide_gengrob(leg, thm)

    vdiffr::expect_doppelganger(
      paste0("L:", substr(l, 1, 1), " T:", substr(t, 1, 1),
             " D:", substr(d, 1, 1)),
      function() {grid.newpage(); grid.draw(grob)}
    )
  }

  unlink(tmp)
})

test_that("incorrect positions get rejected", {

  g <- guide_colourbar_vanilla(title.position = "top")
  expect_equal(g$title.position, "top")

  g <- tryCatch(
    guide_colourbar_vanilla(title.position = "bamboozled"),
    error = function(e) {e$message}
  )
  expect_snapshot(g)

  g <- guide_colourbar_vanilla(label.position = "right")
  expect_equal(g$params$label.position, "right")

  g <- tryCatch(
    guide_colourbar_vanilla(label.position = "bamboozled"),
    error = function(e) {e$message}
  )
  expect_snapshot(g)

  expect_error(
    construct_colourbar(label.position = "foobar")
  )
})
