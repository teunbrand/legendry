test_that("All directions and justifications work", {

  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)

  leg <- guide_colour_histogram(hist(mpg$displ, ), frame = element_rect(colour = "black", fill = NA))
  sc  <- scale_colour_continuous()
  sc$train(range(mpg$displ))

  guides <- guides(colour = leg)
  guides <- guides$setup(scales = list(sc))
  guides$train(list(sc), "vertical", list(colour = "Title"))

  params <- guides$params[[1]]
  params$title.position <- "top"

  thm <- theme_test()
  thm$legend.key.width <- thm$legend.key.height <- thm$legend.key.size

  direction <- c("horizontal", "vertical")
  just <- c(0, 0.5, 1)

  test_grid <- expand.grid(direction = direction, just = just,
                           stringsAsFactors = FALSE)

  gt <-  gtable(
    widths  = unit(rep(1, length(direction)), "null"),
    heights = unit(rep(1, length(just)), "null")
  )

  for (i in seq_row(test_grid)) {
    params$direction <- d <- test_grid$direction[i]
    params$just <- j <- test_grid$just[i]
    params$title <- paste0("direction = ", d, "\njust = ", j)
    d <- match(d, direction)
    j <- match(j, just)
    params$label.position <- if (params$direction == "horizontal") "bottom" else "right"
    grob <- leg$draw(thm, params)

    gt <- gtable_add_grob(gt, grobs = grob, t = j, l = d)
  }

  vdiffr::expect_doppelganger(
    "histogram guides",
    fig = function() {grid.newpage(); grid.draw(gt)}
  )
})
