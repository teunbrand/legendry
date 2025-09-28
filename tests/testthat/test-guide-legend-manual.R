test_that("guide_legend_manual() constructor works as expected", {
  # Empty key results in none guide
  g <- guide_legend_manual(NULL)
  expect_s3_class(g, "GuideNone")

  # Singular layers are expanded to list
  g <- guide_legend_manual("foo", layers = geom_point())
  expect_type(g$params$layers, "list")
  expect_length(g$params$layers, 1L)
  expect_s3_class(g$params$layers[[1]], "LayerInstance")

  # The reverse legend setting is applied to the key
  g <- guide_legend_manual(c("foo", "bar"), legend_args = list(reverse = TRUE))
  expect_equal(
    g$params$legend$key$.label,
    c("bar", "foo")
  )

  # Non-layers are rejected
  expect_snapshot(
    guide_legend_manual("foo", layers = list("invalid input")),
    error = TRUE
  )

  # Recycling rules are applied
  expect_snapshot(
    guide_legend_manual(c("foo", "bar"), colour = c("red", "green", "blue")),
    error = TRUE
  )
})

test_that("guide_legend_manual() can render a legend", {

  p <- ggplot() +
    guides(whatever = guide_legend_manual(
      title = "Foobar",
      label = c("foo", "bar"),
      colour = c("tomato", "dodgerblue"),
      fill = NA,
      layers = list(
        geom_point(shape = c(21, 19)),
        geom_col(colour = c("dodgerblue", 'tomato'))
      ),
      legend_args = list(ncol = 2)
    ))

  vdiffr::expect_doppelganger("manual legend", p)
})

