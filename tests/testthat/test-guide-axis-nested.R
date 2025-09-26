test_that("guide_axis_nested logic works", {

  g <- guide_axis_nested()
  expect_length(g$params$guides, 3L)
  expect_s3_class(g$params$guides[[1]], "PrimitiveLine")
  expect_s3_class(g$params$guides[[2]], "PrimitiveTicks")

  g <- guide_axis_nested(key = "range_auto")
  expect_length(g$params$guides, 3L)

  g <- guide_axis_nested(key = key_range_manual(1, 2, "A"))
  expect_length(g$params$guides, 4L)
  expect_s3_class(g$params$guides[[3]], "PrimitiveLabels")


  g <- guide_axis_nested(type = "bracket")
  expect_s3_class(g$params$guides[[3]], "PrimitiveBracket")

  g <- guide_axis_nested(type = "box")
  expect_s3_class(g$params$guides[[3]], "PrimitiveBox")

  g <- guide_axis_nested(subtitle = "foobar")
  expect_length(g$params$guides, 4L)
  expect_s3_class(g$params$guides[[4]], "PrimitiveTitle")
})

test_that("guide_axis_nested recognised `key_range_auto()`", {

  guide <- guide_axis_nested(key = "range_auto")
  expect_length(guide$params$guides, 3L)

  guide <- guide_axis_nested(key = key_range_auto(sep = "foobar"))
  expect_length(guide$params$guides, 3L)

  guide <- guide_axis_nested(key = key_range_manual(1, 2))
  expect_length(guide$params$guides, 4L)
  expect_s3_class(guide$params$guides[[3]], "PrimitiveLabels")

})

# Visual test -------------------------------------------------------------

test_that("guide_axis_nested looks good as axis", {

  base <- ggplot(mpg, aes(interaction(cyl, drv), hwy)) +
    theme_test() +
    theme(
      panel.background = element_rect(fill = NA, colour = "grey80"),
      panel.grid.major = element_line(colour = "grey90"),
      panel.border = element_blank()
    )

  p <- base + guides(
    x = guide_axis_nested(),
    y = guide_axis_nested(
      key = key_range_manual(18, 32, "Foo"),
      type = "box"
    ),
    y.sec = guide_axis_nested(
      key = key_range_manual(28, 42, "Bar"),
      regular_key = key_manual(c(20, 30))
    ),
    x.sec = guide_axis_nested(type = "box", subtitle = "subtitle")
  )

  vdiffr::expect_doppelganger("guide_axis_base cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    guides(
      theta = guide_axis_nested(),
      r = guide_axis_nested(
        key = key_range_manual(18, 32, "Foo"),
        type = "box"
      ),
      r.sec = guide_axis_nested(
        key = key_range_manual(28, 42, "Bar"),
        regular_key = key_manual(c(20, 30))
      ),
      theta.sec = guide_axis_nested(type = "box")
  )

  vdiffr::expect_doppelganger("guide_axis_base radial", p)
})
