test_that("guide_axis_nested logic works", {

  g <- guide_axis_nested()
  expect_length(g$params$guides, 3L)
  expect_s3_class(g$params$guides[[1L]], "PrimitiveLine")
  expect_s3_class(g$params$guides[[2L]], "PrimitiveTicks")

  g <- guide_axis_nested(key = "range_auto")
  expect_length(g$params$guides, 3L)

  g <- guide_axis_nested(key = key_range_manual(1.0, 2.0, "A"))
  expect_length(g$params$guides, 4L)
  expect_s3_class(g$params$guides[[3L]], "PrimitiveLabels")


  g <- guide_axis_nested(type = "bracket")
  expect_s3_class(g$params$guides[[3L]], "PrimitiveBracket")

  g <- guide_axis_nested(type = "box")
  expect_s3_class(g$params$guides[[3L]], "PrimitiveBox")

  g <- guide_axis_nested(subtitle = "foobar")
  expect_length(g$params$guides, 4L)
  expect_s3_class(g$params$guides[[4L]], "PrimitiveTitle")
})

test_that("guide_axis_nested recognised `key_range_auto()`", {

  guide <- guide_axis_nested(key = "range_auto")
  expect_length(guide$params$guides, 3L)

  guide <- guide_axis_nested(key = key_range_auto(sep = "foobar"))
  expect_length(guide$params$guides, 3L)

  guide <- guide_axis_nested(key = key_range_manual(1.0, 2.0))
  expect_length(guide$params$guides, 4L)
  expect_s3_class(guide$params$guides[[3L]], "PrimitiveLabels")

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
      key = key_range_manual(18.0, 32.0, "Foo"),
      type = "box"
    ),
    y.sec = guide_axis_nested(
      key = key_range_manual(28.0, 42.0, "Bar"),
      regular_key = key_manual(c(20.0, 30.0))
    ),
    x.sec = guide_axis_nested(type = "box", subtitle = "subtitle")
  )

  vdiffr::expect_doppelganger("guide_axis_base cartesian", p)

  p <- base +
    coord_radial(start = 0.25 * pi, end = 1.75 * pi, inner.radius = 0.5) +
    guides(
      theta = guide_axis_nested(),
      r = guide_axis_nested(
        key = key_range_manual(18.0, 32.0, "Foo"),
        type = "box"
      ),
      r.sec = guide_axis_nested(
        key = key_range_manual(28.0, 42.0, "Bar"),
        regular_key = key_manual(c(20.0, 30.0))
      ),
      theta.sec = guide_axis_nested(type = "box")
    )

  vdiffr::expect_doppelganger("guide_axis_base radial", p)
})
