test_that("we can easily turn on or off elements in `guide_axis_dendro()`", {

  g <- guide_axis_dendro(labels = TRUE, ticks = TRUE, axis_line = TRUE)
  inner_guides <- g$params$guides
  expect_s3_class(inner_guides[[1L]], "PrimitiveLine")
  expect_s3_class(inner_guides[[2L]], "PrimitiveTicks")
  expect_s3_class(inner_guides[[3L]], "PrimitiveLabels")
  expect_s3_class(inner_guides[[4L]], "PrimitiveSegments")

  g <- guide_axis_dendro(labels = FALSE, ticks = FALSE, axis_line = FALSE)
  inner_guides <- g$params$guides
  expect_s3_class(inner_guides[[1L]], "GuideNone")
  expect_s3_class(inner_guides[[2L]], "GuideNone")
  expect_s3_class(inner_guides[[3L]], "GuideNone")
  expect_s3_class(inner_guides[[4L]], "PrimitiveSegments")

})
