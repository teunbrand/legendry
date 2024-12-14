test_that("we can easily turn on/off elements in `guide_axis_dendro()`", {

  g <- guide_axis_dendro(labels = TRUE, ticks = TRUE, axis_line = TRUE)
  inner_guides <- g$params$guides
  expect_s3_class(inner_guides[[1]], "PrimitiveLine")
  expect_s3_class(inner_guides[[2]], "PrimitiveTicks")
  expect_s3_class(inner_guides[[3]], "PrimitiveLabels")
  expect_s3_class(inner_guides[[4]], "PrimitiveSegments")

  g <- guide_axis_dendro(labels = FALSE, ticks = FALSE, axis_line = FALSE)
  inner_guides <- g$params$guides
  expect_s3_class(inner_guides[[1]], "GuideNone")
  expect_s3_class(inner_guides[[2]], "GuideNone")
  expect_s3_class(inner_guides[[3]], "GuideNone")
  expect_s3_class(inner_guides[[4]], "PrimitiveSegments")

})
