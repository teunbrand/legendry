
test_that("key_group_split works correctly", {

  scale <- scale_colour_discrete()
  scale$train(c("A:B", "C:D", "E:F"))

  # Standard case
  test <- key_group_split(sep = ":")(scale, "colour")
  expect_equal(
    test[c(".label", ".group")],
    data.frame(.label = c("B", "D", "F"), .group = factor(c("A", "C", "E")))
  )

  # Test reverse argument
  test <- key_group_split(sep = ":", reverse = TRUE)(scale, "colour")
  expect_equal(
    test[c(".label", ".group")],
    data.frame(.label = c("A", "C", "E"), .group = factor(c("B", "D", "F")))
  )

  # Missing label
  scale <- scale_colour_discrete()
  scale$train(c("A", "C:D", "E:F"))

  test <- key_group_split(sep = ":")(scale, "colour")
  expect_equal(
    test[c(".label", ".group")],
    data.frame(.label = c("", "D", "F"), .group = factor(c("A", "C", "E")))
  )

  # Too many labels
  scale <- scale_colour_discrete()
  scale$train(c("A:B", "C:D", "E:F:G"))

  test <- key_group_split(sep = ":")(scale, "colour")
  expect_equal(
    test[c(".label", ".group")],
    data.frame(.label = c("B", "D", "F G"), .group = factor(c("A", "C", "E")))
  )
})
