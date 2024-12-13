
test_that("key_group_split works correctly", {

  scale <- scale_colour_hue()
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
  scale <- scale_colour_hue()
  scale$train(c("A", "C:D", "E:F"))

  test <- key_group_split(sep = ":")(scale, "colour")
  expect_equal(
    test[c(".label", ".group")],
    data.frame(.label = c("", "D", "F"), .group = factor(c("A", "C", "E")))
  )

  # Too many labels
  scale <- scale_colour_hue()
  scale$train(c("A:B", "C:D", "E:F:G"))

  test <- key_group_split(sep = ":")(scale, "colour")
  expect_equal(
    test[c(".label", ".group")],
    data.frame(.label = c("B", "D", "F G"), .group = factor(c("A", "C", "E")))
  )

  # Expression labels
  scale <- scale_colour_hue(labels = expression(A, B, C))
  scale$train(c("A", "B", "C"))
  expect_snapshot(
    key_group_split(sep = ":")(scale, "colour"),
    error = TRUE
  )
})

test_that("key_group_lut works as intended", {

  levels <- c("Coffee", "Tea", "Soda", "Water")
  groups <- rep(c("Hot drinks", "Cold drinks"), each = 2)

  sc <- scale_colour_hue()
  sc$train(levels)
  sc$train("Car")

  key <- key_group_lut(levels,  groups)
  test <- key(sc, "colour")

  expect_equal(test$.label, c(levels, "Car"))
  expect_equal(test$.group, factor(c(groups, "Other"), unique(c(groups, "Other"))))

  # Mismatched lengths
  levels <- c("A", "B")
  groups <- c("X", "X", "Y")
  expect_error(
    key_group_lut(levels, groups),
    "must have the same length"
  )
})
