

test_that("key_group_auto works as intended", {

  levels <- c("Soda", "Water", "Coffee", "Tea")
  groups <- rep(c("Cold drinks", "Hot drinks"), each = 2)
  compound <- paste(groups, levels, sep = "-")

  sc <- scale_color_discrete()
  sc$train(compound)

  key <- key_group_auto(sep = "-")
  test <- key(sc, "colour")

  expect_equal(test$.label, levels)
  expect_equal(test$.group, factor(groups))

  key <- key_group_auto(sep = "-", reverse = TRUE)
  test <- key(sc, "colour")

  expect_equal(test$.group, factor(levels, levels))
  expect_equal(test$.label, groups)

})

test_that("key_group_auto rejects expressions", {

  sc <- scale_color_discrete(labels = expression(A^2, 10^B))
  sc$train(c("A", "B"))

  key <- key_group_auto()
  expect_error(
    key(sc, "colour"),
    "Cannot split"
  )
})

test_that("key_group_lut works as intended", {

  levels <- c("Coffee", "Tea", "Soda", "Water")
  groups <- rep(c("Hot drinks", "Cold drinks"), each = 2)

  sc <- scale_colour_discrete()
  sc$train(levels)
  sc$train("Car")

  key <- key_group_lut(levels, groups)
  test <- key(sc, "colour")

  expect_equal(test$.label, c(levels, "Car"))
  expect_equal(test$.group, factor(c(groups, "Other"), unique(c(groups, "Other"))))

})

test_that("key_group_lut cannot deal with mismatching luts", {

  levels <- c("A", "B")
  groups <- c("X", "X", "Y")

  expect_error(
    key_group_lut(levels, groups),
    "must have the same length"
  )
})
