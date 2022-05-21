
test_that("prioritise_labels returns correct number of elements", {
  expect_identical(prioritise_labels(0), numeric(0))
  expect_setequal(prioritise_labels(1), seq_len(1))
  expect_setequal(prioritise_labels(5), seq_len(5))
  expect_setequal(prioritise_labels(10), seq_len(10))
})

test_that("override_axis_label errors when angles are outside the range [0, 90]", {
  expect_s3_class(override_axis_label("top",    0),  "element")
  expect_s3_class(override_axis_label("bottom", 0),  "element")
  expect_s3_class(override_axis_label("left",   0),  "element")
  expect_s3_class(override_axis_label("right",  0),  "element")
  expect_s3_class(override_axis_label(NULL, NULL),   "element")
  expect_error(override_axis_label("bottom",  91),  "`angle` must")
  expect_error(override_axis_label("bottom", -91),   "`angle` must")
  expect_error(override_axis_label("bamboozled", 0), "Unrecognized position")
})

test_that("rotate_just sets correct just", {
  expect_equal(rotate_just(45, 0.25, 0.75),  list(hjust = 0.25, vjust = 0.75))
  expect_equal(rotate_just(135, 0.25, 0.75), list(hjust = 0.25, vjust = 0.25))
  expect_equal(rotate_just(225, 0.25, 0.75), list(hjust = 0.75, vjust = 0.25))
  expect_equal(rotate_just(315, 0.25, 0.75), list(hjust = 0.75, vjust = 0.75))
})

test_that("width_cm and height_cm error appropriately", {
  expect_error(width_cm(NULL),  "Cannot determine")
  expect_error(height_cm(NULL), "Cannot determine")
})

test_that("modify_list works", {
  old <- list(a = 1, b = 2)
  new <- list(b = 3, c = 4)
  expect_equal(
    modify_list(old, new),
    list(a = 1, b = 3, c = 4)
  )
})

test_that("justify_grobs signals problems", {
  expect_error(justify_grobs(NULL), "Need individual grobs")
  expect_s3_class(justify_grobs(zeroGrob()), "zeroGrob")

  text <- element_render(theme_test(), "legend.text", label = "test")

  out <- justify_grobs(text, theme = theme_test(), debug = TRUE)
  expect_s3_class(out$children[[1]]$children[[1]], "rect")
  expect_s3_class(out$children[[1]]$children[[2]], "titleGrob")
  expect_s3_class(out$children[[2]], "points")
})
