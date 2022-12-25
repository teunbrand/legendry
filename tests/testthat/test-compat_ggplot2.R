
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
  tmp <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp)

  expect_error(justify_grobs(NULL), "Need individual grobs")
  expect_s3_class(justify_grobs(zeroGrob()), "zeroGrob")

  text <- element_render(theme_test(), "legend.text", label = "test")

  out <- justify_grobs(text, theme = theme_test(), debug = TRUE)
  expect_s3_class(out$children[[1]]$children[[1]], "rect")
  expect_s3_class(out$children[[1]]$children[[2]], "titleGrob")
  expect_s3_class(out$children[[2]], "points")

  unlink(tmp)
})

test_that("combine_elements combines elements", {
  test <- combine_elements(element_line(linewidth = 8), element_line(colour = "blue"))
  expect_equal(test$linewidth, 8)
  expect_equal(test$colour, "blue")
  expect_s3_class(test, "element_line")

  test <- combine_elements(element_line(inherit.blank = FALSE), element_blank())
  expect_s3_class(test, "element_line")

  test <- combine_elements(element_line(inherit.blank = TRUE), element_blank())
  expect_s3_class(test, "element_blank")

  test <- combine_elements(element_blank(), element_line(linewidth = 8))
  expect_s3_class(test, "element_blank")

  test <- combine_elements(element_line(linewidth = rel(0.5)), element_line(linewidth = 4))
  expect_equal(test$linewidth, 2)

  test <- combine_elements("foo", "bar")
  expect_equal(test, "foo")
})

test_that("arg_class messages appropriately", {
  el <- element_line()
  ans <- arg_class(el, c("element_line", "element_blank"))
  expect_identical(el, ans)

  ans <- tryCatch(
    arg_class(el, c("element_rect", "element_blank")),
    error = function(e) e$message
  )
  expect_snapshot(ans)

  expect_error(
    arg_class(expression(), "element_rect"),
    "not <expression>"
  )

})

test_that("rename_aes warns appropriately", {
  x <- list(x = 1, x = 2)
  expect_warning(rename_aes(x), "Duplicated aesthetics")
})

test_that("prtct_zlen returns NULL instead of zero-length vectors", {
  x <- prtct_zlen("A")
  expect_equal(x, "A")

  x <- prtct_zlen(character())
  expect_null(x)
})

test_that("find global can search ggplot2 namespace", {
  x <- find_global("geom_point", env = empty_env())()
  expect_s3_class(x, "LayerInstance")
})
