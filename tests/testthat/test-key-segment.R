
test_that("key_segment_manual works as intended", {

  test <- key_segment_manual(
    c("A", "B"), 1L:2L, c("B",  "C"),
    3L:4L, linetype = 1L:2L
  )
  expect_s3_class(test, "key_segment")

  expect_identical(test$value, c("A", "B"))
  expect_identical(test$value_end, c("B", "C"))
  expect_identical(test$oppo, 1L:2L)
  expect_identical(test$oppo_end, 3L:4L)
  expect_identical(test$.linetype, 1L:2L)
})

test_that("key_segment_map works as intended", {

  data <- data_frame0(
    x = c("A", "B"), y = 1L:2L, xend = c("B", "C"), yend = 3L:4L
  )

  test <- key_segment_map(
    data, value = x, oppo = y, value_end = xend, oppo_end = yend,
    linetype = 1L:2L, color = "blue"
  )

  expect_identical(test$value, c("A", "B"))
  expect_identical(test$value_end, c("B", "C"))
  expect_identical(test$oppo, 1L:2L)
  expect_identical(test$oppo_end, 3L:4L)
  expect_identical(test$.linetype, 1L:2L)
  expect_identical(test$.colour, c("blue", "blue"))
})

test_that("key_dendro works as intended", {

  dummy <- scale_x_discrete()
  d <- hclust(dist(matrix(1L:9L, 3L)))
  ptype <- data.frame(
    value = double(), oppo = double(),
    value_end = double(), oppo_end = double()
  )

  test <- key_dendro(d, type = "rectangle")(dummy)
  expect_vector(test, ptype, size = 8L)

  test <- key_dendro(d, type = "triangle")(dummy)
  expect_vector(test, ptype, size = 4L)

})

test_that("segment_extract_key works as intended", {

  key <- key_segment_manual(
    value = 1.0, oppo = 0.0,
    value_end = I(0.9), oppo_end = 1.0
  )

  sc <- scale_x_continuous(limits = c(0.0, 10.0))
  test <- segment_extract_key(sc, "x", key)
  expect_equal(test$x, c(1.0, 9.0))

  sc <- scale_colour_gradient(limits = c(0.0, 10.0))
  test <- segment_extract_key(sc, "colour", key)
  expect_equal(test$.value, c(1.0, 9.0))

})
