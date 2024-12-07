
test_that("key_segment_manual works as intended", {

  test <- key_segment_manual(c("A", "B"), 1:2, c("B",  "C"), 3:4, linetype = 1:2)
  expect_s3_class(test, 'key_segment')

  expect_equal(test$value, c("A", "B"))
  expect_equal(test$value_end, c("B", "C"))
  expect_equal(test$oppo, 1:2)
  expect_equal(test$oppo_end, 3:4)
  expect_equal(test$.linetype, 1:2)
})

test_that("key_segment_map works as intended", {

  data <- data.frame(
    x = c("A", "B"), y = 1:2, xend = c("B",  "C"), yend = 3:4
  )

  test <- key_segment_map(
    data, value = x, oppo = y, value_end = xend, oppo_end = yend,
    linetype = 1:2, color = "blue"
  )

  expect_equal(test$value, c("A", "B"))
  expect_equal(test$value_end, c("B", "C"))
  expect_equal(test$oppo, 1:2)
  expect_equal(test$oppo_end, 3:4)
  expect_equal(test$.linetype, 1:2)
  expect_equal(test$.colour, c("blue", "blue"))
})

test_that("key_dendro works as intended", {

  dummy <- scale_x_discrete()
  d <- hclust(dist(matrix(1:9, 3)))
  ptype <- data.frame(
    value = double(), oppo = double(),
    value_end = double(), oppo_end = double()
  )

  test <- key_dendro(d, type = "rectangle")(dummy)
  expect_vector(test, ptype, size = 8)

  test <- key_dendro(d, type = "triangle")(dummy)
  expect_vector(test, ptype, size = 4)

})
