
test_that("coord_guided's ratio parameter works", {

  p <- ggplot() +
    geom_blank() +
    coord_guided(ratio = 1)
  gt <- ggplotGrob(p)

  expect_true(gt$respect)
  expect_equal(as.numeric(gt$widths[5]), 1)
  expect_equal(as.numeric(gt$heights[7]), 1)

  p <- ggplot() +
    geom_blank() +
    coord_guided()
  gt <- ggplotGrob(p)

  expect_false(gt$respect)
  expect_equal(as.numeric(gt$widths[5]), 1)
  expect_equal(as.numeric(gt$heights[7]), 1)

  p <- ggplot() +
    geom_blank() +
    coord_guided(ratio = 2)
  gt <- ggplotGrob(p)

  expect_true(gt$respect)
  expect_equal(as.numeric(gt$widths[5]), 1)
  expect_equal(as.numeric(gt$heights[7]), 2)

})
