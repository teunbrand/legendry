test_that("string chunking works as expected", {

  test <- chunk_string("foo\nbar")
  expect_equal(test$string, c("foo", "bar"))
  expect_equal(test$line, 1:2)
  expect_equal(test$depth, c(0, 0))
  expect_equal(test$group, c(1, 1))

  test <- chunk_string("foo{.bar}")
  expect_equal(test$string, c("foo", "{.bar}"))
  expect_equal(test$line, c(1, 1))
  expect_equal(test$depth, c(0, 1))
  expect_equal(test$group, c(1, 2))

  string <- c("foo {.bar {.baz} qux {.quux} corge} grault {.garply}")
  test <- chunk_string(string)
  expect_equal(
    test$string,
    c("foo ", "{.bar ", "{.baz}", " qux ", "{.quux}", " corge}", " grault ", "{.garply}")
  )
  expect_equal(test$line, rep(1L, 8))
  expect_equal(test$depth, c(0, 1, 2, 1, 2, 1, 0, 1))
  expect_equal(test$group, c(1, 2, 4, 2, 5, 2, 1, 3))

})

test_that("guide_subtitle works as intended", {

  p <- ggplot(mtcars, aes(mpg, disp, colour = factor(cyl))) +
    geom_point()

  vdiffr::expect_doppelganger(
    "standard subtitle guide",
    p + scale_colour_discrete(
      name = "Cars with {.1 four}, {.3 eight} and {.2 six} cylinders",
      guide = "subtitle"
    )
  )

  # Should be the exact same as previous
  vdiffr::expect_doppelganger(
    "standard subtitle guide",
    p + scale_colour_discrete(
      name  = "Cars with {.1}, {.3} and {.2} cylinders",
      labels = c("four", "six", "eight"),
      guide = "subtitle"
    )
  )

})
