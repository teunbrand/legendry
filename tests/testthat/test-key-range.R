test_that("key_range_auto works as intended", {

  fun <- key_range_auto()
  expect_type(fun, "closure")

  template <- scale_x_discrete(limits = c("1 A", "2 A", "1 B", "2 B", "3 A"))
  test <- fun(template)
  expect_s3_class(test, "key_range")

  # First 5 should be singletons
  expect_equal(unclass(test$start),  c(1:5, 1, 3, 5))
  expect_equal(unclass(test$end),    c(1:5, 2, 4, 5))
  expect_equal(test$.label, c(1:2, 1:3, "A", "B", "A"))
  expect_equal(test$.level, rep(c(0, 1), c(5, 3)))

  template$limits[5] <- "3"
  expect_warning(fun(template), regexp = "can be split into equal lengths")

  # No nesting
  template$limits <- LETTERS[1:5]
  test <- fun(template)

  expect_equal(unclass(test$start), 1:5)
  expect_equal(unclass(test$end),   1:5)
  expect_equal(test$.label, LETTERS[1:5])
  expect_equal(test$.level, rep(0, 5))

})

test_that("key_range_manual works as intended", {

  test <- key_range_manual(1:5, 4:8, LETTERS[1:5], c(1, 1, 2, 2, 1))
  expect_s3_class(test, "key_range")

  expect_equal(test$start, 1:5)
  expect_equal(test$end,   4:8)
  expect_equal(test$.label, LETTERS[1:5])
  expect_equal(test$.level, c(1, 1, 2, 2, 1))

})

test_that("key_range_map works as intended", {

  test <- key_range_map(presidential, start = start, end = end, name = name)
  expect_s3_class(test, "key_range")

  expect_equal(test$start, presidential$start)
  expect_equal(test$end,   presidential$end)
  expect_equal(test$.label, presidential$name)

  expect_warning(expect_warning(
    key_range_map(presidential, foo = start),
    "No valid data"
  ), "Ignoring unknown")

  expect_error(
    key_range_map(presidential, start = start),
    "column is required"
  )
})
