test_that("key_range_auto works as intended", {

  fun <- key_range_auto(colour = "red")
  expect_type(fun, "closure")

  template <- scale_x_discrete(limits = c("1 A", "2 A", "1 B", "2 B", "3 A"))
  test <- fun(template)
  expect_s3_class(test, "key_range")

  # First 5 should be singletons
  expect_equal(unclass(test$start),  c(1:5, 1, 3, 5))
  expect_equal(unclass(test$end),    c(1:5, 2, 4, 5))
  expect_equal(test$.label, c(1:2, 1:3, "A", "B", "A"))
  expect_equal(test$.level, rep(c(0, 1), c(5, 3)))
  expect_equal(test$.colour, rep("red", nrow(test)))

  template$limits[5] <- "3"
  expect_warning(fun(template), regexp = "can be split into equal lengths")

  # No nesting
  template$limits <- LETTERS[1:5]
  test <- fun(template)

  expect_equal(unclass(test$start), 1:5)
  expect_equal(unclass(test$end),   1:5)
  expect_equal(test$.label, LETTERS[1:5])
  expect_equal(test$.level, rep(0, 5))
  expect_equal(test$.colour, rep("red", nrow(test)))

})

test_that("key_range_manual works as intended", {

  test <- key_range_manual(1:5, 4:8, LETTERS[1:5], c(1, 1, 2, 2, 1), colour = "blue")
  expect_s3_class(test, "key_range")

  expect_equal(test$start, 1:5)
  expect_equal(test$end,   4:8)
  expect_equal(test$.label, LETTERS[1:5])
  expect_equal(test$.level, c(1, 1, 2, 2, 1))
  expect_equal(test$.colour, rep("blue", nrow(test)))

})

test_that("key_range_map works as intended", {

  test <- key_range_map(presidential, start = start, end = end, name = name, colour = "green")
  expect_s3_class(test, "key_range")

  expect_equal(test$start,  presidential$start)
  expect_equal(test$end,    presidential$end)
  expect_equal(test$.label, presidential$name)
  expect_equal(test$.colour, rep("green", nrow(test)))

  expect_warning(expect_warning(
    key_range_map(presidential, foo = start),
    "No valid data"
  ), "Ignoring unknown")

  expect_error(
    key_range_map(presidential, start = start),
    "column is required"
  )
})

test_that("key_range_rle works as intended", {

  test <- key_range_rle(rep(LETTERS[1:5], 5:1), colour = "orange")
  expect_s3_class(test, "key_range")

  expect_equal(test$start, c(0, 5, 9, 12, 14) + 0.5)
  expect_equal(test$end, c(5, 9, 12, 14, 15) + 0.5)
  expect_equal(test$.label, LETTERS[1:5])
  expect_equal(test$.colour, rep("orange", nrow(test)))
})

test_that("range_extract_key can censor oob values", {

  scale <- scale_x_continuous(
    limits = c(0, 10)
  )

  key <- key_range_manual(
    start = c(-1, 2, 9),
    end   = c(1, 8, 11),
    name  = c("A", "B", "C")
  )

  test <- range_extract_key(scale, "x", key, oob = "censor")
  expect_equal(nrow(test), 1L)
  expect_equal(test$.label, 'B')
})

test_that("range_extract_key backtransforms AsIs variables", {

  scale <- scale_x_continuous(limits = c(0, 10))

  key <- key_range_manual(
    start = I(c(0.1, 0.3, 0.5)),
    end   = c(1, 3, 5),
    name  = c("A", "B", "C")
  )

  test <- range_extract_key(scale, "x", key)
  expect_equal(test$start, test$end)
})

test_that("range_from_label can extract ranges", {

  values <- c("A 1", "B 1", "C 2")
  scale <- scale_x_discrete(limits = values)

  test <- range_from_label(scale, "x")
  expect_snapshot(test)

  exprs <- as.expression(values)
  scale <- scale_x_discrete(limits = values, labels = exprs)

  expect_error(
    range_from_label(scale, "x"),
    "Cannot split"
  )

})

test_that("setup_range_params sets up ranges correctly", {

  params <- list(
    aesthetics = "colour",
    position = "right",
    direction = "horizontal",
    limits = c(0, 10),
    key = data.frame(
      start = c(0, 4),
      end   = c(3, 9)
    ),
    decor = data.frame(colour = 5)
  )

  test <- setup_range_params(params)
  expect_equal(test$key$x, c(0, 0.4))
  expect_equal(test$key$y, c(0, 0))
  expect_equal(test$key$xend, c(0.3, 0.9))
  expect_equal(test$decor$x, 0.5)

  params$direction <- "vertical"
  test <- setup_range_params(params)
  expect_equal(test$key$y, c(0, 0.4))
  expect_equal(test$key$x, c(0, 0))
  expect_equal(test$key$yend, c(0.3, 0.9))
  expect_equal(test$decor$y, 0.5)
})
