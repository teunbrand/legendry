test_that("key_range_auto works as intended", {

  fun <- key_range_auto(colour = "red")
  expect_type(fun, "closure")

  template <- scale_x_discrete(limits = c("1 A", "2 A", "1 B", "2 B", "3 A"))
  test <- fun(template)
  expect_s3_class(test, "key_range")

  # First 5 should be singletons
  expect_identical(unclass(test$start), c(1.0:5.0, 1.0, 3.0, 5.0))
  expect_identical(unclass(test$end),   c(1.0:5.0, 2.0, 4.0, 5.0))
  expect_identical(test$.label, c(1L:2L, 1L:3L, "A", "B", "A"))
  expect_identical(test$.level, rep(c(0L, 1L), c(5L, 3L)))
  expect_identical(test$.colour, rep("red", nrow(test)))

  template$limits[5L] <- "3"
  expect_warning(fun(template), regexp = "can be split into equal lengths")

  # No nesting
  template$limits <- LETTERS[1L:5L]
  test <- fun(template)

  expect_identical(unclass(test$start), seq(1.0, 5.0, by = 1.0))
  expect_identical(unclass(test$end),   seq(1.0, 5.0, by = 1.0))
  expect_identical(test$.label, LETTERS[1L:5L])
  expect_identical(test$.level, rep(0L, 5L))
  expect_identical(test$.colour, rep("red", nrow(test)))

})

test_that("key_range_manual works as intended", {

  test <-
    key_range_manual(
      1L:5L, 4L:8L, LETTERS[1L:5L],
      c(1L, 1L, 2L, 2L, 1L), colour = "blue"
    )
  expect_s3_class(test, "key_range")

  expect_identical(test$start, 1L:5L)
  expect_identical(test$end,   4L:8L)
  expect_identical(test$.label, LETTERS[1L:5L])
  expect_identical(test$.level, c(1L, 1L, 2L, 2L, 1L))
  expect_identical(test$.colour, rep("blue", nrow(test)))

})

test_that("key_range_map works as intended", {

  test <- key_range_map(
    presidential,
    start = start, end = end,
    name = name, colour = "green"
  )
  expect_s3_class(test, "key_range")

  expect_identical(test$start,  presidential$start)
  expect_identical(test$end,    presidential$end)
  expect_identical(test$.label, presidential$name)
  expect_identical(test$.colour, rep("green", nrow(test)))

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

  test <- key_range_rle(rep(LETTERS[1L:5L], 5L:1L), colour = "orange")
  expect_s3_class(test, "key_range")

  expect_identical(
    test$start,
    as_mapped_discrete(c(1.0, 6.0, 10.0, 13.0, 15.0))
  )
  expect_identical(test$end, as_mapped_discrete(c(5.0, 9.0, 12.0, 14.0, 15.0)))
  expect_identical(test$.label, LETTERS[1L:5L])
  expect_identical(test$.colour, rep("orange", nrow(test)))
})

test_that("range_extract_key can censor oob values", {

  scale <- scale_x_continuous(
    limits = c(0.0, 10.0)
  )

  key <- key_range_manual(
    start = c(-1.0, 2.0, 9.0),
    end   = c(1.0, 8.0, 11.0),
    name  = c("A", "B", "C")
  )

  test <- range_extract_key(scale, "x", key, oob = "censor")
  expect_identical(nrow(test), 1L)
  expect_identical(test$.label, "B")
})

test_that("range_extract_key backtransforms AsIs variables", {

  scale <- scale_x_continuous(limits = c(0.0, 10.0))

  key <- key_range_manual(
    start = I(c(0.1, 0.3, 0.5)),
    end   = c(1.0, 3.0, 5.0),
    name  = c("A", "B", "C")
  )

  test <- range_extract_key(scale, "x", key)
  expect_identical(test$start, test$end)
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
    limits = c(0.0, 10.0),
    key = data.frame(
      start = c(0.0, 4.0),
      end   = c(3.0, 9.0)
    ),
    decor = data.frame(colour = 5.0)
  )

  test <- setup_range_params(params)
  expect_equal(test$key$x, c(0.0, 0.4))
  expect_equal(test$key$y, c(0.0, 0.0))
  expect_equal(test$key$xend, c(0.3, 0.9))
  expect_equal(test$decor$x, 0.5)

  params$direction <- "vertical"
  test <- setup_range_params(params)
  expect_equal(test$key$y, c(0.0, 0.4))
  expect_equal(test$key$x, c(0.0, 0.0))
  expect_equal(test$key$yend, c(0.3, 0.9))
  expect_equal(test$decor$y, 0.5)
})
