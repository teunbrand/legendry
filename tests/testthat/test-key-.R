
test_that("key_auto works as intended", {

  fun <- key_auto(colour = "red")
  expect_type(fun, "closure")

  template <- scale_x_discrete(limits = LETTERS[1L:5L])
  test <- fun(template)
  expect_s3_class(test, "key_standard")

  expect_identical(test$x, as_mapped_discrete(1L:5L))
  expect_identical(test$.value, LETTERS[1L:5L], ignore_attr = TRUE)
  expect_identical(test$.label, LETTERS[1L:5L])
  expect_identical(test$.colour, rep("red", nrow(test)))

})

test_that("key_manual works as intended", {

  test <- key_manual(1L:5L, colour = "blue")
  expect_s3_class(test, "key_standard")

  expect_identical(test$aesthetic, 1L:5L)
  expect_identical(test$.value, 1L:5L)
  expect_identical(test$.label, as.character(1L:5L))
  expect_identical(test$.colour, rep("blue", nrow(test)))
})

test_that("key_map works as intended", {

  test <-
    key_map(iris, aesthetic = as.character(unique(Species)), colour = "green")
  expect_s3_class(test, "key_standard")

  expect_identical(test$aesthetic, levels(iris$Species))
  expect_identical(test$.value, levels(iris$Species))
  expect_identical(test$.label, levels(iris$Species))
  expect_identical(test$.colour, rep("green", nrow(test)))

  expect_error(expect_warning(expect_warning(
    key_map(iris, foo = Species),
    "No valid data"
  ), "Ignoring unknown"), "columns are required")
})

test_that("key_minor works as intended", {

  fun <- key_minor(colour = "purple")
  expect_type(fun, "closure")

  template <- scale_x_continuous(
    limits = c(0.0, 10.0),
    breaks = seq(0.0, 10.0, by = 2.0)
  )
  test <- fun(template)
  expect_s3_class(test, "key_standard")

  expect_equal(
    test$x, c(0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 1.0, 3.0, 5.0, 7.0, 9.0),
    ignore_attr = TRUE
  )
  expect_equal(
    test$.value, c(0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 1.0, 3.0, 5.0, 7.0, 9.0),
    ignore_attr = TRUE
  )
  expect_identical(
    test$.label,
    c("0", "2", "4", "6", "8", "10", rep(NA_character_, 5L))
  )
  expect_identical(test$.type, rep(c("major", "minor"), c(6L, 5L)))
  expect_identical(test$.colour, rep("purple", nrow(test)))

})

test_that("key_log works as intended", {

  fun <- key_log(colour = "pink")
  expect_type(fun, "closure")

  template <- scale_x_continuous(limits = c(0.1, 10.0), transform = "log10")
  test <- fun(template)
  expect_s3_class(test, "key_standard")
  expect_snapshot(test)
  expect_identical(test$.colour, rep("pink", nrow(test)))
})

test_that("validate_key_types throws appropriate warning", {

  expect_silent(
    test <- validate_key_types(data_frame0(.type = "major"))
  )
  expect_identical(dim(test), c(1L, 1L))
  expect_warning(
    test <- validate_key_types(data_frame0(.type = "foobar")),
    "Unknown types are dropped"
  )
  expect_identical(dim(test), c(0L, 1L))
})

test_that("resolve_key throws appropriate error", {

  expect_silent(resolve_key("auto"))
  expect_error(
    resolve_key(mtcars),
    "Unknown key specification"
  )

})

test_that("log10_keys returns sensible results", {

  scale <- scale_x_discrete()
  expect_error(
    log10_keys(scale, "x"),
    "Cannot calculate logarithmic ticks for discrete scales"
  )

  scale <- scale_x_log10()
  scale$train(c(0.0, 2.0))

  expect_warning(
    test <- log10_keys(scale, "x", 10L, negative_small = 0.1, expanded = FALSE),
    "argument will override"
  )
  expect_identical(unique(test$.type), c("major", "minor", "mini"))

  scale <- scale_x_continuous(transform = "asinh")
  scale$train(c(-5.0, 5.0))

  test <- log10_keys(scale, "x", NULL, 0.1, expanded = FALSE)


})
