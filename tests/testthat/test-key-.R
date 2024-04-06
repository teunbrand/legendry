
test_that("key_auto works as intended", {

  fun <- key_auto()
  expect_type(fun, "closure")

  template <- scale_x_discrete(limits = LETTERS[1:5])
  test <- fun(template)
  expect_s3_class(test, "key_standard")

  expect_equal(test$x, 1:5, ignore_attr = TRUE)
  expect_equal(test$.value, LETTERS[1:5], ignore_attr = TRUE)
  expect_equal(test$.label, LETTERS[1:5])

})

test_that("key_manual works as intended", {

  test <- key_manual(1:5)
  expect_s3_class(test, "key_standard")

  expect_equal(test$aesthetic, 1:5)
  expect_equal(test$.value, 1:5)
  expect_equal(test$.label, as.character(1:5))

})

test_that("key_map works as intended", {

  test <- key_map(iris, aesthetic = as.character(unique(Species)))
  expect_s3_class(test, "key_standard")

  expect_equal(test$aesthetic, levels(iris$Species))
  expect_equal(test$.value, levels(iris$Species))
  expect_equal(test$.label, levels(iris$Species))

  expect_error(expect_warning(expect_warning(
    key_map(iris, foo = Species),
    "No valid data"
  ), "Ignoring unknown"), "columns are required")
})

test_that("key_minor works as intended", {

  fun <- key_minor()
  expect_type(fun, "closure")

  template <- scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2))
  test <- fun(template)
  expect_s3_class(test, "key_standard")

  expect_equal(test$x, c(0, 2, 4, 6, 8, 10, 1, 3, 5, 7, 9), ignore_attr = TRUE)
  expect_equal(test$.value, c(0, 2, 4, 6, 8, 10, 1, 3, 5, 7, 9), ignore_attr = TRUE)
  expect_equal(test$.label, c(0, 2, 4, 6, 8, 10, rep(NA_character_, 5)))
  expect_equal(test$.type, rep(c("major", "minor"), c(6, 5)))

})

test_that("key_log works as intended", {

  fun <- key_log()
  expect_type(fun, "closure")

  template <- scale_x_continuous(limits = c(0.1, 10), transform = "log10")
  test <- fun(template)
  expect_s3_class(test, "key_standard")
  expect_snapshot(test)

})

test_that("validate_key_types throws appropriate warning", {

  expect_silent(
    test <- validate_key_types(data.frame(.type = "major"))
  )
  expect_equal(dim(test), c(1, 1))
  expect_warning(
    test <- validate_key_types(data.frame(.type = "foobar")),
    "Unknown types are dropped"
  )
  expect_equal(dim(test), c(0, 1))
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
  scale$train(c(0, 2))

  expect_warning(
    test <- log10_keys(scale, "x", 10, negative_small = 0.1, expanded = FALSE),
    "argument will override"
  )
  expect_equal(unique(test$.type), c("major", "minor", "mini"))

  scale <- scale_x_continuous(transform = "asinh")
  scale$train(c(-5, 5))

  test <- log10_keys(scale, "x", NULL, 0.1, expanded = FALSE)


})


df <- data.frame(x = rcauchy(100), y = rnorm(100))

ggplot(df, aes(x, y)) +
  geom_point() +
  scale_x_continuous(
    trans = "asinh",
    guide = guide_axis_custom("log")
  )
