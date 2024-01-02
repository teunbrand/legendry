
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

