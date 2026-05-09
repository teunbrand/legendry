
test_that("eval_aes evaluates aesthetics", {

  test <- eval_aes(mtcars, aes(colour = cyl), optional = "colour")
  expect_identical(test$colour, mtcars$cyl)

  expect_warning(expect_warning(
    eval_aes(mtcars, aes(colour = cyl)),
    "Ignoring unknown"
  ), "No valid data")

  expect_error(
    eval_aes(mtcars, list("colour")),
    "must be created by"
  )
})

test_that("%|NA|% works as intended", {

  expect_equal(NULL %|NA|% 1.0, 1.0)
  expect_equal(1.0 %|NA|% 2.0, 1.0)
  expect_equal(c(1.0, NA, 2.0) %|NA|% 4.0, c(1.0, 4.0, 2.0))
  expect_equal(c(1.0, NA, 2.0) %|NA|% c(3.0, 4.0, 5.0), c(1.0, 4.0, 2.0))

})

test_that("pad pads", {
  expect_equal(pad(1.0, 2.0), c(1.0, NA))
  expect_equal(pad(1.0, 2.0, where = "start"), c(NA, 1.0))
  expect_equal(pad(1.0, 1.0), 1.0)
})

test_that("scale_transform throws appropriate error", {
  expect_error(
    scale_transform("A", scale_x_continuous()),
    "not discrete"
  )
})

test_that("by_group computes things by group", {
  test <- by_group(
    1L:4L, group = c("A", "A", "B", "B"),
    fun = mean, value = 1.0
  )
  expect_identical(test, c(1.5, 3.5))
})

test_that("match_list can find needle in list haystack", {
  needle <- c("D", "F")
  haystack <- list(c("A", "B", "C"), "D", c("E", "F", "G"))
  expect_identical(
    match_list(needle, haystack),
    c(2L, 3L)
  )
})
