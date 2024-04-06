
test_that("eval_aes evaluates aesthetics", {

  test <- eval_aes(mtcars, aes(colour = cyl), optional = "colour")
  expect_equal(test$colour, mtcars$cyl)

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

  expect_equal(NULL %|NA|% 1, 1)
  expect_equal(1 %|NA|% 2, 1)
  expect_equal(c(1, NA, 2) %|NA|% 4, c(1, 4, 2))
  expect_equal(c(1, NA, 2) %|NA|% c(3, 4, 5), c(1, 4, 2))

})

test_that("pad pads", {
  expect_equal(pad(1, 2), c(1, NA))
  expect_equal(pad(1, 2, where = "start"), c(NA, 1))
  expect_equal(pad(1, 1), 1)
})

test_that("scale_transform throws appropriate error", {
  expect_error(
    scale_transform("A", scale_x_continuous()),
    "not discrete"
  )
})

test_that("by_group computes things by group", {
  test <- by_group(1:4, group = c("A", "A", "B", "B"), fun = mean, value = double(1))
  expect_equal(test, c(1.5, 3.5))
})

test_that("match_list can find needle in list haystack", {
  needle <- c("D", "F")
  haystack <- list(c("A", "B", "C"), "D", c("E", "F", "G"))
  expect_equal(
    match_list(needle, haystack),
    c(2, 3)
  )
})
