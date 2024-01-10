test_that("check_columns throws appropriate errors", {
  expect_silent(check_columns(mtcars, c("mpg", "cyl")))
  expect_error(
    check_columns(mtcars, c("foo", "bar")),
    "columns are required"
  )
})

test_that("check_list_names throws appropriate errors", {

  lst <- as.list(mtcars)
  expect_silent(check_list_names(lst, c("mpg", "cyl")))
  expect_error(
    check_list_names(lst, c("foo", "bar")),
    "must have named"
  )

})

test_that("check_unit throws appropriate errors", {

  expect_silent(check_unit(unit(1, "cm")))
  expect_silent(check_unit(NULL, allow_null = TRUE))
  expect_error(
    check_unit(10),
    "must be"
  )

})

test_that("check_bare_numeric throws appropriate errors", {

  expect_silent(check_bare_numeric(c(10L, 20L)))
  expect_silent(check_bare_numeric(NULL, allow_null = TRUE))
  expect_error(
    check_bare_numeric(unit(1, "cm")),
    "must be a bare"
  )

})

test_that("check_object throws appropriate errors", {

  expect_silent(check_object(mtcars, is.data.frame, "data.frame"))
  expect_silent(check_object(NULL, is.data.frame, "data.frame", allow_null = TRUE))
  expect_error(
    check_object(mtcars, is.environment, "an environment"),
    "must be an environment"
  )

})

test_that("check_length throws appropriate errors", {

  expect_silent(check_length(NULL, allow_null = TRUE))
  expect_silent(check_length(1:2, min = 1))
  expect_silent(check_length(1:2, max = 2))
  expect_silent(check_length(1:2, min = 1, max = 3))
  expect_silent(check_length(1:2, exact = 2))

  expect_error(
    check_length(1:2, min = 3),
    "more than or equal to 3"
  )
  expect_error(
    check_length(1:2, max = 1),
    "less than or equal to 1"
  )
  expect_error(
    check_length(1:2, min = 3, max = 5),
    "between 3 and 5"
  )
  expect_error(
    check_length(1:2, exact = c(1, 3)),
    "equal to 1 or 3"
  )
  expect_error(
    check_length(1:2, exact = 1),
    "equal to 1"
  )

})

test_that("check_inherits throws appropriate errors", {

  expect_silent(check_inherits(mtcars, "data.frame"))
  expect_silent(check_inherits(NULL, "data.frame", allow_null = TRUE))
  expect_error(
    check_inherits(1:2, "data.frame"),
    "must be a "
  )

})

test_that("check_argmatch throws appropriate errors", {

  expect_silent(check_argmatch("A", c("A", "B")))
  expect_silent(check_argmatch(NULL, c("A", "B"), allow_null = TRUE))
  expect_snapshot_error(check_argmatch("C", c("A", "B")))
  expect_error(
    check_argmatch(mtcars, c("A", "B")),
    "must be a single string"
  )

})
