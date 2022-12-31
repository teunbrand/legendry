test_that("arg_range gives appropriate errors", {

  expect_equal(
    arg_range(0.5, c(0, 1)),
    0.5
  )
  expect_equal(
    arg_range(NA_real_, c(0, 1), allow_na = TRUE),
    NA_real_
  )
  expect_error(
    arg_range(2, c(0, 1)),
    "must be inside"
  )
  expect_error(
    arg_range(2, c(-Inf, 0)),
    "must be negative"
  )
  expect_error(
    arg_range(-2, c(0, Inf)),
    "must be positive"
  )
  expect_error(
    arg_range(NA_real_, c(0, 1)),
    "cannot be `NA`"
  )

  val <- c(2, NA)
  expect_error(
    arg_range(val, c(0, 1)),
    c("cannot have `NA`")
  )
})
