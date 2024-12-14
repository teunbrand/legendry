test_that("all cap shapes return matrices", {

  test <- cap_triangle()
  expect_true(is.matrix(test))
  expect_equal(dim(test), c(3, 2))

  test <- cap_round()
  expect_true(is.matrix(test))
  expect_equal(dim(test), c(100, 2))

  test <- cap_arch()
  expect_true(is.matrix(test))
  expect_equal(dim(test), c(100, 2))

  test <- cap_ogee()
  expect_true(is.matrix(test))
  expect_equal(dim(test), c(101, 2))

  test <- cap_none()
  expect_true(is.matrix(test))
  expect_equal(dim(test), c(2, 2))

})

test_that("resolve_cap_shape returns appropriate errors", {

  expect_error(
    resolve_cap_shape("foobar"),
    "Cannot find function"
  )

  expect_error(
    resolve_cap_shape(mtcars),
    "must be a"
  )

  expect_error(
    resolve_cap_shape(matrix(NA, 2, 1)),
    "2 columns"
  )

  expect_error(
    resolve_cap_shape(matrix(NA, 1, 2)),
    "2 or more rows"
  )
})
