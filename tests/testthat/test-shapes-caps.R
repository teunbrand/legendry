test_that("all cap shapes return matrices", {

  test <- cap_triangle()
  expect_true(is.matrix(test))
  expect_identical(dim(test), c(3L, 2L))

  test <- cap_round()
  expect_true(is.matrix(test))
  expect_identical(dim(test), c(100L, 2L))

  test <- cap_arch()
  expect_true(is.matrix(test))
  expect_identical(dim(test), c(100L, 2L))

  test <- cap_ogee()
  expect_true(is.matrix(test))
  expect_identical(dim(test), c(101L, 2L))

  test <- cap_none()
  expect_true(is.matrix(test))
  expect_identical(dim(test), c(2L, 2L))

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
    resolve_cap_shape(matrix(NA, 2L, 1L)),
    "2 columns"
  )

  expect_error(
    resolve_cap_shape(matrix(NA, 1L, 2L)),
    "2 or more rows"
  )
})
