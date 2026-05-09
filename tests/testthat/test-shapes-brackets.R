
test_that("every bracket function returns a matrix", {

  test <- bracket_line()
  expect_true(is.matrix(test))
  expect_identical(dim(test), c(2L, 2L))

  test <- bracket_square()
  expect_true(is.matrix(test))
  expect_identical(dim(test), c(4L, 2L))

  test <- bracket_chevron()
  expect_true(is.matrix(test))
  expect_identical(dim(test), c(3L, 2L))

  test <- bracket_round()
  expect_true(is.matrix(test))
  expect_identical(dim(test), c(100L, 2L))

  test <- bracket_sigmoid()
  expect_true(is.matrix(test))
  expect_identical(dim(test), c(100L, 2L))

  test <- bracket_atan()
  expect_true(is.matrix(test))
  expect_identical(dim(test), c(100L, 2L))

  test <- bracket_curvy()
  expect_true(is.matrix(test))
  expect_identical(dim(test), c(100L, 2L))

})

test_that("resolve_bracket throws appropriate errors", {
  expect_error(
    resolve_bracket("foobar"),
    "Cannot find function"
  )
  expect_error(
    resolve_bracket(mtcars),
    "must be a"
  )
  expect_error(
    resolve_bracket(matrix(NA, 2L, 1L)),
    "2 or 3 columns"
  )
  expect_error(
    resolve_bracket(matrix(NA, 0L, 2L)),
    "2 or more rows"
  )
})

test_that("transform_bracket works for theta positions", {

  expect_null(transform_bracket(NULL))

  bracket <- data.frame(
    x = c(1.0, 2.0),
    offset = 0.0
  )
  panel <- list(
    bbox = list(x = c(0.0, 1.0), y = c(0.0, 1.0)),
    inner_radius = c(0.2, 0.4),
    r.range = c(0.0, 1.0),
    theta.range = c(0.0, 4.0),
    arc = c(0.0, 2.0 * pi),
    direction = 1.0
  )

  coord <- coord_radial()

  test <- transform_bracket(bracket, "theta", coord, panel)
  expect_gte(nrow(test), 40L)
  expect_equal(test$r[1L], 0.4)

  test <- transform_bracket(bracket, "theta.sec", coord, panel)
  expect_lte(nrow(test), 10L)
  expect_equal(test$r[1L], 0.2)
})
