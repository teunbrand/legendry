
test_that("every bracket function returns a matrix", {

  test <- bracket_line()
  expect_true(is.matrix(test))
  expect_equal(dim(test), c(2, 2))

  test <- bracket_square()
  expect_true(is.matrix(test))
  expect_equal(dim(test), c(4, 2))

  test <- bracket_chevron()
  expect_true(is.matrix(test))
  expect_equal(dim(test), c(3, 2))

  test <- bracket_round()
  expect_true(is.matrix(test))
  expect_equal(dim(test), c(100, 2))

  test <- bracket_sigmoid()
  expect_true(is.matrix(test))
  expect_equal(dim(test), c(100, 2))

  test <- bracket_atan()
  expect_true(is.matrix(test))
  expect_equal(dim(test), c(100, 2))

  test <- bracket_curvy()
  expect_true(is.matrix(test))
  expect_equal(dim(test), c(100, 2))

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
    resolve_bracket(matrix(NA, 2, 1)),
    "2 or 3 columns"
  )
  expect_error(
    resolve_bracket(matrix(NA, 0, 2)),
    "2 or more rows"
  )
})

test_that("transform_bracket works for theta positions", {

  expect_null(transform_bracket(NULL))

  bracket <- data.frame(
    x = c(1, 2),
    offset = 0
  )
  panel <- list(
    bbox = list(x = c(0, 1), y = c(0, 1)),
    inner_radius = c(0.2, 0.4),
    r.range = c(0, 1),
    theta.range = c(0, 4),
    arc = c(0, 2 * pi),
    direction = 1
  )

  coord <- coord_radial()

  test <- transform_bracket(bracket, "theta", coord, panel)
  expect_gte(nrow(test), 40)
  expect_equal(test$r[1], 0.4)

  test <- transform_bracket(bracket, "theta.sec", coord, panel)
  expect_lte(nrow(test), 10)
  expect_equal(test$r[1], 0.2)
})
