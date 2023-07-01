test_that("scale_grid_extract gives correct output for untransformed scales", {

  sc <- scale_x_continuous()
  sc$train(c(0, 10))
  vs <- ggplot2:::view_scale_primary(sc)

  # Test scale defaults
  ans <- scale_grid_extract(waiver(), waiver(), vs)
  expect_equal(ans$major, c(0, 2.5, 5, 7.5, 10))
  expect_equal(ans$minor, c(1.25, 3.75, 6.25, 8.75))

  # Test manual breaks
  ans <- scale_grid_extract(seq(0, 10, by = 2), seq(0, 10, by = 1), vs)
  expect_equal(ans$major, seq(0, 10, by = 2))
  expect_equal(ans$minor, seq(1, 9,  by = 2))

  # Test function breaks
  ans <- scale_grid_extract(scales::breaks_width(2), scales::breaks_width(1), vs)
  expect_equal(ans$major, seq(0, 10, by = 2))
  expect_equal(ans$minor, seq(1, 9,  by = 2))

})

test_that("scale_grid_extract gives correct output for transformed scales", {

  sc <- scale_x_continuous(trans = "sqrt")
  sc$train(sc$transform(c(0, 100)))
  vs <- ggplot2:::view_scale_primary(sc)

  # Test scale defaults
  ans <- scale_grid_extract(waiver(), waiver(), vs)
  expect_equal(ans$major^2, c(0, 25, 50, 75, 100))
  expect_equal(ans$minor, (ans$major[-1] + ans$major[-length(ans$major)]) / 2)

  # Test manual breaks
  ans <- scale_grid_extract(seq(0, 10, by = 2)^2, seq(1, 9, by = 2)^2, vs)
  expect_equal(ans$major, seq(0, 10, by = 2))
  expect_equal(ans$minor, seq(1, 9,  by = 2))

  # Test function breaks
  ans <- scale_grid_extract(scales::breaks_width(10), scales::breaks_width(5), vs)
  expect_equal(ans$major^2, seq(0, 100, by = 10))
  expect_equal(ans$minor^2, seq(5, 95, by = 10))
})

test_that("scale_grid_extract gives correct output for date scales", {

  sc <- scale_x_date()
  sc$train(as.Date(c("1900-01-01", "2000-01-01")))
  vs <- ggplot2:::view_scale_primary(sc)

  fmt <- function(x) unname(as.character(structure(x, class = "Date")))

  # Test scale defaults
  ans <- scale_grid_extract(waiver(), waiver(), vs)
  expect_equal(
    fmt(ans$major),
    c("1900-01-01", "1920-01-01", "1940-01-01", "1960-01-01", "1980-01-01", "2000-01-01")
  )
  expect_equal(
    fmt(ans$minor),
    c("1910-01-01", '1929-12-31', "1949-12-31", "1969-12-31", "1989-12-31")
  )

  # Test manual breaks
  ans <- scale_grid_extract(
    as.Date(c("1900-01-01", "1950-01-01", "2000-01-01")),
    as.Date(c("1925-01-01", "1975-01-01")),
    vs
  )
  expect_equal(fmt(ans$major), c("1900-01-01", "1950-01-01", "2000-01-01"))
  expect_equal(fmt(ans$minor), c("1925-01-01", "1975-01-01"))

  # Test function breaks
  ans <- scale_grid_extract(scales::breaks_width("50 years"), scales::breaks_width("25 years"), vs)
  expect_equal(fmt(ans$major), c("1900-01-01", "1950-01-01", "2000-01-01"))
  expect_equal(fmt(ans$minor), c("1925-01-01", "1975-01-01"))
})

test_that("scale_grid_extract gives correct output for discrete scales", {

  sc <- scale_x_discrete()
  sc$train(LETTERS[1:5])
  vs <- ggplot2:::view_scale_primary(sc)

  # Test scale defaults
  ans <- scale_grid_extract(waiver(), waiver(), vs)
  expect_equal(unclass(ans$major), 1:5)
  expect_null(ans$minor)

  # Test manual character breaks
  ans <- scale_grid_extract(c("A", "B", "E", "Z"), c("C", "D", "X"), vs)
  expect_equal(unclass(ans$major), c(1, 2, 5))
  expect_equal(unclass(ans$minor), c(3, 4))

  # Test manual numeric breaks
  ans <- scale_grid_extract(c(1, 2.5, 5), c(1.5, 3.5, 4.5), vs)
  expect_equal(unclass(ans$major), c(1, 2.5, 5))
  expect_equal(unclass(ans$minor), c(1.5, 3.5, 4.5))

  # Test function breaks
  ans <- scale_grid_extract(breaks_between(), seq_along, vs)
  expect_equal(unclass(ans$major), seq(0.5, 5.5, by = 1))
  expect_equal(unclass(ans$minor), 1:5)
})

test_that("guide_grid works as expected", {

  guide <- guide_grid(x_breaks = breaks_between(), x_minor_breaks = seq_along)

  p <- ggplot(mpg, aes(x = class, y = displ)) +
    geom_boxplot() +
    coord_guided(guide = guide) +
    theme_gray()

  vdiffr::expect_doppelganger(
    "guide_grid swapped major x", p
  )

  guide <- guide_grid(breaks = NULL, minor_breaks = NULL)

  p <- ggplot(mpg, aes(x = class, y = displ)) +
    geom_boxplot() +
    coord_guided(guide = guide) +
    theme_gray()

  vdiffr::expect_doppelganger(
    "guide_grid with no breaks", p
  )
})
