test_that("key_sequence gives appropriate errors", {

  key <- key_sequence()

  expect_error(
    key(scale_x_discrete(), "x"),
    "for discrete scales"
  )

  expect_error(
    key_sequence(0.0)
  )
})

test_that("key_bins works with discrete scales", {
  skip_if(
    "even.steps" %in% fn_fmls_names(parse_binned_breaks),
    "Not implemented yet"
  )

  scale <- scale_x_discrete(limits = c("[0,1)", "[1,3)", "[3,4)"))

  test <- key_bins(even.steps = TRUE, show.limits = TRUE)(scale, "x")
  expect_identical(test$.label, as.character(c(0L, 1L, 3L, 4L)))
  expect_identical(test$min, c(0L:2L, NA))
  expect_identical(test$max, c(1L:3L, NA))

  test <- key_bins(even.steps = FALSE, show.limits = TRUE)(scale, "x")
  expect_identical(test$.label, as.character(c(0L, 1L, 3L, 4L)))
  expect_identical(test$min, c(0.0, 1.0, 3.0, NA))
  expect_identical(test$max, c(1.0, 3.0, 4.0, NA))

})

test_that("key_bins throws appropriate messages", {

  key <- key_bins(show.limits = TRUE)

  scale <- scale_x_continuous(
    limits = c(0.0, 10.0),
    breaks = c(2.0, 8.0),
    labels = c("2", "8")
  )

  expect_warning(
    key(scale, "x"),
    "is ignored"
  )
})

test_that("key_upset can set order", {
  scale <- scale_x_discrete(limits = c("X,Y", "X", "", "Z"))

  key <- key_upset()
  test <- key(scale)
  test <- test[test$.symbol & !is.na(test$.symbol), ]
  expect_identical(levels(test$.value), c("X", "Y", "Z", "Other"))

  key <- key_upset(order = c("Y", "X"))
  test <- key(scale)
  test <- test[test$.symbol & !is.na(test$.symbol), ]
  expect_identical(levels(test$.value), c("Y", "X", "Z", "Other"))

  key <- key_upset(order = c(3L, 1L))
  test <- key(scale)
  test <- test[test$.symbol & !is.na(test$.symbol), ]
  expect_identical(levels(test$.value), c("Z", "X", "Y", "Other"))
})

test_that("key_upset can set labels for empty levels", {
  scale <- scale_x_discrete(limits = c("X,Y", "X", "", "Z"))

  key <- key_upset(empty_label = "foo")
  test <- key(scale)
  expect_identical(levels(test$.value), c("X", "Y", "Z", "foo"))

  key <- key_upset(empty_label = NULL)
  test <- key(scale)
  expect_identical(levels(test$.value), c("X", "Y", "Z"))
})

test_that("key_symbols levels work as intended", {

  scale <- scale_x_discrete(limits = c("X", "Y", "Z"))
  key <- key_symbols(c("X", "Y", "Z"), c("C", "A", "B"))
  test <- key(scale)

  expect_identical(
    test[c(".value", ".col")],
    data_frame0(
      .value = factor(c("C", "A", "B"), levels = c("C", "A", "B")),
      .col = c(1L, 2L, 3L)
    )
  )

  key <- key_symbols(c("X", "Y", "Z"), factor(c("C", "A", "B")))
  test <- key(scale)
  expect_identical(
    test[c(".value", ".col")],
    data_frame0(
      .value = factor(c("C", "A", "B"), levels = c("A", "B", "C")),
      .col = c(3L, 1L, 2L)
    )
  )

  key <- key_symbols(c("X", "Y", "Z"), c(2L, 3L, 1L))
  test <- key(scale)
  expect_identical(
    test[c(".value", ".col")],
    data_frame0(
      .value = factor(c("2", "3", "1"), levels = c("1", "2", "3")),
      .col = c(2L, 3L, 1L)
    )
  )
})
