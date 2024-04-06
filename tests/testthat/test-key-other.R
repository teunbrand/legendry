test_that("key_sequence gives appropriate errors", {

  key <- key_sequence()

  expect_error(
    key(scale_x_discrete(), "x"),
    "for discrete scales"
  )

  expect_error(
    key_sequence(0)
  )
})

test_that("key_bins works with discrete scales", {
  skip_if(
    "even.steps" %in% fn_fmls_names(parse_binned_breaks),
    "Not implemented yet"
  )

  scale <- scale_x_discrete(limits = c("[0,1)", "[1,3)", "[3,4)"))

  test <- key_bins(even.steps = TRUE, show.limits = TRUE)(scale, "x")
  expect_equal(test$.label, as.character(c(0, 1, 3, 4)))
  expect_equal(test$min, c(0:2, NA))
  expect_equal(test$max, c(1:3, NA))

  test <- key_bins(even.steps = FALSE, show.limits = TRUE)(scale, "x")
  expect_equal(test$.label, as.character(c(0, 1, 3, 4)))
  expect_equal(test$min, c(0, 1, 3, NA))
  expect_equal(test$max, c(1, 3, 4, NA))

})

test_that("key_bins throws appropriate messages", {

  key <- key_bins(show.limits = TRUE)

  scale <- scale_x_continuous(
    limits = c(0, 10),
    breaks = c(2, 8),
    labels = c("2", "8")
  )

  expect_warning(
    key(scale, "x"),
    "is ignored"
  )


})
