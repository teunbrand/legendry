test_that("new_compose throws appropriate messages", {

  expect_error(
    new_compose(list()),
    "must be at least one guide"
  )

  dummy1 <- new_guide(available_aes = "foo", super = Guide)
  dummy2 <- new_guide(available_aes = "bar", super = Guide)

  expect_error(
    new_compose(list(dummy1), available_aes = "bar"),
    "must include"
  )

  expect_error(
    new_compose(list(dummy1, dummy2)),
    "guides to combine have no shared"
  )

  expect_error(
    new_compose("foo"),
    "Unknown guide"
  )

})
