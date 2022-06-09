test_that("elements_text constructs a list of text elements", {

  test <- elements_text(
    colour = c("green", NA, "blue"),
    margin = list(NULL, NULL, margin(t = 5))
  )

  expect_equal(
    test,
    list(
      element_text(colour = "green"),
      element_text(),
      element_text(colour = "blue", margin = margin(t = 5))
    )
  )

  expect_equal(
    elements_text()[[1]],
    element_text()
  )

})
