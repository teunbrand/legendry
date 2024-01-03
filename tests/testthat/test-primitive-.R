test_that("new_params include default params", {
  # This test acts as a sentinel when ggplot2 changes default parameters
  expect_equal(new_params(), Guide$params)
})
