
test_that("theme elements can be registered", {

  expect_in("legendry.bracket", names(get_element_tree()))

  ggplot2::reset_theme_settings()

  expect_false("legendry.bracket" %in% names(get_element_tree()))

  register_legendry_elements()

  expect_in("legendry.bracket", names(get_element_tree()))

})
