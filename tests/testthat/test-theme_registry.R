
test_that("theme elements can be registered", {

  expect_in("gguidance.bracket", names(get_element_tree()))

  ggplot2::reset_theme_settings()

  expect_false("gguidance.bracket" %in% names(get_element_tree()))

  register_gguidance_elements()

  expect_in("gguidance.bracket", names(get_element_tree()))

})
