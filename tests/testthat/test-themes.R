
test_that("theme elements can be registered", {

  expect_in("legendry.bracket", names(get_element_tree()))

  ggplot2::reset_theme_settings()

  expect_false("legendry.bracket" %in% names(get_element_tree()))

  register_legendry_elements()

  expect_in("legendry.bracket", names(get_element_tree()))

})

test_that("all arguments of theme_guide are used", {

  fmls <- fn_fmls_names(theme_guide)
  args <- set_names(seq_along(fmls), fmls)
  theme <- inject(theme_guide(!!!args))
  expect_setequal(unlist(theme), seq_along(fmls))

})
