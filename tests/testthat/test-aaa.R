test_that("new_params works as intended", {

  params <- new_params(foo = "bar")
  expect_named(
    params,
    c(names(Guide$params), "foo")
  )

})

test_that("standard_extract_key works as intended", {

  scale <- scale_x_continuous(limits = c(0, 10), breaks = c(1, 5, 10))
  key <- standard_extract_key(scale, "x", key = "auto")
  expect_snapshot(key)

  key <- standard_extract_key(scale, "x", key = key_manual(I(c(0.3, 0.7))))
  expect_equal(key$x, I(c(0.3, 0.7)))

})
