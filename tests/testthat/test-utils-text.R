
test_that("get_text_dim_cm works", {

  style <- theme_gray()$text

  withr::with_pdf(
    tempfile(fileext = ".pdf"),
    {
      width  <- get_text_dim_cm("foobar", style, "width")
      height <- get_text_dim_cm("foobar", style, "height")
      both   <- get_text_dim_cm("foobar", style, "both")
    }
  )

  expect_equal(both$width, width)
  expect_equal(both$height, height)

  expect_error(
    get_text_dim_cm("foobar", element_rect(), "both"),
    "must be"
  )
})

test_that("get_fontmetrics works", {
  style <- theme_gray()$text
  metrics <- get_fontmetrics(style)
  expect_true(is.list(metrics))
})
