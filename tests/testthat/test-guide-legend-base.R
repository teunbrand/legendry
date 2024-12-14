
test_that("guide_legend_base can parse different designs", {

  guide <- guide_legend_base()
  expect_null(guide$params$design)

  example_design <- matrix(1:4, nrow = 2, byrow = TRUE)

  guide <- guide_legend_base(design = example_design)
  expect_equal(guide$params$design, example_design)

  guide <- guide_legend_base(design = c("AB\nCD"))
  expect_equal(guide$params$design, example_design, ignore_attr = TRUE)

  guide <- guide_legend_base(design = 1:4)
  expect_equal(guide$params$design, as.matrix(1:4))

  # Check warning is emitted when there is a conflict
  expect_warning(
    guide_legend_base(design = example_design, ncol = 2),
    "`ncol` argument is ignored"
  )

  # Check error is thrown when design is invalid
  expect_error(
    guide_legend_base(design = "AB\nCDE"),
    "must be rectangular"
  )

})

test_that("design application is correct", {

  data <- data.frame(value = 1:8)

  # Default horizontal
  test <- apply_design(data, direction = "horizontal")
  expect_equal(
    test,
    data.frame(value = 1:8, .index = 1:8, .row = rep(1:2, 4), .col = rep(1:4, each = 2L))
  )

  # Default vertical
  test <- apply_design(data, direction = "vertical")
  expect_equal(
    test,
    data.frame(value = 1:8, .index = 1:8, .row = 1:8, .col = rep(1L, 8L))
  )

  # Vertical with fixed columns
  test <- apply_design(data, ncol = 2, direction = "vertical")
  expect_equal(
    test,
    data.frame(value = 1:8, .index = 1:8, .row = rep(1:4, 2), .col = rep(1:2, each = 4L))
  )

  # Custom design
  design <- matrix(c(1:3, 8, NA, 4, 7:5), nrow = 3)
  test <- apply_design(data, design = design)
  expect_equal(
    test,
    data.frame(
      value  = c(1L, 2L, 3L, 8L, 4L, 7L, 6L, 5L),
      .index = c(1L, 2L, 3L, 8L, 4L, 7L, 6L, 5L),
      .row   = c(1L, 2L, 3L, 1L, 3L, 1L, 2L, 3L),
      .col   = rep(1:3, c(3L, 2L, 3L))
    )
  )

  # Warning about flawed design
  design <- matrix(c(1:3, NA, NA, 4, 7:5), nrow = 3)
  expect_warning(
    apply_design(data, design = design),
    "insufficient levels"
  )

  # Warning about flawed ncol/nrow
  expect_warning(
    apply_design(data, nrow = 2, ncol = 2),
    "insufficient levels"
  )
})

test_that("guide_legend_base can draw a custom design", {

  design <- "1#7\n2#6\n345"

  df <- data.frame(x = LETTERS[1:7])

  p <- ggplot(df, aes(x, x, fill = x)) +
    geom_tile()

  vdiffr::expect_doppelganger(
    "standard legend design",
    p + guides(fill = guide_legend_base())
  )

  vdiffr::expect_doppelganger(
    "custom legend design",
    p + guides(fill = guide_legend_base(design = design))
  )
})
