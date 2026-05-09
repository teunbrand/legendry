test_that("legend cross labels can be placed anywhere", {

  p <- ggplot(data_frame0(colour = c("A:a", "B:a", "C:a", "A:b", "C:b"))) +
    geom_point(aes(1L:5L, 1L:5L, colour = colour)) +
    scale_colour_discrete(guide = guide_legend_cross())

  build <- ggplot_build(p)
  guide  <- build$plot$guides$get_guide("colour")
  params <- build$plot$guides$get_params("colour")
  params[c("position", "direction")] <- list("right", "vertical")

  sets <- list(
    c("left", "bottom"),
    c("left", "top"),
    c("right", "bottom"),
    c("right", "top")
  )

  grobs <- lapply(sets, function(set) {
    guide$draw(
      theme_get() + theme(legend.text.position = set),
      params = params
    )
  })

  gt <- gtable(unit(c(0.5, 0.5), "null"), unit(c(0.5, 0.5), "null"))
  gt <- gtable_add_grob(
    gt, grobs,
    t = c(2L, 1L, 2L, 1L), l = c(1L, 1L, 2L, 2L)
  )

  vdiffr::expect_doppelganger(
    "legend cross orientations",
    gt
  )
})

test_that("cross legend can be constructed from single scale", {
  df <- data_frame0(
    x = 1L:5L, y = 1L:5L,
    z = c("A:1", "A:2", "B:2", "C:1", "C:2")
  )

  p <- ggplot(df, aes(x, y, colour = z)) +
    geom_point() +
    guides(colour = "legend_cross")

  vdiffr::expect_doppelganger(
    "legend cross single scale",
    p
  )

  p <- ggplot(df, aes(x, y, colour = z)) +
    geom_point() +
    guides(colour = guide_legend_cross(reverse = c(TRUE, TRUE)))

  vdiffr::expect_doppelganger(
    "legend cross with double reverse",
    p
  )
})

test_that("cross legend can be constructed from dual scales", {

  df <- data_frame0(
    x = 1L:5L, y = 1L:5L,
    v = c("A", "A", "B", "C", "C"),
    w = c("1", "2", "2", "1", "2")
  )

  guide <- guide_legend_cross(title = "cross legend", key = "auto")

  p <- ggplot(df, aes(x, y, colour = v, shape = w)) +
    geom_point() +
    scale_colour_discrete(guide = guide) +
    scale_shape_discrete(guide = guide)

  vdiffr::expect_doppelganger(
    "legend cross two scales",
    p
  )

  guide <- guide_legend_cross(title = "cross legend", key = "auto",
                              swap = TRUE)

  p <- ggplot(df, aes(x, y, colour = v, shape = w)) +
    geom_point() +
    scale_colour_discrete(guide = guide) +
    scale_shape_discrete(guide = guide)

  vdiffr::expect_doppelganger(
    "legend cross two scales swapped order",
    p
  )
})

test_that("subtitles are placed correctly", {

  placement <- position_text(
    angle  = c(0.0, 270.0, 180.0, 90.0),
    colour = c("orchid", "tomato", "dodgerblue", "limegreen"),
    hjust  = 0.5
  )

  df <- data_frame0(
    x = 1L:5L, y = 1L:5L,
    z = c("A:1", "A:2", "B:2", "C:1", "C:2")
  )

  p <- ggplot(df, aes(x, y, colour = z)) +
    geom_point() +
    guides(colour = guide_legend_cross(
      row_title = "Row Title",
      col_title = "Column Title",
      subtitle_position = placement
    ))

  vdiffr::expect_doppelganger(
    "legend cross subtitles bottom right",
    p
  )

  p <- ggplot(df, aes(x, y, colour = z)) +
    geom_point() +
    guides(colour = guide_legend_cross(
      row_title = "Row Title",
      col_title = "Column Title",
      subtitle_position = placement,
      theme = theme(
        legend.text.position = c("top", "left")
      )
    ))

  vdiffr::expect_doppelganger(
    "legend cross subtitles top left",
    p
  )
})

test_that("merge strategies work as intended", {

  df <- data_frame0(
    x = 1L:5L, y = 1L:5L,
    v = c("A", "A", "B", "C", "C"),
    w = c("1", "2", "2", "1", "2")
  )

  guide <- guide_legend_cross(title = "cross legend", key = "auto")

  # Uses the 'incomplete' strategy
  p <- ggplot(df, aes(x, y, colour = v, shape = w)) +
    geom_point() +
    scale_colour_discrete(guide = guide) +
    scale_shape_discrete(guide = guide)

  build <- ggplot_build(p)
  key <- build$plot$guides$get_params(1L)$key
  expect_identical(key$.row_label, c("A", "A", "B", "B", "C", "C"))
  expect_identical(key$.col_label, c("1", "2", "1", "2", "1", "2"))

  # Uses the 'partial' strategy
  p <- ggplot(df, aes(x, y, colour = paste(v, w), shape = w)) +
    geom_point() +
    scale_colour_discrete(guide = guide_legend_cross(title = "cross legend")) +
    scale_shape_discrete(guide = guide)

  build <- ggplot_build(p)
  key <- build$plot$guides$get_params(1L)$key
  expect_identical(key$.row_label, factor(rep(c("1", "2"), each = 3L)))
  expect_identical(key$.col_label, factor(c("A", "B", "C", "A", "B", "C")))
  # The B-1 combination does not exist in the data
  expect_true(is.na(key$colour[2L]))
  expect_false(is.na(key$shape[2L]))

  # Uses the 'complete' strategy
  guide <- guide_legend_cross(title = "cross legend")
  p <- ggplot(df, aes(x, y, colour = paste(v, w), shape = paste(v, w))) +
    geom_point() +
    scale_colour_discrete(guide = guide) +
    scale_shape_discrete(guide = guide)

  build <- ggplot_build(p)
  key <- build$plot$guides$get_params(1L)$key
  expect_identical(key$.row_label, factor(rep(c("1", "2"), each = 3L)))
  expect_identical(key$.col_label, factor(c("A", "B", "C", "A", "B", "C")))
  # The B-1 combination does not exist in the data
  expect_true(is.na(key$colour[2L]))
  expect_true(is.na(key$shape[2L]))

  # Edge cases
  a <- data.frame(foo = 1L:2L, .row_label = 1L:2L, .col_label = 1L:2L)
  b <- data.frame(bar = 3L:4L, .row_label = 3L:4L, .col_label = 3L:4L)
  expect_error(cross_merge_complete(a, b), "Cannot merge")
  expect_error(cross_merge_partial(a, b), "Cannot match")
  d <- data_frame0(qux = 1L:2L, .label = c("A", "B"))
  expect_identical(d, cross_merge_incomplete(d, d))
})
