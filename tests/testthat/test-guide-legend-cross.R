test_that("legend cross labels can be placed anywhere", {

  p <- ggplot(data.frame(colour = c("A:a", "B:a", "C:a", "A:b", "C:b"))) +
    geom_point(aes(1:5, 1:5, colour = colour)) +
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
    t = c(2, 1, 2, 1), l = c(1, 1, 2, 2)
  )

  vdiffr::expect_doppelganger(
    "legend cross orientations",
    gt
  )
})

test_that("cross legend can be constructed from single scale", {
  df <- data.frame(
    x = 1:5, y = 1:5,
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

  df <- data.frame(
    x = 1:5, y = 1:5,
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
    angle  = c(0, 270, 180, 90),
    colour = c("orchid", "tomato", "dodgerblue", "limegreen"),
    hjust  = 0.5
  )

  df <- data.frame(
    x = 1:5, y = 1:5,
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

  df <- data.frame(
    x = 1:5, y = 1:5,
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
  expect_equal(key$.row_label, c("A", "A", "B", "B", "C", "C"))
  expect_equal(key$.col_label, c("1", "2", "1", "2", "1", "2"))

  # Uses the 'partial' strategy
  p <- ggplot(df, aes(x, y, colour = paste(v, w), shape = w)) +
    geom_point() +
    scale_colour_discrete(guide = guide_legend_cross(title = "cross legend")) +
    scale_shape_discrete(guide = guide)

  build <- ggplot_build(p)
  key <- build$plot$guides$get_params(1L)$key
  expect_equal(key$.row_label, factor(rep(c("1", "2"), each = 3L)))
  expect_equal(key$.col_label, factor(c("A", "B", "C", "A", "B", "C")))
  # The B-1 combination does not exist in the data
  expect_true(is.na(key$colour[2]))
  expect_false(is.na(key$shape[2]))

  # Uses the 'complete' strategy
  guide <- guide_legend_cross(title = "cross legend")
  p <- ggplot(df, aes(x, y, colour = paste(v, w), shape = paste(v, w))) +
    geom_point() +
    scale_colour_discrete(guide = guide) +
    scale_shape_discrete(guide = guide)

  build <- ggplot_build(p)
  key <- build$plot$guides$get_params(1L)$key
  expect_equal(key$.row_label, factor(rep(c("1", "2"), each = 3L)))
  expect_equal(key$.col_label, factor(c("A", "B", "C", "A", "B", "C")))
  # The B-1 combination does not exist in the data
  expect_true(is.na(key$colour[2]))
  expect_true(is.na(key$shape[2]))

  # Edge cases
  a <- data.frame(foo = 1:2, .row_label = 1:2, .col_label = 1:2)
  b <- data.frame(bar = 3:4, .row_label = 3:4, .col_label = 3:4)
  expect_error(cross_merge_complete(a, b), "Cannot merge")
  expect_error(cross_merge_partial(a, b), "Cannot match")
  d <- data.frame(qux = 1:2, .label = c("A", "B"))
  expect_equal(d, cross_merge_incomplete(d, d))
})
