test_that("connectors works", {

  expect_silent(check_connect_arg("perpendicular"))
  expect_snapshot_error(
    check_connect_arg(data.frame(foo = 1L:2L))
  )


  df <- data.frame(
    x    = c(3.0, 3.0),
    y    = c(3.0, 3.0),
    .col = c(2L, 3L),
    .row = c(3L, 3L),
    .id  = c(1L, 1L),
    .symbol = TRUE
  )

  line <- draw_connectors(
    df,
    list(connect = "perpendicular", position = "top"),
    sizes = c(1.0, 1.0, 1.0),
    elems = list(connector = element_line())
  )
  expect_s3_class(line, "polyline")
  expect_equal(as.numeric(line$x), c(3.0, 3.0))
  expect_equal(as.numeric(line$y), c(1.5, 0.5))

  line <- draw_connectors(
    df,
    list(connect = "perpendicular", position = "left"),
    sizes = c(1.0, 1.0, 1.0),
    elems = list(connector = element_line())
  )
  expect_equal(as.numeric(line$x), c(1.5, 2.5))
  expect_equal(as.numeric(line$y), c(3.0, 3.0))

  line <- draw_connectors(
    df,
    list(connector = NULL, position = "left"),
    sizes = c(1.0, 1.0, 1.0),
    elems = list(connector = element_line())
  )
  expect_true(is_zero(line))
})

test_that("guide_axis_symbols does input checks", {
  expect_snapshot_error(guide_axis_symbols())
})

test_that("guide_axis_upset treats input correctly", {
  expect_snapshot_error(guide_axis_upset(key = NULL))
  expect_silent(guide_axis_upset())
})

test_that("guide_axis_upset forwards key order argument", {
  g <- guide_axis_upset(c("foo", "qux", "bar"))
  key <- g$params$key(scale_x_discrete(limits = c("foo,bar", "qux,bar", "")))
  expect_identical(levels(key$.value), c("foo", "qux", "bar", "Other"))
})

test_that("guide_axis_upset gives informative error for `override.aes`", {
  override <- list(size = 5.0, colour = c("red", "blue", "green", "purple"))
  p <- ggplot() +
    scale_x_discrete(
      limits = c("foo,bar", "bar"),
      guide = guide_axis_upset(override.aes = override)
    )
  expect_snapshot_error(ggplotGrob(p))
})

test_that("guide_axis_symbols and guide_axis_upset can be drawn", {
  df <- data_frame0(
    x = c("", "A", "A,B", "B,C", "A,C"),
    y = c("X;Y", "X;Z", "Y;Z", "X;Y;Z", "")
  )

  symbol_key <- key_symbols(
    rep(1L:5L, c(1L:4L, 2L)),
    5L - c(1L, 1L, 2L, 1L, 2L, 3L, 1L, 2L, 3L, 4L, 1L, 2L),
    c(1L, 2L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 2L, 1L),
    size = 3.0
  )

  connector <- data_frame0(
    value_start = 1.0, value_end = 4.0,
    level_start = 4L, level_end = 1L,
    colour = "red"
  )

  p <- ggplot(df, aes(x, y)) +
    geom_point() +
    guides(
      x = guide_axis_upset(
        theme = theme(
          legendry.symbol = element_point(size = 3.0),
          legendry.connector = element_line(linetype = "dotted")
        )
      ),
      y = guide_axis_upset(
        key_upset(empty_label = NULL, order = c("Z", "Y", "X")),
        connect = "parallel",
        override.aes = list(shape = c(15L, 0L, 2L))
      ),
      x.sec = guide_axis_symbols(
        symbol_key,
        override.aes = list(colour = c("red", "blue")),
        connect = connector,
        theme = theme(legendry.axis.subtitle.position = "right")
      ),
      y.sec = guide_axis_symbols(
        key_symbols(df$y, level = LETTERS[seq_len(nrow(df))]),
        connect = NULL
      )
    )

  vdiffr::expect_doppelganger("upset and symbol guides", p)
})
