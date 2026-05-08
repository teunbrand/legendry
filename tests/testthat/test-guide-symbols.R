test_that("connectors works", {

  expect_silent(check_connect_arg("perpendicular"))
  expect_snapshot_error(
    check_connect_arg(data.frame(foo = 1:2))
  )


  df <- data.frame(
    x    = c(3, 3),
    y    = c(3, 3),
    .col = c(2, 3),
    .row = c(3, 3),
    .id  = c(1, 1),
    .symbol = TRUE
  )

  line <- draw_connectors(
    df,
    list(connect = "perpendicular", position = "top"),
    sizes = c(1, 1, 1),
    elems = list(connector = element_line())
  )
  expect_s3_class(line, 'polyline')
  expect_equal(as.numeric(line$x), c(3, 3))
  expect_equal(as.numeric(line$y), c(1.5, 0.5))

  line <- draw_connectors(
    df,
    list(connect = "perpendicular", position = "left"),
    sizes = c(1, 1, 1),
    elems = list(connector = element_line())
  )
  expect_equal(as.numeric(line$x), c(1.5, 2.5))
  expect_equal(as.numeric(line$y), c(3, 3))

  line <- draw_connectors(
    df,
    list(connector = NULL, position = "left"),
    sizes = c(1, 1, 1),
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
  expect_equal(levels(key$.value), c("foo", "qux", "bar", "Other"))
})

test_that("guide_axis_upset gives informative error for `override.aes`", {
  p <- ggplot() +
    scale_x_discrete(
      limits = c("foo,bar", "bar"),
      guide = guide_axis_upset(override.aes = list(size = 5, colour = c("red", "blue", "green", "purple")))
    )
  expect_snapshot_error(ggplotGrob(p))
})

test_that("guide_axis_symbols and guide_axis_upset can be drawn", {
  df <- data.frame(
    x = c("", "A", "A,B", "B,C", "A,C"),
    y = c("X;Y", "X;Z", "Y;Z", "X;Y;Z", "")
  )

  symbol_key <- key_symbols(
    rep(1:5, c(1:4, 2)),
    5 - c(1, 1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2),
    c(1, 2, 1, 2, 2, 1, 1, 2, 2, 1, 2, 1),
    size = 3
  )

  connector <- data_frame0(
    value_start = 1, value_end = 4,
    level_start = 4, level_end = 1,
    colour = 'red'
  )

  p <- ggplot(df, aes(x, y)) +
    geom_point() +
    guides(
      x = guide_axis_upset(
        theme = theme(
          legendry.symbol = element_point(size = 3),
          legendry.connector = element_line(linetype = "dotted")
        )
      ),
      y = guide_axis_upset(
        key_upset(empty_label = NULL, order = c("Z", "Y", "X")),
        connect = "parallel",
        override.aes = list(shape = c(15, 0, 2))
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
