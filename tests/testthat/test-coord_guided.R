test_that("coord_guided appropriately rejects incorrect guides", {

  p <- ggplot(mtcars, aes(disp, mpg)) + geom_point() + coord_guided()

  expect_snapshot_warning(ggplotGrob(
    p + guides(grid = "foobar")
  ))

  expect_snapshot_warning(ggplotGrob(
    p + guides(grid = "legend")
  ))

})

test_that("coord_guided can use appropriate guides", {

  p <- ggplot(mtcars, aes(disp, mpg)) + geom_point() + theme_gray()

  vdiffr::expect_doppelganger(
    "coord_guided with grid without major grid",
    p + coord_guided(guide = guide_grid(breaks = NULL))
  )

  vdiffr::expect_doppelganger(
    "coord_guided with guide_none()",
    p + coord_guided(guide = "none")
  )
})
