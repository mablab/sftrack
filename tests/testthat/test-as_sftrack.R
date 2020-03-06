test_that("sftrack is built correct", {
  # multiple bursts become combined
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1,1,2,2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(1:4)
  )
  my_sftrack <- as_sftrack(data = df1,burst_list=list(id=df1$id, month = df1$month),
    time_col = 'timez', active_burst = c('id','month'), coords = c('x','y','z'))
  expect_equal(class(my_sftrack$geometry)[1], 'sfc_POINT')
})
