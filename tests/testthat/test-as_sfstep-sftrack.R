test_that("sftraj is built correct", {
  # multiple bursts become combined
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1,1,2,2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(1:4)
  )
  my_sftraj <- suppressMessages(as_sftraj(data = df1,burst=list(id=df1$id, month = df1$month),
    time = df1$timez, active_burst = c('id','month'), coords = c('x','y','z')))
  expect_equal(unlist(my_sftraj$geometry), c( 27,27,-80,-81,0,1,27,-81,1,NA,NA,NA,27,27,-82,-83,2,3,27,-83,3,NA,NA,NA))

  # test that sf_step can change active_bursts
  expect_equal(attr(my_sftraj,'active_burst'), c('id','month'))
  my_sftraj <- suppressMessages(as_sftraj(data = df1,burst=list(id=df1$id, month = df1$month),
    time = df1$timez, active_burst = c('id'), coords = c('x','y','z')))
  expect_equal(attr(my_sftraj,'active_burst'), c('id'))
})


test_that("sf track is built correct", {
  # multiple bursts become combined
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1,1,2,2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(1:4)
  )
  my_sftrack <- as_sftrack(data = df1,burst=list(id=df1$id, month = df1$month),
    time = df1$timez, active_burst = c('id','month'), coords = c('x','y','z'))
  expect_equal(class(my_sftrack$geometry)[1], 'sfc_POINT')
})
