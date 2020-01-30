test_that("sf step is built correct", {
  # multiple bursts become combined
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1,1,2,2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(1:4)
  )
  my_sfstep <- suppressMessages(new_sfstep(data = df1,burst=list(id=df1$id, month = df1$month), time = df1$timez, active_burst = c('id','month')))
  expect_equal(unlist(my_sfstep$geometry), c( 27,27,-80,-80,0, 0 ,27,27,-80,-81,0,1,27,27,-82,-82,2,2,27,27,-82,-83,2,3))
})


test_that("sf traj is built correct", {
  # multiple bursts become combined
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1,1,2,2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(1:4)
  )
  my_sftraj <- new_sftraj(data = df1,burst=list(id=df1$id, month = df1$month), time = df1$timez)
  expect_equal(class(my_sftraj$geometry)[1], 'sfc_POINT')
})
