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
    time = 'timez', active_burst = c('id','month'), coords = c('x','y','z')))
  expect_equal(unlist(my_sftraj$geometry), c( 27,27,-80,-81,0,1,27,-81,1,27,27,-82,-83,2,3,27,-83,3))

  my_sftraj <- as_sftrack(data = df1,burst=list(id=df1$id, month = df1$month),
    time = df1$timez, active_burst = c('id','month'), coords = df1[,c('x','y')])
  expect_equal(colnames(my_sftraj),c('id','month','x','y','z','timez','reloc_time','burst','geometry'))
  expect_equal(class(my_sftraj$geometry)[1], 'sfc_POINT')

  # vector mode with data

  my_sftraj <- as_sftrack(data = df1,burst=c('id','month'),
    time = 'timez', active_burst = c('id','month'), coords = c('x','y'))
  expect_equal(colnames(my_sftraj),c('id','month','x','y','z','timez','burst','geometry'))

  # data.frame mode without data; accepts null
  my_sftraj <- as_sftrack(burst=list(id=df1$id, month = df1$month),
    time = df1$timez, active_burst = c('id','month'), coords = df1[,c('x','y')])
  expect_equal(colnames(my_sftraj),c('sftrack_id','reloc_time','burst','geometry'))

  # check 2 dimensions
  expect_equal(class(my_sftraj$geometry[[1]])[1], 'XY')

  # test that sftrack can change active_bursts
  expect_equal(attr(my_sftraj$burst,'active_burst'), c('id','month'))
  my_sftraj <- suppressMessages( as_sftrack(burst=list(id=df1$id, month = df1$month),
    time = df1$timez, active_burst = c('id'), coords = df1[,c('x','y','z')]))
  expect_equal(attr(my_sftraj$burst,'active_burst'), c('id'))

  # test that dimensions are created equally
  expect_equal(class(my_sftraj$geometry[[1]])[1], 'XYZ')


})



