test_that("checks check successfully", {

  #duplicated time stamps
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1,1,2,2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(c(1,1,3,4))
  )
  expect_error( as_sftrack(data = df1,burst=list(id=df1$id, month = df1$month),
    time_col = 'timez', active_burst = c('id','month'), coords = c('x','y','z')))

  #columns not found in data set
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1,1,2,2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(c(1,2,3,4))
  )
  expect_error( as_sftrack(data = df1,burst=list(id=df1$id, month = df1$month),
    time_col = 'nottime', active_burst = c('id','month'), coords = c('x','y','z')))

  #NAs must be through whole row
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1,1,2,2),
    x = c(27, 27, NA, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(c(1,2,3,4))
  )
  expect_error( as_sftrack(data = df1,burst_list=list(id=df1$id, month = df1$month),
    time_col = 'timez', active_burst = c('id','month'), coords = c('x','y','z')), 'x column has NAs that are not found in other coordinate columns')
})
