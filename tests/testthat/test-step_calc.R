test_that("step geometry calculates correctly", {
  df1 <- data.frame(
    id = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(1:4)
  )
  geom_ans <- c(27,  27, -80, -81,  27, -81,  27,  27, -82, -83,  27, -83)
  my_track <- as_sftrack(data = df1, burst = 'id', time = 'timez', coords = c('x','y'))

  my_step_geom <- make_step_geom(burst = my_track$burst, geometry = my_track$geometry, time_data =my_track$timez)
  # order was done correctly

  expect_equal(unlist(my_step_geom), geom_ans)
  # object is an sf line string
  expect_equal(class(my_step_geom)[1], c('sfc_GEOMETRY'))

  # multiple bursts become combined
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1,1,2,2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(1:4)
  )
  my_track <- as_sftrack(data = df1,burst = c('id','month'), time = 'timez', coords = c('x','y'))

  my_step_geom <- suppressMessages(make_step_geom(burst = my_track$burst, geometry = my_track$geometry, time_data = my_track$time))

  expect_equal(unlist(my_step_geom), geom_ans)

  #
})

test_that('step metrics works',{
df1 <- data.frame(
  id = c(1, 1, 1, 1,1,1),
  month = c(1,1,1,1,1,1),
  x = c(1, 1, 1, NA,2,3),
  y = c(0,1,2,NA, 3,3),
  z = c(0,1,2,3,4,5),
  timez = as.POSIXct('2020-01-01 12:00:00', tz = 'UTC') + 60*60*(1:6)
)
my_sftraj <- as_sftraj(data = df1,burst=list(id=df1$id, month = df1$month),
  time = df1$timez, active_burst = c('id','month'), coords = df1[,c('x','y')])
sm <- step_metrics(my_sftraj)
ans <- data.frame(
  dx = c(0,0,NA,NA,1,NA),
  dy = c(1,1,NA,NA,0,NA),
  dist = c(1,1,NA,NA,1,NA),
  dt = c(3600,3600,3600,3600,3600,NA),
  abs_angle = c(1.570796,1.570796,NA,NA,0,NA)
)
ans$speed <- ifelse(is.na(ans$dist),NA,ans$dist/ans$dt)

expect_equivalent(ans, sm[,1:6], tolerance = 1e-06)
})
