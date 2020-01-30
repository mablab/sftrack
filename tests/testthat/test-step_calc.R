test_that("step geometry calculates correctly", {
  df1 <- data.frame(
    id = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(1:4)
  )
  geom_ans <- c( 27,27,-80,-80,0, 0 ,27,27,-80,-81,0,1,27,27,-82,-82,2,2,27,27,-82,-83,2,3)
  my_traj <- new_sftraj(data = df1,burst=list(id=df1$id), time = df1$timez)

  my_step_geom <- make_step_geom(burst_id = my_traj$burst, geometry = my_traj$geometry, timez = my_traj$time)
  # order was done correctly

  expect_equal(unlist(my_step_geom), geom_ans)
  # object is an sf line string
  expect_equal(class(my_step_geom)[1], 'sfc_LINESTRING')

  # multiple bursts become combined
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1,1,2,2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(1:4)
  )
  my_traj <- new_sftraj(data = df1,burst=list(id=df1$id, month = df1$month), time = df1$timez)

  my_step_geom <- suppressMessages(make_step_geom(burst_id = my_traj$burst, geometry = my_traj$geometry, timez = my_traj$time))

  expect_equal(unlist(my_step_geom), geom_ans)
})

