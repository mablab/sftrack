test_that("step geometry calculates correctly", {
  df1 <- data.frame(
    id = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = 0:3,
    timez = Sys.time() + 60*60*(1:4)
  )
  geom_ans <- c( 27,27,-80,-81,0,1,27,-81,1,NA,NA,NA,27,27,-82,-83,2,3,27,-83,3,NA,NA,NA)
  my_track <- as_sftrack(data = df1,burst=list(id=df1$id), time = df1$timez, coords = c('x','y','z'))

  my_step_geom <- make_step_geom(burst_id = my_track$burst, geometry = my_track$geometry, timez = my_track$time)
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
  my_track <- as_sftrack(data = df1,burst=list(id=df1$id, month = df1$month), time = df1$timez, coords = c('x','y','z'))

  my_step_geom <- suppressMessages(make_step_geom(burst_id = my_track$burst, geometry = my_track$geometry, timez = my_track$time))

  expect_equal(unlist(my_step_geom), geom_ans)

  #
})

