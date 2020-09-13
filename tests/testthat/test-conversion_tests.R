test_that("sftraj conversions", {
  # multiple bursts become combined
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = 0:3,
    timez = Sys.time() + 60 * 60 * (1:4)
  )
  my_sftraj <- suppressMessages(as_sftraj(
    data = df1, group = c("id", "month"),
    time = "timez", coords = c("x", "y", "z")
  ))
  #  my_sftraj
  # sftraj -> sftrack -> sftraj
  new_sftrack <- as_sftrack(my_sftraj)
  # new_sftrack
  conv_back_sftraj <- as_sftraj(new_sftrack)
  expect_equal(my_sftraj, conv_back_sftraj)
})


test_that("sftrack conversions", {
  # multiple bursts become combined
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = 0:3,
    timez = Sys.time() + 60 * 60 * (1:4)
  )
  my_sftrack <- suppressMessages(as_sftrack(
    data = df1, group = c("id", "month"),
    time = "timez", coords = c("x", "y", "z")
  ))
  # my_sftrack
  # sftrack -> sftraj -> sftrack
  new_sftraj <- as_sftraj(my_sftrack)
  # new_sftraj
  conv_back_sftrack <- as_sftrack(new_sftraj)
  expect_equal(my_sftrack, conv_back_sftrack)

  # check coordinate dimensions get carried over
  my_sftrack_2d <- suppressMessages(suppressMessages(as_sftrack(
    data = df1, group = c("id", "month"),
    time = "timez", coords = c("x", "y")
  )))
  new_sftraj <- as_sftraj(my_sftrack_2d)
  conv_back_sftrack_2d <- as_sftrack(new_sftraj)
  expect_equal(class(conv_back_sftrack_2d$geometry[[1]])[1], "XY")
})


# test_that("from ltraj conversions", {
#   # multiple bursts become combined
#   df1 <- data.frame(
#     id = c(1, 1, 1, 1),
#     month = c(1,1,2,2),
#     year = c(2020,2020,2020,2020),
#     x = c(27, 27, 27, 27),
#     y = c(-80,-81,-82,-83),
#     z = 0:3,
#     timez = Sys.time() + 60*60*(1:4)
#   )
#
#   ltraj_df <- as.ltraj(xy=df1[,c('x','y')], date = df1$timez,
#    id = df1$id, typeII = TRUE,
#    infolocs = df1[,3:4] )
#   as_sftrack(data = ltraj_df)
#   my_sftrack <- suppressMessages(as_sftrack(data = df1,burst=list(id=df1$id, month = df1$month),
#     time = df1$timez, active_burst = c('id','month'), coords = c('x','y','z')))
#   my_sftrack
#   # sftrack -> sftraj -> sftrack
#   new_sftraj <- as_sftraj(my_sftrack)
#   new_sftraj
#   conv_back_sftrack <- as_sftrack(new_sftraj)
#   all.equal(my_sftrack,conv_back_sftrack)
# })
