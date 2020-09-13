test_that("sftrack is built correct", {
  # data.frame mode works successfully
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = c(0, 1, 2, 3),
    timez = as.POSIXct("2020-01-01 12:00:00", tz = "UTC") + 60 * 60 * (1:4)
  )
  my_sftrack <- as_sftrack(
    data = df1, group = list(id = df1$id, month = df1$month),
    time = df1$timez, active_group = c("id", "month"), coords = df1[, c("x", "y")]
  )
  expect_equal(colnames(my_sftrack), c("id", "month", "x", "y", "z", "timez", "sft_timestamp", "sft_group", "geometry"))
  expect_equal(class(my_sftrack$geometry)[1], "sfc_POINT")

  # vector mode with data

  my_sftrack <- as_sftrack(
    data = df1, group = c("id", "month"),
    time = "timez", active_group = c("id", "month"), coords = c("x", "y")
  )
  expect_equal(colnames(my_sftrack), c("id", "month", "x", "y", "z", "timez", "sft_group", "geometry"))

  # data.frame mode without data; accepts null
  my_sftrack <- as_sftrack(
    group = list(id = df1$id, month = df1$month),
    time = df1$timez, active_group = c("id", "month"), coords = df1[, c("x", "y")]
  )
  expect_equal(colnames(my_sftrack), c("sftrack_id", "sft_timestamp", "sft_group", "geometry"))

  # check 2 dimensions
  expect_equal(class(my_sftrack$geometry[[1]])[1], "XY")

  # test that sftrack can change active_bursts
  expect_equal(attr(my_sftrack$sft_group, "active_group"), c("id", "month"))
  my_sftrack <- suppressMessages(as_sftrack(
    group = list(id = df1$id, month = df1$month),
    time = df1$timez, active_group = c("id"), coords = df1[, c("x", "y", "z")]
  ))
  expect_equal(attr(my_sftrack$sft_group, "active_group"), c("id"))

  # test that dimensions are created equally
  expect_equal(class(my_sftrack$geometry[[1]])[1], "XYZ")

  # test that a column name can be burst
  df1 <- data.frame(
    burst = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = c(0, 1, 2, 3),
    timez = as.POSIXct("2020-01-01 12:00:00", tz = "UTC") + 60 * 60 * (1:4)
  )
  my_sftrack <- as_sftrack(
    data = df1, group = c(id = "burst"),
    time = df1$timez, active_group = c("id"), coords = df1[, c("x", "y")]
  )

  expect_equal(colnames(my_sftrack), c("burst", "month", "x", "y", "z", "timez", "sft_timestamp", "sft_group", "geometry"))
})

test_that("as_sftrack and sftraj convert back and forth successfully", {
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = c(0, 1, 2, 3),
    timez = as.POSIXct("2020-01-01 12:00:00", tz = "UTC") + 60 * 60 * (1:4)
  )
  my_sftrack <- as_sftrack(
    data = df1, group = c("id", "month"),
    time = "timez", active_group = c("id", "month"), coords = c("x", "y")
  )
  my_sftraj <- as_sftraj(
    data = df1, group = c("id", "month"),
    time = "timez", active_group = c("id", "month"), coords = c("x", "y")
  )
  new_sftraj <- as_sftraj(my_sftrack)
  expect_equal(new_sftraj, my_sftraj)
  new_sftrack <- as_sftrack(my_sftraj)
  expect_equal(new_sftrack, my_sftrack)
})

test_that("input as sf successfully", {
  # Input is a sf object

  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = c(0, 1, 2, 3),
    timez = as.POSIXct("2020-01-01 12:00:00", tz = "UTC") + 60 * 60 * (1:4)
  )

  sf_df <- st_as_sf(df1, coords = c("x", "y"))
  new_sftrack <- as_sftrack(data = sf_df, group = "id", time = "timez")

  # Not include sfc_point
  sf_df <- st_as_sf(df1, coords = c("x", "y"))
  sf_df$geometry <- st_sfc(list(st_multipoint(), st_multipoint(), st_multipoint(), st_multipoint()))

  expect_error(as_sftrack(data = sf_df, group = "id", time = "timez"))
})


test_that("subset works correctly", {
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = c(0, 1, 2, 3),
    timez = as.POSIXct("2020-01-01 12:00:00", tz = "UTC") + 60 * 60 * (1:4)
  )
  # retains sftrack class
  my_sftrack <- as_sftrack(
    data = df1, group = c("id", "month"),
    time = "timez", active_group = c("id", "month"), coords = c("x", "y")
  )
  expect_equal(class(my_sftrack[1:3, ]), c("sftrack", "sf", "data.frame"))

  expect_equal(ncol(my_sftrack[, 3, ]), 4)

  expect_silent(my_sftrack[, 3, drop = T])

  expect_equal(nrow(my_sftrack[, 3, drop = T]), NULL)

  # subset by colname without dropped columns
  expect_equal(colnames(my_sftrack[, c("id", "month")]), c("id", "month", "sft_group", "timez", "geometry"))

  # rbind
  df2 <- df1
  df2$timez <- df2$timez + 10
  my_sftrack2 <- as_sftrack(
    data = df2, group = c("id", "month"),
    time = "timez", active_group = c("id", "month"), coords = c("x", "y")
  )
  my_sftrack3 <- rbind(my_sftrack, my_sftrack2)
  expect_equal(class(my_sftrack3)[1], "sftrack")

  expect_equal(nrow(my_sftrack3), 8)

  # change active burst
  active_group(my_sftrack2) <- "id"
  expect_equal(attr(my_sftrack2$sft_group, "active_group"), "id")
})
