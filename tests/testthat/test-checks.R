test_that("checks check successfully", {

  # columns not found in data set
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = 0:3,
    timez = Sys.time() + 60 * 60 * (c(1, 2, 3, 4))
  )
  expect_error(as_sftrack(
    data = df1, group = list(id = df1$id, month = df1$month),
    time_col = "nottime", active_group = c("id", "month"), coords = c("x", "y", "z")
  ))

  # Z coordinates
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = 0:3,
    timez = Sys.time() + 60 * 60 * (1:4)
  )
  expect_message(
    as_sftrack(
      data = df1, group = list(id = df1$id, month = df1$month),
      time = "timez", active_group = c("id", "month"), coords = c("x", "y", "z")
    ),
    "Z coordinates found. Note that the vast majority of rgdal calculations are done using 2D geometry"
  )

  # duplicate time_stamps
  df2 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c("march", "march", "april", "april"),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    timez = Sys.time() + 60 * 60 * c(1, 1, 2, 3)
  )
  expect_error(
    as_sftrack(
      data = df2, group = c("id", "month"),
      time = "timez", coords = c("x", "y")
    ),
    "groups: 1_march have duplicated time stamps"
  )

  # NAs in some coordinate columns
  df3 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = c(NA, NA, 1, 0),
    timez = Sys.time() + 60 * 60 * (1:4)
  )
  expect_error(
    as_sftrack(
      data = df3, group = c("id", "month"),
      time = "timez", coords = c("x", "y", "z")
    ),
    "z column has NAs that are not found in other coordinate columns"
  )

  # check coord names exist
  expect_error(
    as_sftrack(
      data = df1, group = c("id", "month"),
      time = "timez", coords = c("x", "y", "z1")
    ),
    "z1 not found in data frame"
  )

  # data not ordered
  df4 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    timez = Sys.time() + 60 * 60 * c(4, 1, 2, 3)
  )
  expect_message(
    as_sftrack(
      data = df4, group = c("id", "month"),
      time = "timez", coords = c("x", "y")
    ),
    "time was not ordered for each burst"
  )
})
