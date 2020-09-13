test_that("can identify duplicate time stamps", {
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = c(0, 1, 2, 3),
    timez = as.POSIXct("2020-01-01 12:00:00", tz = "UTC") + 60 * 60 * (1:4)
  )
  df1$timez[1] <- df1$timez[2]
  expect_equal(which_duplicated(data = df1, group = c("id"), time = "timez")$which_row, c(1, 2))
})
