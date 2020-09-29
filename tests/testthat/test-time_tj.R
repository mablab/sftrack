test_that("sft_timestamp built correctly", {
  time <- list(c(1, 10), c(2, 20))
  time <- list(c(as.POSIXct("2020-01-01 10:00:00") + c(1, 100)))
  time <- 1:10

  timez <- sft_time(time)
  ans <- list(class = c("sft_timestamp", "integer"), tzone = "", type = "numeric")
  expect_equal(attributes(timez), ans)

  time <- list(c(1, 10), c(2, 20))
  timez <- sft_time(time)
  ans <- list(class = c("sft_timestamp", "list"), tzone = "", type = "numeric")
  expect_equal(attributes(timez), ans)

  time <- list(as.POSIXct("2020-01-01 10:00:00", tz = "UTC") + c(1, 100))
  timez <- sft_time(time)
  ans <- list(class = c("sft_timestamp", "list"), tzone = "UTC", type = "POSIX")
  expect_equal(attributes(timez), ans)
})


test_that("t1 and t2 work", {
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
  my_sftraj <- as_sftraj(
    data = df1, group = list(id = df1$id, month = df1$month),
    time = df1$timez, active_group = c("id", "month"), coords = df1[, c("x", "y")]
  )


  t1_ans <- c("2020-01-01 13:00:00 UTC", "2020-01-01 14:00:00 UTC", "2020-01-01 15:00:00 UTC", "2020-01-01 16:00:00 UTC")
  t2_ans <- c("2020-01-01 14:00:00 UTC", NA, "2020-01-01 16:00:00 UTC", NA)
  t1_ans <- as.POSIXct(t1_ans, tz = "UTC")
  t2_ans <- as.POSIXct(t2_ans, tz = "UTC")
  expect_equal(t1(my_sftrack), t1_ans)
  expect_equal(t2(my_sftrack), t2_ans)
  expect_equal(t1(my_sftraj), t1_ans)
  expect_equal(t2(my_sftraj), t2_ans)

  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = c(0, 1, 2, 3),
    timez = 1:4
  )
  my_sftrack <- as_sftrack(
    data = df1, group = list(id = df1$id, month = df1$month),
    time = df1$timez, active_group = c("id", "month"), coords = df1[, c("x", "y")]
  )
  my_sftraj <- as_sftraj(
    data = df1, group = list(id = df1$id, month = df1$month),
    time = df1$timez, active_group = c("id", "month"), coords = df1[, c("x", "y")]
  )
  t1_ans <- c(1, 2, 3, 4)
  t2_ans <- c(2, NA, 4, NA)
  expect_equal(t1(my_sftrack), t1_ans)
  expect_equal(t2(my_sftrack), t2_ans)
  expect_equal(t1(my_sftraj), t1_ans)
  expect_equal(t2(my_sftraj), t2_ans)

  df1 <- df1[c(1, 3, 2, 4), ]
  my_sftrack <- as_sftrack(
    data = df1, group = list(id = df1$id, month = df1$month),
    time = df1$timez, active_group = c("id", "month"), coords = df1[, c("x", "y")]
  )
  # out of order
  expect_equal(t2(my_sftrack), c(2, 4, NA, NA))
})

test_that("time recalculates correctly", {
  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1, 1, 2, 2),
    x = c(27, 27, 27, 27),
    y = c(-80, -81, -82, -83),
    z = c(0, 1, 2, 3),
    timez = 1:4
  )


  my_sftraj <- as_sftraj(
    data = df1, group = list(id = df1$id, month = df1$month),
    time = df1$timez, active_group = c("id", "month"), coords = df1[, c("x", "y")]
  )
  my_sftraj <- my_sftraj[-2, ]
  ans <- c(1, NA, 3, 4, 4, NA)

  expect_equal(unlist(time_recalc(my_sftraj)$sft_timestamp), ans)
})
