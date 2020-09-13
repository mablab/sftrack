

test_that("s_group works", {
  ind <- make_s_group(list(id = 1, month = "Jan"))
  expect_equal(names(ind), c("id", "month"))

  # burst names not duplicated
  expect_error(make_s_group(list(id = 1, id = 1)), "group names are duplicated")

  # There is an id column
  expect_error(make_s_group(list(month = "Jan", Year = 2020)), "There is no `id` column in group names")
})

test_that("c_grouping works", {
  burstz <- list(id = rep(1, 4), col1 = c(1, 1, 2, 2), col2 = c("A", "B", "B", "A"))

  obj1 <- suppressWarnings(make_c_grouping(x = burstz, active_group = "id"))
  expect_equal(sapply(obj1, function(x) x$col1), as.character(c("1", "1", "2", "2")))

  cg <- make_c_grouping(x = burstz, active_group = "id")
  expect_equal(attr(cg, "active_group"), "id")
  expect_message(make_c_grouping(x = burstz))

  # check levels
})

test_that("concatenate groups works", {
  mb1 <- make_s_group(list(id = 1, month = "Jan"))
  mb2 <- make_s_group(list(id = 1, month = "Feb"))
  comb <- c(mb1, mb1, mb2, mb2)
  expect_equal(attr(comb, "active_group"), c("id", "month"))

  burst1 <- make_c_grouping(list(id = rep(1, 4), col1 = c(1, 1, 2, 2)))
  burst2 <- make_c_grouping(list(id = rep(1, 4), col1 = c("A", "B", "B", "A")))

  comb <- c(burst1, burst2)
  expect_equal(attr(comb, "active_group"), c("id", "col1"))

  burst1 <- make_c_grouping(list(id = rep(1, 4), col1 = c(1, 1, 2, 2)))
  burst2 <- make_c_grouping(list(id = rep(1, 4), col2 = c("A", "B", "B", "A")))
  expect_error(c(burst1, burst2), "Group names do not match")
})

test_that("active group can change correctly", {
  burst1 <- make_c_grouping(list(id = rep(1, 4), col2 = c("A", "B", "B", "A")))
  expect_equal(active_group(burst1), c("id", "col2"))
  active_group(burst1) <- "id"
  expect_equal(active_group(burst1), c("id"))

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
  expect_equal(active_group(my_sftrack), c("id", "month"))
  active_group(my_sftrack) <- "id"
  expect_equal(active_group(my_sftrack), c("id"))
})

test_that("subset c_grouping", {
  burst1 <- make_c_grouping(list(id = rep(1, 4), col2 = c("A", "B", "B", "A")))
  expect_equal(class(burst1[1]), "c_grouping")

  # subsetting via label name
  expect_equal(length(burst1["1_B"]), 2)

  # replace item in a multi_burst
  burst1 <- make_c_grouping(list(id = rep(1, 5), col2 = c("A", "A", "C", "C", "A")))
  burst1[2] <- s_group(list(id = 1, col2 = "C"))
  expect_equal(class(burst1[[2]]), "s_group")
  expect_equal(as.character(attr(burst1, "sort_index")[2]), "1_C")

  # replacing an individual element in a multi_burst
  burst1[[1]]$id <- 3
  expect_equal(burst1 [[1]][[1]], "3")


  # subset to 0
  mb_attr <- attributes(burst1[F])
  expect_equal(mb_attr, list(active_group = c("id", "col2"), sort_index = factor(NULL), class = "c_grouping"))
})
