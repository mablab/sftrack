test_that("Bursts build correctly", {
  burstz <- list(id = rep(1,4),col1 = c(1,1,2,2), col2 = c(1,2,1,2))

  obj1 <- make_ind_burst(burst = burstz)
  expect_equal(obj1$col1, factor(c('1','1','2','2')))


})

test_that("testing how burst defines levels",{

  burstz <- list(id = rep(1,4),month = c(1,1,2,2))
  # individual burst change in levels
  df1 <- make_ind_burst(burst=list(id=1,month=3, height=10), new_levels = list('month' = 0:11))
  expect_equal(levels(df1$month[1]), paste0(0:11))
  # multi burst change in levels
  df2 <- suppressMessages(make_multi_burst(burstz, new_levels = list(month = 0:11)))
  expect_equal(levels(df2[[1]]$month), paste0(0:11))
  # multi burst no change
  df3 <- suppressMessages(make_multi_burst(burstz))
  expect_equal(levels(df3[[1]]$id), '1')
  expect_equal(levels(df3[[1]]$month), c('1','2'))
})
