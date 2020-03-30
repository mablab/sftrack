test_that("Bursts build correctly", {
  burstz <- list(id = rep(1,4),col1 = c(1,1,2,2), col2 = c(1,2,1,2))

  obj1 <- make_ind_burst(burst = burstz)
  expect_equal(obj1$col1, as.character(c('1','1','2','2')))


})
#
# test_that("testing how burst defines levels",{
#
#   burstz <- list(id = rep(1,4),month = c(1,1,2,2))
#   # multi burst change in levels
#   df2 <- suppressMessages(make_multi_burst(burst_list=burstz,  burst_levels = list('id'=1:5,'month' = 0:11)))
#   expect_equal(attr(df2,'burst_levels')$month, paste0(0:11))
#   # multi burst no change
#   df3 <- suppressMessages(make_multi_burst(burst_list=burstz))
#   expect_equal(attr(df3,'burst_levels')$id, '1')
#   expect_equal(attr(df3,'burst_levels')$month, c('1','2'))
# })
