test_that("Bursts build correctly", {
  burstz <- list(col1 = c(1,1,2,2), col2 = c(1,2,1,2))
  id <- rep(1,4)
  obj1 <- ind_burst(id = id, burst = burstz)
  expect_equal(obj1$burst$col1, factor(c('1','1','2','2')))

  burstz <- NULL
  obj2 <- ind_burst(id = id, burst = burstz)
  expect_equal(obj2$burst$id, factor(c('1','1','1','1')))
})
