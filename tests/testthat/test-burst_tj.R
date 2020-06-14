

test_that('ind_burst works',{
  ind <- make_ind_burst(list(id = 1,month = 'Jan'))
  expect_equal(names(ind),c('id','month'))

  #labels made correctly
  expect_equal(attr(ind,'label'), '1_Jan')

  # burst names not duplicated
  expect_error(make_ind_burst(list(id=1, id = 1)), 'burst names are duplicated')

  # There is an id column
  expect_error(make_ind_burst(list(month = 'Jan', Year = 2020)), 'There is no `id` column in burst names')

})

test_that('multi_burst works', {
  burstz <- list(id = rep(1,4),col1 = c(1,1,2,2), col2 = c('A','B','B','A'))

  obj1 <- suppressWarnings(make_multi_burst(x = burstz, active_burst = 'id'))
  expect_equal(sapply(obj1,function(x) x$col1), as.character(c('1','1','2','2')))

  mb <- make_multi_burst(x = burstz,active_burst = 'id')
  expect_equal(attr(mb[[1]],'active_burst'), 'id')

  expect_warning(make_multi_burst(x = burstz))

  # check levels

})

test_that('concatenate bursts',{
  mb1 <- make_ind_burst(list(id=1,month='Jan'))
  mb2 <- make_ind_burst(list(id=1,month='Feb'))
  comb <- c(mb1, mb1,mb2,mb2)
  expect_equal(attr(comb,'active_burst'),c('id', 'month'))

  burst1 <- make_multi_burst(list(id = rep(1,4),col1 = c(1,1,2,2)))
  burst2 <- make_multi_burst(list(id = rep(1,4), col1 = c('A','B','B','A')))

  comb <- c(burst1, burst2)
  expect_equal(attr(comb, 'active_burst'),c('id','col1'))

  burst1 <- make_multi_burst(list(id = rep(1,4),col1 = c(1,1,2,2)))
  burst2 <- make_multi_burst(list(id = rep(1,4), col2 = c('A','B','B','A')))
  expect_error(c(burst1, burst2), 'There are more than one possible active bursts')
})

test_that('active bursts can change correctly',{
  burst1 <- make_multi_burst(list(id = rep(1,4), col2 = c('A','B','B','A')))
  expect_equal(active_burst(burst1),c('id','col2'))
  active_burst(burst1)    <- 'id'
  expect_equal(active_burst(burst1),c('id'))

  df1 <- data.frame(
    id = c(1, 1, 1, 1),
    month = c(1,1,2,2),
    x = c(27, 27, 27, 27),
    y = c(-80,-81,-82,-83),
    z = c(0,1,2,3),
    timez = as.POSIXct('2020-01-01 12:00:00', tz = 'UTC') + 60*60*(1:4)
  )
  my_sftrack <- as_sftrack(data = df1,burst=c('id','month'),
    time = 'timez', active_burst = c('id','month'), coords = c('x','y'))
  expect_equal(active_burst(my_sftrack),c('id','month'))
  active_burst(my_sftrack) <- 'id'
  expect_equal(active_burst(my_sftrack),c('id'))


})

test_that('subset multi_bursts',{
  burst1 <- make_multi_burst(list(id = rep(1,4), col2 = c('A','B','B','A')))
  expect_equal(class(burst1[1]),'multi_burst')

  # subsetting via label name
 expect_equal(length(burst1['1_B']),2)

 # replace item in a multi_burst
 burst1 <- make_multi_burst(list(id = rep(1,5), col2 = c('A','A','C','C','A')))
 burst1[2] <- ind_burst(list(id = 1, col2 = 'C'))
 expect_equal(class(burst1[[2]]),'ind_burst')
 expect_equal(attr(burst1[[2]],'label'),'1_C')

 #replacing an individual element in an ind burst
 ib <- make_ind_burst(list(id = 1,month='Jan'))
 ib$id <- 2
 expect_equal(attr(ib, 'label'),'2_Jan')

 #replacing an individual element in a multi_burst
 burst1[[1]]$id <- 3
 expect_equal(burst1 [[1]][[1]],'3')

 ib <- make_ind_burst(list(id='test', month = 'Jan'))
 active_burst(ib) <- 'id'
 expect_equal(attr(ib, 'active_burst'), 'id')
 expect_equal(attr(ib,'label'), 'test')
})
