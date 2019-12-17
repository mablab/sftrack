test_that("time_tj build correctly", {
obj <- new_time_tj(time = Sys.time()+100*1:4, id=1:4)
expect_equal(length(unique(obj)),4)

expect_error(new_time_tj(time = rep(Sys.time(),2), id=c('animal1','animal1')),
  'time is not unique for individuals: animal1')
})
