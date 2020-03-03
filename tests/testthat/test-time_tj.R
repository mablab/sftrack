test_that("time_tj build correctly", {
obj <- new_time_tj(time = Sys.time()+100*1:4)
expect_equal(length(unique(obj)),4)

})
