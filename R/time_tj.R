
# df1 <- data.frame(
#   id = rep(1,10),
#   time = 1:10,
#   weather = c('clear','windy','clear','windy','clear','clear','rainy','clear','cloudy','cloudy'),
#   x = c(0,1,1,1,NA,2,3,1,2,2),
#   y = c(0,0,1,1,NA,0,1,1,2,2)
# )
#

sft_time <- function(x = list()) {

  if(length(x[[1]])==2){namez <- c('start','end') }else{namez <- 'start'}

 timez <-  lapply(x, function(y){
    names(y) <- namez
    y
  })
 tz <- if(is.numeric(x[[1]])) NA else attr(x[[1]],'tzone')
  structure(
    I(timez),
    tz = tz,
    class = c('sft_timestamp',class(x))
  )
}

# sft_time(list(c(1,2), c(2,3)))
# x = sft_time(list(c(1,2), c(2,3)))

as.data.frame.sft_timestamp <- function(x, ...) {
  ret <- data.frame(row.names = seq_along(x))
  ret[['sft_timestamp']] <- I(x)
  ret
}

c.sft_timestamp <- function(..., recursive = FALSE){
lst = list(...)
ret = unlist(lapply(lst, unclass), recursive = FALSE)
sft_time(ret)
}

'$<-.sft_timestamp' <- function(x, i, value) {
  if (is.null(value) || inherits(value, "sft_timestamp")) {
    value <- I(value)
  }
  NextMethod()
}
# df1$timestamp <- timez
# df1$timestamp

t1 <- function(x) {
  UseMethod("t1")
}

t1.sftrack <- function(x){
  vapply(x[[attr(x,'time_col')]], function(y)y[1], numeric(1))
}

t1.sftraj <- function(x){
  vapply(x[[attr(x,'time_col')]], function(y)y[1], numeric(1))
}

t1.sft_timestamp <- function(x){
  vapply(x, function(y)y[1], numeric(1))
}

t2 <- function(x) {
  UseMethod("t2")
}

t2.sftrack <- function(x){
  c(x[[attr(x,'time_col')]][-1],NA)
}

t2.sftraj <- function(x){
  vapply(x[[attr(x,'time_col')]], function(y)y[2], numeric(1))
}

t2.sft_timestamp <- function(x){
  vapply(x, function(y)y[2], numeric(1))
}
