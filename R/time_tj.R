
# df1 <- data.frame(
#   id = rep(1,10),
#   time = 1:10,
#   weather = c('clear','windy','clear','windy','clear','clear','rainy','clear','cloudy','cloudy'),
#   x = c(0,1,1,1,NA,2,3,1,2,2),
#   y = c(0,0,1,1,NA,0,1,1,2,2)
# )
#
#' @export
sft_time <- function(x = list()) {

  class(x) <- setdiff(class(x),'sft_timestamp')
  # check for null
  if(length(x)==0){return(structure(list(),
                                    tzone = NA,
                                    type = 'numeric',
                                    class = c('sft_timestamp',class(x))
  )
  )
  }

  # if(!all(c('start','end')%in%names(x[[1]]))){
  #   if(length(x[[1]])==2){namez <- c('start','end') }else{namez <- 'start'}
  #   timez <-  lapply(x, function(y){
  #     names(y) <- namez
  #     y
  #   })
  # } else{
  #   timez = x}
  type = if(inherits(x[[1]][[1]], 'POSIXct')){'POSIX'}else{'numeric'}
  tz <- if(type == 'POSIX'){ attr(x[[1]][[1]],'tzone')}else { NA }
  ret = structure(
    I(x),
    tzone = tz,
    type = type,
    class = c('sft_timestamp',class(x))
  )
  ret
}


#' @export
as.data.frame.sft_timestamp <- function(x, ...) {
  ret <- data.frame(row.names = seq_along(x))
  ret[['sft_timestamp']] <- I(x)
  ret
}

#' @export
c.sft_timestamp <- function(..., recursive = FALSE){
lst = list(...)
ret = unlist(lapply(lst, unclass), recursive = FALSE)
sft_time(ret)
}

#' @export
'[.sft_timestamp' <- function(x,i,j,...){
  sft_time(NextMethod())
}

#' @export
'$<-.sft_timestamp' <- function(x, i, value) {
  if (is.null(value) || inherits(value, "sft_timestamp")) {
    value <- I(value)
  }
  NextMethod()
}
# df1$timestamp <- timez
# df1$timestamp

#' @export
t1 <- function(x) {
  UseMethod("t1")
}

#' @export
#' @method t1 sftrack
t1.sftrack <- function(x){
  timez <- x[[attr(x,'time_col')]]
  ret <- vapply(x[[attr(x,'time_col')]], function(y)y[[1]], numeric(1))
  if(inherits(timez[[1]],'POSIXct')){
    tzone <- attr(timez, 'tzone')
  ret <- as.POSIXct(ret, origin = '1970-01-01 00:00.00 UTC', tz = tzone)
  }
  ret

}

#' @export
#' @method t1 sftraj
t1.sftraj <- function(x){
  timez <- x[[attr(x,'time_col')]]
  ret <- vapply(x[[attr(x,'time_col')]], function(y)y[[1]], numeric(1))
  if(attr(timez,'type') == 'POSIX'){
    tzone <- attr(timez, 'tzone')
    ret <- as.POSIXct(ret, origin = '1970-01-01 00:00.00 UTC', tz = tzone)
  }
  ret
}

#' @export
#' @method t1 sft_timestamp
t1.sft_timestamp <- function(x){
  ret <- vapply(x, function(y)y[[1]], numeric(1))
  if(attr(x,'type') == 'POSIX'){
    tzone <- attr(x, 'tzone')
    ret <- as.POSIXct(ret, origin = '1970-01-01 00:00.00 UTC', tz = tzone)
  }
  ret
}

#' @export
t2 <- function(x) {
  UseMethod("t2")
}

#' @export
#' @method t2 sftrack
t2.sftrack <- function(x){
  timez <- x[[attr(x,'time_col')]]
  c(timez[-1],NA)
  ret <- c(unlist(timez[-1]),NA)
  if(attr(timez,'type') == 'POSIX'){
    tzone <- attr(timez, 'tzone')
    ret <- as.POSIXct(ret, origin = '1970-01-01 00:00.00 UTC', tz = tzone)
  }
  ret
}

#' @export
#' @method t2 sftraj
t2.sftraj <- function(x){
  timez <- x[[attr(x,'time_col')]]
  ret <- vapply(x[[attr(x,'time_col')]], function(y)y[[2]], numeric(1))
  if(attr(timez,'type') == 'POSIX'){
    tzone <- attr(timez, 'tzone')
    ret <- as.POSIXct(ret, origin = '1970-01-01 00:00.00 UTC', tz = tzone)
  }
  ret
}

#' @export
#' @method t2 sft_timestamp
t2.sft_timestamp <- function(x){
  if(length(x[[1]])==2){
  ret <- vapply(x, function(y)y[[2]], numeric(1))
  } else {
    ret <- c(unlist(x[-1]),NA)
  }

  if(attr(x,'type') == 'POSIX'){
    tzone <- attr(x, 'tzone')
    ret <- as.POSIXct(ret, origin = '1970-01-01 00:00.00 UTC', tz = tzone)
  }
  ret
}

print.sft_timestamp <- function(x,...){
  NextMethod(tz = attr(x,'tzone'))
}

#' @export
c.sft_timestamp <- function(..., recursive = FALSE) {
  # x <- list(my_sftraj$timez, my_sftraj2$timez)
  x <- list(...)
  tz_test <- lapply(x[!is.na(x)], function(y)attr(y,'tzone'))
  if(length(unique(tz_test[!is.na(tz_test)]))>1) {
    stop('times are not in the same timezone \n Please convert them to the same timezone and try again')
  }
  x <- unlist(x, recursive = FALSE)
 sft_time(x)
}

missing_next_pt <- function(x){
  #  x <- my_sftraj
  gl <- group_labels(x)
  ret <- which(t1(x)[-1]  != t2(x)[-nrow(x)] & gl[-1] == gl[-length(gl)] )
  if(length(ret)==0){ret <- NA}
  ret
}

make_time_list <- function(t1,t2){
  mapply(function(x,y){c(I(x), I(y))},t1, t2, SIMPLIFY = FALSE)
}
