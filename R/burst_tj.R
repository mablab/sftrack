#' Burst Class
#'
#' This class holds bursts, which is a place-holder name
#' that describes any grouping variable to split the data points, in which only the animal ID
#' is required, and otherwise can have n number of other grouping names.
#' Must be a named list
#'
#' ind_burst()
#' multi_burst()
#' make_bulti_burst()
#'
#' @param id id
#' @param burst burst
#' @export ind_burst
#' @export multi_burst
#' @export make_mulit_burst
#' @examples
#'  data(raccoon_data)
#'  burstz <- list(month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
#'  make_multi_burst(id=raccoon_data$sensor_code,burst=burstz)
#'
ind_burst<- function(id = c(), burst = NULL,...) {
  # Check if id is the only list
  if(is.null(burst)){burst = as.character(id)} else{burst = burst}
  structure(list(id = as.character(id),
    burst = burst) ,
    class = c("ind_burst")
  )
}
multi_burst <- function(x){
  structure(x,
    class = c('multi_burst'))
}

## Constructor
make_multi_burst <- function(id, burst=NULL){
  if(!is.null(burst)){
  burst[['id']] <- id
  burst_list <- do.call(function(...) mapply(list,...,SIMPLIFY=F), burst)
  ret <- multi_burst(lapply(burst_list, function(x) ind_burst(id = x$id, burst=x[names(x)!='id'])))
  }
  if(is.null(burst)){
    burst_list <- lapply(id,function(x) list(id=x))
  ret <- multi_burst(lapply(burst_list, function(x) ind_burst(id = x$id, burst=NULL)))
  }
  return(ret)
}

c.ind_burst <- function(..., recursive = FALSE){
  ind_burst(c(unlist(lapply(list(...), unclass))))
}

c.multi_burst <- function(..., recursive = FALSE){
  multi_burst(c(unlist(lapply(list(...), unclass))))
}

as.data.frame.multi_burst <- function(x,...){
  ret = data.frame(row.names = seq_along(x))
  ret$burst  = multi_burst(x)
  ret
}

#########
# # test bed
# tt <- sapply(x, function(x)x)
#
# ind_burst(id = 1, burst=3)
# p <- ind_burst(id = 1)
# burstz <- list(month = as.POSIXlt(df1$utc_date)$mon, height =as.numeric(df1$height>5))
# burstz[['id']] <- df1$sensor_code
# burst_list <- do.call(function(...) mapply(list,...,SIMPLIFY=F), burstz)
#
# pp <- mapply(ind_burst, id=df1$sensor_code)
# tt <- sapply(burst_list, names)
# names()!='id'
# pp <- multi_burst(lapply(burst_list, function(x) ind_burst(id = x$id, burst=x[names(x)!='id'])))
# pp
#
# pp <- multi_burst(lapply(burst_list, function(x) ind_burst(id = x$id, burst=x[names(x)!='id'])))
# attributes(pp)
# ###############
# pp <- make_multi_burst(id=df1$sensor_code,burst=burstz)
#
# lapply(pp, function(x) x$burst$month)

