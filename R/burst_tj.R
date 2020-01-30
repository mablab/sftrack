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
#' @export make_multi_burst
#' @examples
#'  data(raccoon_data)
#'  burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
#'  mb1 <- make_multi_burst(id=raccoon_data$sensor_code,burst=burstz)
#'
ind_burst<- function(burst, new_levels = NULL,...) {
  # new_levels <- list(month = 0:11)
  # burst=burstz
  if(!is.null(new_levels) & !is.list(new_levels)){ stop('new_levels must be a list')}
  if(!is.null(new_levels) &!all(names(new_levels)%in%names(burst))){ stop('not all levels found in burst')}
  # Check if id is the only list
  if(!'id' %in% names(burst)){stop('There is no id column')}

  new_burst <- lapply(burst, as.factor)

  # override any levels requested
  if(!is.null(new_levels)){
    for(i in names(new_levels)){
      new_burst[[i]] <- factor(burst[[i]], levels = new_levels[[i]])
    }
  }

  structure(new_burst ,
    class = c("ind_burst")
  )
}

#ind_burst(burst=list(id=1,month=3, height=10))

multi_burst <- function(x){
  structure(x,
    class = c('multi_burst'))
}

## Constructor
make_multi_burst <- function(burst=NULL, new_levels=NULL){
  # new_levels <- list(month = 1:12, height = 1:10)
  # burst=burstz

  burst_list <- do.call(function(...) mapply(list,...,SIMPLIFY=F), burst)
  burst_levels <- lapply(burst, unique)

  #override levels if provided
  if(!is.null(new_levels)){
    old_names <- names(burst_levels)
    new_names <- names(new_levels)
    for(i in new_names){burst_levels[old_names==i] <- new_levels[i]}
  }
  ret <- multi_burst(lapply(burst_list, function(x) ind_burst(burst=x, new_levels=burst_levels)))

  return(ret)
}

#make_multi_burst(burst=burstz)

#' @export
c.ind_burst <- function(..., recursive = FALSE){
  ind_burst(c(unlist(lapply(list(...), unclass))))
}

#' @export
c.multi_burst <- function(..., recursive = FALSE){
  multi_burst(c(unlist(lapply(list(...), unclass))))
}

#' @export
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

