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
#' @param newlevels a list of new levels if youd like to override the precalculated levels in a burst. ex. list(id = c('new','levels'))
#' @export ind_burst
#' @export multi_burst
#' @export make_multi_burst
#' @examples
#'  data(raccoon_data)
#'  burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
#'  mb1 <- make_multi_burst(id=raccoon_data$sensor_code,burst=burstz)
#'
ind_burst<- function(burst, new_levels = NULL,active_burst = NULL) {
  # new_levels <- list(month = 0:11)
  # burst=list(id=1, month=2)
  # burst=list(id=factor('CJ11'), month=2)
  if(!is.null(new_levels) & !is.list(new_levels)){ stop('new_levels must be a list')}
  check_levels <- all(sapply(names(new_levels),
    function(x){
      burst[[x]]%in%new_levels[[x]]
    }
  ))
  if(!is.null(new_levels) &! check_levels   ){ stop('not all levels found in burst')}
  # Check if id is the only list
  if(!'id' %in% names(burst)){stop('There is no id column')}

  new_burst <- lapply(burst, as.factor)
  if(is.null(active_burst)){ active_burst <- names(burst) }
  active_burst <- c('id',active_burst[active_burst!='id'])
  # override any levels requested
  if(!is.null(new_levels)){
    for(i in names(new_levels)){
      new_burst[[i]] <- factor(burst[[i]], levels = new_levels[[i]])
    }
  }

  labelz <- paste0(sapply(burst[active_burst], as.character), collapse='_')
  structure(new_burst ,
    label = labelz,
    active_burst = active_burst,
    class = c("ind_burst")
  )
}

# ind_burst(burst=list(id='CJ11',month=3, height=10))
multi_burst <- function(x=list(),active_burst){
    structure(x,
    active_burst = active_burst,
    sort_index = factor(sapply(x, function(x) attr(x, 'label'))),
    class = c('multi_burst'))
}

## Constructor
make_multi_burst <- function(burst=NULL, new_levels=NULL, active_burst='id'){
  # new_levels <- list(month = 1:12, height = 1:10)
  # burst=burstz
active_burst=active_burst
  burst_list <- do.call(function(...) mapply(list,...,SIMPLIFY=F), burst)
  burst_levels <- lapply(burst, unique)

  #override levels if provided
  if(!is.null(new_levels)){
    old_names <- names(burst_levels)
    new_names <- names(new_levels)
    for(i in new_names){burst_levels[old_names==i] <- new_levels[i]}
  }
  # ret <- multi_burst(lapply(burst_list,
  #   function(x,...active_burst) ind_burst(burst=x, new_levels=burst_levels, active_burst = active_burst)), active_burst=active_burst)
    ret <- lapply(burst_list,
      function(x,...) ind_burst(burst=x, new_levels=burst_levels, active_burst = active_burst))
    # structure(ret,
    #   active_burst = active_burst,
    #   sort_index = factor(sapply(ret, function(x) attr(x, 'label'))),
    #   class = c('multi_burst'))
    multi_burst(ret, active_burst = active_burst)

}

#make_multi_burst(burst=burstz, active_burst=c('id','month'))
#' @export
ind_2_multi_burst <- function(x=list()){
  ret <- x
  ## Need to write in a function that checks if all bursts are the same

  if(length(unique(burst_labels(ret)))>1){stop('There are more than one possible active burst')}
  active_burst <- attr(ret[[1]], 'active_burst')

  multi_burst(ret, active_burst = active_burst)
}
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
  ret$burst  = x
  ret
}

#' @export
print.ind_burst <- function(x,...){
  cat(paste0('Active burst : ',attr(x,'label')))
}

#' @export
str.multi_burst <- function(object,...){
  n <- length(object)
  cat(paste0(class(object)[1], " of length ", n))
  if (n > 0) {
    cat("; first list element: ")
    str(object[[1]], ...)
  }
}
# str.multi_burst(my_track$burst)

#' @export
"[.multi_burst" <- function (x, i, j, ...) {
  multi_burst(NextMethod(), active_burst = attr(x,'active_burst'))
}

# mb[1:10]
#' make burst labels
#'
#' @description Pulls burst labels from a multi_burst
#'
#' @param burst A burst object

#' @export burst_labels

burst_labels <- function(burst){
  vapply(burst, FUN = function(x) attr(x,'label'),vector('character',length=1))
}

burst_sort <- function(burst){
  attr(burst, 'sort_index')
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

