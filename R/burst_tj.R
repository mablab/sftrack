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
#' # Make a single burst
#' make_ind_burst(burst=list(id='CJ11',month=3, height=10))
#'
#' # Make a multi burst
#'  data(raccoon_data)
#'  burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#'  mb1 <- make_multi_burst(burst=burstz, active_burst=c('id','month'))
#'  str(mb1)

#' @export make_ind_burst
make_ind_burst <- function(burst,
  new_levels = NULL,
  active_burst = NULL) {
  # new_levels <- list(month = 0:11)
  # burst=list(id=1, month=2)
  # burst=list(id=factor('CJ11'), month=2)

  # check duplicated
  if(any(duplicated(names(burst)))){stop('burst names are duplicated')}
  if (!is.null(new_levels) &
      !is.list(new_levels)) {
    stop('new_levels must be a list')
  }

  if(!is.null(new_levels) & !all(names(new_levels)%in% names(burst) )){stop('Not all names in new_levels found in burst')}

  check_levels <- all(sapply(names(new_levels),
    function(x) {
      burst[[x]] %in% new_levels[[x]]
    }))
  if (!is.null(new_levels) &
      !check_levels) {
    stop('not all levels found in burst')
  }
  # Check if id is the only list
  if (!'id' %in% names(burst)) {
    stop('There is no id column')
  }

  new_burst <- lapply(burst, as.factor)
  if (is.null(active_burst)) {
    active_burst <- names(burst)
  }
  active_burst <- c('id', active_burst[active_burst != 'id'])
  # override any levels requested
  if (!is.null(new_levels)) {
    for (i in names(new_levels)) {
      new_burst[[i]] <- factor(burst[[i]], levels = new_levels[[i]])
    }
  }

ind_burst(new_burst, active_burst = active_burst)

}

ind_burst <- function(burst, active_burst = NULL){
  labelz <-paste0(sapply(burst[active_burst], as.character), collapse = '_')
  if(is.null(active_burst)){active_burst <- names(burst)}
structure(
  burst ,
  label = labelz,
  active_burst = active_burst,
  class = c("ind_burst")
)
}

# ind_burst(burst=list(id='CJ11',month=3, height=10))
multi_burst <- function(x = list(), active_burst) {
  structure(
    x,
    active_burst = active_burst,
    burst_names = names(x[[1]]),
    sort_index = factor(sapply(x, function(x)
      attr(x, 'label'))),
    class = c('multi_burst')
  )
}

## Constructor
make_multi_burst <-
  function(burst_list = NULL,
    burst,
    new_levels = NULL,
    active_burst = 'id') {
    # new_levels <- list(month = 1:12, height = 1:10)
    # burst=burstz
    burst_list = burst_list
    active_burst = active_burst
    # check duplicated
    if(any(duplicated(names(burst_list)))){stop('burst names can not be duplicated')}
    burst_levels <- lapply(burst_list, unique)
    #
    if(missing(burst)){burst <-
      do.call(function(...)
        mapply(list, ..., SIMPLIFY = F), burst_list)}


    NAburst(burst)
    #override levels if provided
    if (!is.null(new_levels)) {
      old_names <- names(burst_levels)
      new_names <- names(new_levels)
      for (i in new_names) {
        burst_levels[old_names == i] <- new_levels[i]
      }
    }
    ret <- lapply(burst,
      function(x, ...)
        make_ind_burst(
          burst = x,
          new_levels = burst_levels,
          active_burst = active_burst
        ))
    check_burst_names(ret)
    mb <- multi_burst(ret, active_burst = active_burst)

    #check more than one burst
    check_two_bursts(mb)

    return(mb)
  }

#make_multi_burst(burst=burstz, active_burst=c('id','month'))

#' @export
c.ind_burst <- function(...){
  ret = list(...)
  #ret = list(a,b)
  if (length(unique_active_bursts(ret)) > 1) {
    stop('There are more than one possible active bursts')
  }
  active_burst <- attr(ret[[1]], 'active_burst')
  check_burst_names(ret)
  bursts = names(ret[[1]])
  new_levels <- lapply(bursts, function(x){
    unique(sapply(ret, function(y) y[[x]]))
  })
  names(new_levels) <- bursts
  df <- lapply(ret, function(x) {
    make_ind_burst(unclass(x), new_levels = new_levels, active_burst=active_burst)
  })
  mb <- multi_burst(df, active_burst)
  check_two_bursts(mb)
  NAburst(mb)
  return(mb)
}

#' @export
c.multi_burst <- function(..., recursive = FALSE) {
  multi_burst(c(unlist(lapply(
    list(...), unclass
  ))))
}

#' @export
as.data.frame.multi_burst <- function(x, ...) {
  ret = data.frame(row.names = seq_along(x))
  ret$burst  = x
  ret
}

#' @export
print.ind_burst <- function(x, ...) {
  print(lapply(x, as.character))
}

#' @export
print.multi_burst <- function(x,...) {
  print(lapply(x, function(x)x))
}

#' @export
str.multi_burst <- function(object, ...) {
  n <- length(object)
  cat(paste0(class(object)[1], " of length ", n))
  if (n > 0) {
    cat("; first list element: ")
    str(object[[1]], ...)
  }
}
# str.multi_burst(my_track$burst)

#' @export
format.ind_burst <- function(x){
  message(paste0('(',paste0(names(x),': ',as.character(unlist(x)),collapse=', '),')'))

}

#' @export
format.multi_burst <- function(mb,...){
  paste0('(',sapply(mb,function(x)paste(paste0(names(x),': ',as.character(unlist(x))) ,collapse=', ')),')')
}

#' @export
"[.multi_burst" <- function (x, i, j, ...) {
  multi_burst(NextMethod(), active_burst = attr(x, 'active_burst'))
}

# mb[1:10]
#' make burst labels
#'
#' @description These functions access bursts in various ways
#'
#' @param burst A burst object

#' @export burst_labels
#' @export burst_sort
#' @export burst_select

burst_labels <- function(burst) {
  vapply(
    burst,
    FUN = function(x)
      attr(x, 'label'),
    vector('character', length = 1)
  )
}

#' @export
burst_sort <- function(burst) {
  attr(burst, 'sort_index')
}

#' @export
burst_select <- function(burst){
  # should also pull out select bursts, or perhaps `multi.burst[` already does this
  lapply(burst, function(x) x[names(x)%in%attr(burst,'active_burst')])
}

#' @export
unique_active_bursts <- function(burst) unique(vapply(burst, function(x) paste0(attr(x,'active_burst'), collapse=', '),character(1)))

# change label

#' active burst
#' @export
active_burst <- function(burst){
  check_burst_names(burst)
  attr(burst, 'active_burst')
}
#active_burst(mb)

#' change active burst
#' @export
#' @examples
#'mb <- ind_burst(list(id=1, group=2),active_burst='id')
#'active_burst(mb)
#'active_burst(mb) <- c('id','group')
#'active_burst(mb)
'active_burst<-' <- function(burst, value){
  if(!all(value==attr(burst, 'burst_names'))){stop('not all values not found in burst')}
  attr(burst, 'active_burst') <- value
  burst
}

