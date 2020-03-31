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
#'  mb1 <- make_multi_burst(burst_list=burstz, active_burst=c('id','month'))
#'  str(mb1)

#' @export make_ind_burst
make_ind_burst <- function(burst) {
  # check duplicated
  if (any(duplicated(names(burst)))) {
    stop('burst names are duplicated')
  }

  # Check if id is the only list
  if (!'id' %in% names(burst)) {
    stop('There is no id column')
  }

  new_burst <- lapply(burst, as.character)
  # if (is.null(active_burst)) {
  #   active_burst <- names(burst)
  # }
  # active_burst <- c('id', active_burst[active_burst != 'id'])

  ind_burst(new_burst)

}

ind_burst <- function(burst) {
  # if(is.null(label)) {
  #   label <- paste0(sapply(burst, as.character), collapse = '_')
  # } else {
  #   label <- as.character(label)
  # }
  structure(burst ,
    class = c("ind_burst"))
}

# ind_burst(burst=list(id='CJ11',month=3, height=10))
multi_burst <- function(x = list(), active_burst = NULL) {
  if (is.null(active_burst)) {
    active_burst <- names(x)
  }

  sort_index <- calc_sort_index(x, active_burst)

  structure(
    x,
    active_burst = active_burst,
    burst_names = names(x[[1]]),
    sort_index = sort_index,
    class = c('multi_burst')
  )
}

## Constructor
make_multi_burst <-
  function(burst,
    burst_list = NULL,
    active_burst = NULL,
    burst_levels = NULL) {
    burst_list = burst_list
    # check duplicated
    if (any(duplicated(names(burst_list)))) {
      stop('burst names can not be duplicated')
    }

    if (missing(burst)) {
      burst <-
        do.call(function(...)
          mapply(list, ..., SIMPLIFY = F), burst_list)
    }

    NAburst(burst)
    check_burst_names(burst)
    if (is.null(active_burst)) {
      active_burst <- names(burst[[1]])
    }
    ret <- lapply(burst,
      function(x, ...)
        make_ind_burst(burst = x))


    # if(is.null(burst_levels)){burst_levels = calc_burst_levels(ret)}

    mb <- multi_burst(ret, active_burst = active_burst)
    #check more than one burst
    check_two_bursts(mb)

    return(mb)
  }

# make_multi_burst(burst_list=burst_list, active_burst=c('id','month'))

#' @export
c.ind_burst <- function(...) {
  ret = list(...)
  #ret = list(a,b)
  if (length(unique_active_bursts(ret)) > 1) {
    stop('There are more than one possible active bursts')
  }

  check_burst_names(ret)
  active_burst <- names(ret[[1]])
  # bursts = names(ret[[1]])
  # new_levels <- lapply(bursts, function(x){
  #   unique(sapply(ret, function(y) y[[x]]))
  # })
  # names(new_levels) <- bursts
  df <- lapply(ret, function(x) {
    make_ind_burst(unclass(x))
  })
  mb <- multi_burst(df, active_burst = active_burst)
  check_two_bursts(mb)
  NAburst(mb)
  return(mb)
}

#' @export
c.multi_burst <- function(..., recursive = FALSE) {
  x = list(...)
  active_burstz <- unique_active_bursts(x)
  if (length(active_burstz) > 1) {
    stop('There are more than one possible active bursts')
  }

  multi_burst(unlist(lapply(x, unclass), recursive = FALSE),
    active_burst = active_burst(x[[1]][1]))
}
# c(my_track$burst, my_track$burst)

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
print.multi_burst <- function(x, ...) {
  print(lapply(x, function(x)
    x))
}

#' @export
str.multi_burst <- function(object, ...) {
  n <- length(object)
  cat(paste0(class(object)[1], " of length ", n))
  if (n > 0) {
    cat("; first list element: ")
    utils::str(object[[1]], ...)
  }
}
# str.multi_burst(my_track$burst)

#' @export
format.ind_burst <- function(x,...) {
  message(paste0('(', paste0(
    names(x), ': ', as.character(unlist(x)), collapse = ', '
  ), ')'))

}

#' @export
format.multi_burst <- function(x, ...) {
  paste0('(', vapply(x, function(y)
    paste(
      paste0(names(y), ': ', as.character(unlist(y))) , collapse = ', '
    ), NA_character_), ')')
}

#' @export
"[.multi_burst" <- function (x, i, j, ...) {
  multi_burst(NextMethod(), active_burst = attr(x, 'active_burst'))
}
# mb[1:10]

#' @export
"[<-.multi_burst" <- function(x, i, value) {
  if (is.null(value) || inherits(value, "ind_burst"))
    value = list(value)
  x = unclass(x) # becomes a list, but keeps attributes
  ret = make_multi_burst(NextMethod(), active_burst = active_burst(x))

}

#' @export
"[<-.ind_burst" <- function(x, i, value) {
  if (is.null(value) || inherits(value, "ind_burst"))
    value = list(value)
  x = unclass(x) # becomes a list, but keeps attributes
  # ret = make_ind_burst(NextMethod(),active_burst = active_burst(x))
  ret = make_ind_burst(NextMethod())
  structure(ret)
}

#' @export
"$<-.ind_burst" <- function(x, i, value) {
  if (is.null(value) || inherits(value, "ind_burst"))
    value = list(value)
  x = unclass(x) # becomes a list, but keeps attributes
  # ret = make_ind_burst(NextMethod(),active_burst = active_burst(x))
  ret = make_ind_burst(NextMethod())
  structure(ret)
}

#' @export
summary.multi_burst <- function(object, ...) {
  #object = my_track$burst
  #attributes(object)
  levelz <- attr(object, 'sort_index')
  #cat(paste0(class(object)[1], " of length ", length(object),' | active burst : ',paste(active_burst(object),collapse=', '),'\n'))
  #cat('counts: \n')
  summary(levelz)
}
#summary(mb)
#' make burst labels
#'
#' @description These functions access bursts in various ways
#'
#' @param burst A burst object


#' @export burst_sort
#' @export burst_select

# burst_labels <- function(burst) {
#   vapply(
#     burst,
#     FUN = function(x)
#       attr(x, 'label'),
#     vector('character', length = 1)
#   )
# }


#' @export
burst_sort <- function(burst) {
  attr(burst, 'sort_index')
}

#' @export
burst_levels <- function(burst, value) {
  levels(attr(burst, 'sort_index'))
}
#burst_levels(mb1)

#' @export
'burst_levels<-' <- function(burst, value) {
  levels(attr(burst, 'sort_index')) <- value
  burst
}

#' @export
burst_select <- function(burst) {
  # should also pull out select bursts, or perhaps `multi.burst[` already does this
  lapply(burst, function(x)
    x[names(x) %in% attr(burst, 'active_burst')])
}

#' @export
unique_active_bursts <-
  function(burst)
    unique(vapply(burst, function(x)
      paste0(attr(x, 'active_burst'), collapse = ', '), character(1)))

#' active burst
#' @export active_burst
active_burst <- function(burst) {
  check_burst_names(burst)
  attr(burst, 'active_burst')
}
#active_burst(mb)

#' change active burst
#' @export
#' @examples
#'  data(raccoon_data)
#'  burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#'  mb1 <- make_multi_burst(burst_list=burstz, active_burst=c('id','month'))
#'  active_burst(mb1)
#'  active_burst(mb1) <- c('id')
#'  active_burst(mb1)
'active_burst<-' <- function(burst, value) {
  if (!all(value %in% attr(burst, 'burst_names'))) {
    stop('not all values not found in burst')
  }
  attr(burst, 'active_burst') <- value
  attr(burst, 'sort_index') <-
    calc_sort_index(burst, active_burst = value)
  burst
}



#' #' @export
#' calc_burst_levels <- function(x){
#'   burst_levels <- lapply(names(x[[1]]), function(y) unique(sapply(x, function(z) z[[y]])))
#'   names(burst_levels) <- names(x[[1]])
#'   return(burst_levels)
#' }

#' @export
calc_sort_index <- function(burst, active_burst) {
  burstz <-
    vapply(burst, function(x)
      paste0(x[active_burst], collapse = '_'), NA_character_)

  factor(burstz)
}
# calc_sort_index(burst)
