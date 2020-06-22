#' @name burst-class
#' @title A class to group movement data
#' @description This class describes a burst, which is a grouping variable for movement data.
#' It is composed of a list with named vectors.
#' One of which must be 'id', the id of the subject monitored,
#' and can be any length beyond that.
#'
#' @details A burst is a list of possible grouping categories. The 'active_bursts'
#' of these groups is a collection of these variables that when combined form a singular group
#' for analysis purposes. The 'active_burst' can be any combination of the categories in a burst,
#' and can change with the use of `active_burst()`.
#'
#' An `ind_burst` is a single rows burst. It is a 1xn dimensional list with any length(n) > 1.
#' Atleast one of the bursts must be named 'id' which is the subjects id.
#'
#' A `multi_burst` is a collection of `ind_burst`s, it is a data.frame with dimensions of 1xnrow(data).
#' One multi_burst has one 'active_burst' which describes the set of names in each ind_burst to use as grouping
#' variables for analysis purposes. When you change the 'active_burst', calculations and plots change accordingly
#' to the new grouping levels.
#'
#' You can create bursts with make_ind_burst and make multi_burst.
#' @param x a list containing named burst variables, one item must be named 'id'. ex: list(id = 1, month = 'may'). For a multi_burst: A list of ind_bursts or
#' a list of equal length named vectors which will be combined to create a multi_burst. ex: list(x = 1st_vector, y = 2nd_vector)
#' @param active_burst a vector of the names of the bursts to be considered 'active' for the sake of analysis.
#' @export make_multi_burst
#' @export make_ind_burst
#' @examples
#' # Make a single burst
#'
#' make_ind_burst(x = list(id='CJ11',month=3, height=10))
#'
#' # Make a multi burst
#'  raccoon_data <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))
#'  raccoon_data$acquisition_time <- as.POSIXct(raccoon_data$acquisition_time, 'EST')
#'  burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#'  mb1 <- make_multi_burst(x=burstz, active_burst=c('id','month'))
#'  str(mb1)
#'
#' # Make a multi_burst from many ind_bursts
#'  a <- make_ind_burst(list(id = 1, year = 2020))
#'  b <- make_ind_burst(list(id = 1, year = 2020))
#'  c <- make_ind_burst(list(id = 2, year = 2020))
#'
#'  c(a, b, c)

#' @rdname burst-class
make_ind_burst <- function(x, active_burst = NULL) {
  # check duplicated
  if (any(duplicated(names(x)))) {
    stop('burst names are duplicated')
  }

  # check id in burst
  check_burst_id(x)

  new_burst <- lapply(x, as.character)
  ind_burst(new_burst, active_burst)

}
#ib <-  make_ind_burst(burst=list(id='CJ11',month=3, height=10))
#attributes(ib)

ind_burst <- function(x = list(), active_burst = NULL) {

  if(is.null(active_burst)){active_burst <- names(x)}
  label <- paste(x[active_burst],collapse='_')

  structure(x ,
    active_burst = active_burst,
    label = label,
    class = c("ind_burst"))
}

 #ind_burst(x=list(id='CJ11',month=3, height=10))

multi_burst <- function(x = list()) {
  # if (is.null(active_burst)) {
  #   if( inherits(x,'multi_burst')) {
  #     active_burst = active_burst(x)
  #   } else
  #     active_burst <- names(x[[1]])
  # }

  structure(
    x,
    active_burst = attr(x[[1]],'active_burst'),
    class = c('multi_burst')
  )
}

#x = list(id = rep(c(1:5),2), col1=rep(c('a','b'),5))
#active_burst = 'id'
#' @rdname burst-class
make_multi_burst <-
  function(x = NULL,
    active_burst = NULL) {

    if(inherits(x[[1]], c('multi_burst','ind_burst'))) {burst = x } else{
    if(!inherits(x, 'list')){stop('burst must be a list')}

      burst <-
        do.call(function(...)
          mapply(list, ..., SIMPLIFY = FALSE), x)
    }

    # to stop a problem where vector mode names are created via the first item in the list
    names(burst) <- NULL
    if (is.null(active_burst)) {
      active_burst <- names(burst[[1]])
    }
    check_NA_burst(burst)
    check_burst_names(burst)
    check_active_burst(burst, active_burst, FALSE)
    check_two_bursts(burst, active_burst = active_burst)
    ret <- lapply(burst,
      function(x, ...)
        make_ind_burst(x, active_burst = active_burst) )

    mb <- multi_burst(ret)

    return(mb)
  }

# make_multi_burst(x=burst_list, active_burst=c('id','month'))
#' @rdname burst-class
#' @method c ind_burst
#' @export
#' @param ... objects to be pasted together into a multi_burst
c.ind_burst <- function(...) {
  ret = list(...)
  #ret = list(ind_burst(list(id=1)), ind_burst(list(id=2)))
  unique_active_bursts(ret)
  make_multi_burst(ret)
  # check_burst_names(ret)
  # active_burst <- names(ret[[1]])
  # df <- lapply(ret, function(x) {
  #   make_ind_burst(unclass(x))
  # })
  # check_NA_burst(df)
  # mb <- multi_burst(df, active_burst = active_burst)
  # check_two_bursts(mb)
  #
  # return(mb)
}

#' @rdname burst-class
#' @export
#' @method c multi_burst
#' @param recursive ignored
c.multi_burst <- function(..., recursive = FALSE) {
  x = list(...)
  # x = list(multi_burst(list(ind_burst(list(id=1,id2=3)), ind_burst(list(id=2,id2=3)))),
  # multi_burst(list(ind_burst(list(id=1,id2=3)), ind_burst(list(id=2,id2=3)))))
  x = unlist(x, recursive = FALSE)
  unique_active_bursts(x)
  active_burst <- attr(x[[1]],'active_burst')
  make_multi_burst(x, active_burst = active_burst)

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
#' @importFrom utils str
str.multi_burst <- function(object, ...) {
  n <- length(object)
  cat(paste0(class(object)[1],' object \n'))
    utils::str(unclass(object), list.len=1)
}
# str.multi_burst(my_sftrack$burst)

#' @export
format.ind_burst <- function(x, ...) {
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
  nargs = nargs()
  if (!missing(i) && is.character(i)) {
    i = which(lapply(x, attr,'label') %in% i)
  }
  multi_burst(NextMethod())
}
# mb[1:10]
#mb1[1:10]
#mb1['CJ13_1']
#my_sftrack['ID101_1_IS_FU4_3',]
#' @export
"[<-.multi_burst" <- function(x, i, value) {
  # x = make_multi_burst(list(id = rep(c(1:5),2), col1=rep(c('a','b'),5)), active_burst = 'id')
  # i = 1:3
  # value = ind_burst(list(id = 20, col1 = 'c'),'id')
  if (is.null(value) || inherits(value, "ind_burst"))
    value = list(value)
#attributes(x[[1]])
  unique_active_bursts(list(x[[1]],value[[1]]))
  x = unclass(x) # becomes a list, but keeps attributes
  ret = multi_burst(NextMethod())
  check_burst_names(ret)
  check_two_bursts(ret)
  check_NA_burst(ret)
  ret
}

#' @export
"[<-.ind_burst" <- function(x, i, value) {
  if (is.null(value) || inherits(value, "ind_burst"))
    value = list(value)
  x = unclass(x) # becomes a list, but keeps attributes
  # ret = make_ind_burst(NextMethod(),active_burst = active_burst(x))
  ret = make_ind_burst(NextMethod(), active_burst = attr(x, 'active_burst'))
  structure(ret)
}

#' @export
"$<-.ind_burst" <- function(x, i, value) {
  if (is.null(value) || inherits(value, "ind_burst"))
    value = list(value)
  x = unclass(x) # becomes a list, but keeps attributes
  ret = make_ind_burst(NextMethod(), active_burst = attr(x, 'active_burst'))
  structure(ret)
}

#' @export
summary.multi_burst <- function(object, ...) {
  ab <- paste0(attr(object, 'active_burst'),collapse=', ')
  object <- burst_labels(object, factor = T)
  levels(object) <- c(levels(object), paste('active_burst:',ab))
 NextMethod()
}
#summary(mb1)

#' @title Shows burst labels created from the ind_burst and the active_burst
#' @name burst_labels
#' @param x a sftrack or burst object
#' @param factor logical, whether to return a factor, default return is a character
#' @param ... ignored
#' @export
burst_labels <- function(x, ...) {
  UseMethod('burst_labels', object = x)
}


#' @export
#' @rdname burst_labels
burst_labels.sftrack <-   function(x,..., factor = F) {
  burst <- x$burst

  ret <- vapply(burst, function(x) {attr(x,'label')},NA_character_)
  if (factor) {
    ret <- factor(ret)
  }

  return(ret)
}

#' @export
#' @rdname burst_labels
burst_labels.sftraj <- function(x,..., factor = F) {
  burst <- x$burst

  ret <- vapply(burst, function(x) {attr(x,'label')},NA_character_)
  if (factor) {
    ret <- factor(ret)
  }

  return(ret)
}
#' @export
#' @rdname burst_labels
burst_labels.ind_burst <- function(x, ...) {
  burst <- x

  ret <- attr(x,'label')

  return(ret)
}

#' @export
#' @rdname burst_labels
burst_labels.multi_burst <- function(x,..., factor = F) {
  burst <- x

  ret <- vapply(burst, function(x) {attr(x,'label')},NA_character_)
  if (factor) {
    ret <- factor(ret)
  }

  return(ret)
}

#' @title Access the active_burst value
#' @rdname active_burst
#' @param burst a multi_burst
#' @export
#' @description The active burst is the combination of bursts that group the data sets.
#' The active_bursts are essentially a paste(names_of_bursts, sep = '_') grouping variable.
#' @examples
#' raccoon_data <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))
#' raccoon_data$acquisition_time <- as.POSIXct(raccoon_data$acquisition_time, 'EST')
#'  burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#'  mb1 <- make_multi_burst(x=burstz, active_burst=c('id','month'))
#'
#'  # see the current active burst
#'  active_burst(mb1)
#'
#'  # change the active burst
#'  active_burst(mb1) <- 'id'
#'
#'  # Using a full data set
#'  my_track <- as_sftrack(raccoon_data, time = 'acquisition_time',
#'   error = NA, coords = c('longitude','latitude'),
#'   burst = burstz)
#'
#'  summary(my_track)
#'
#'  # change active_burst
#'  active_burst(my_track$burst) <- 'id'
#'
#'  summary(my_track)
#' #'
active_burst <- function(burst) {
  if(inherits(burst,c('sftrack','sftraj'))){return(attr(burst$burst, 'active_burst'))}
  #check_burst_names(x)
  attr(burst, 'active_burst')
}
#active_burst(my_sftrack)
#' @title Set new active burst
#' @export
#' @rdname active_burst_replace
#' @rdname x sftrack/sftraj/multi_burst/ind_burst object
#' @param value character vector of the burst names to make active
'active_burst<-' <- function(x, value) {
  UseMethod('active_burst<-', object = x)
}

#' @export
#' @name active_burst_replace
'active_burst<-.sftrack' <- function(x, value) {
  #x = my_sftrack
  #value = 'id'
    burst <- x$burst

  if (!all(value %in% names(burst[[1]]))) {
    stop('not all names found in burst')
  }

    x$burst <-  make_multi_burst(burst, active_burst = value)
    structure(x)
}
#' @export
#' @name active_burst_replace
'active_burst<-.sftraj' <- function(x, value) {
  #x = my_sftraj
  #value = 'id'
  burst <- x$burst

  if (!all(value %in% names(burst[[1]]))) {
    stop('not all names found in burst')
  }

  x$burst <-  make_multi_burst(burst, active_burst = value)
  step_recalc(x)

}

#' @export
#' @name active_burst_replace
'active_burst<-.multi_burst' <- function(x, value) {
  #x = my_sftrack
  #value = 'id'
 burst = x

  if (!all(value %in% names(burst[[1]]))) {
    stop('not all names found in burst')
  }

 make_multi_burst(burst, active_burst = value)

}

#' @export
#' @name active_burst_replace
'active_burst<-.ind_burst' <- function(x, value) {
  #x = my_sftrack
  #value = 'id'
  burst = x

  if (!all(value %in% names(burst))) {
    stop('not all names found in burst')
  }

  make_ind_burst(burst, active_burst = value)

}
#' # active_burst(my_sftrack) <- c('id','numSat')

