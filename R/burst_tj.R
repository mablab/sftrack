#' @name burst-class
#' @title A class to group movement data
#' @description This class describes a burst, which is a grouping variable
#' in which to split the data for analysis purposes.
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
#' @param burst a list containing named burst variables, one item must be named 'id'. ex: list(id = 1, month = 'may'). In the case of a multi_burst can be a list of ind_bursts.
#' @param burst_list a list of equal length vectors which will be combined to create a multi_burst. ex: list(x = 1st_vector, y = 2nd_vector)
#' @param active_burst a vector of the names of the bursts to be considered 'active' for the sake of analysis.
#' @export make_multi_burst
#' @export make_ind_burst
#' @examples
#' # Make a single burst
#' make_ind_burst(burst=list(id='CJ11',month=3, height=10))
#'
#' # Make a multi burst
#'  raccoon_data <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))
#'  raccoon_data$acquisition_time <- as.POSIXct(raccoon_data$acquisition_time, 'EST')
#'  burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#'  mb1 <- make_multi_burst(burst_list=burstz, active_burst=c('id','month'))
#'  str(mb1)
#'
#' # Make a multi_burst from many ind_bursts
#'  a <- make_ind_burst(list(id = 1, year = 2020))
#'  b <- make_ind_burst(list(id = 1, year = 2020))
#'  c <- make_ind_burst(list(id = 2, year = 2020))
#'
#'  c(a, b, c)

#' @rdname burst-class
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
  ind_burst(new_burst)

}

ind_burst <- function(x = list()) {
  structure(x ,
    class = c("ind_burst"))
}

# ind_burst(burst=list(id='CJ11',month=3, height=10))

multi_burst <- function(x = list(), active_burst = NULL) {
  if (is.null(active_burst)) {
    active_burst <- names(x)
  }

  sort_index <- burst_labels(x, TRUE, active_burst)

  structure(
    x,
    active_burst = active_burst,
    burst_names = names(x[[1]]),
    sort_index = sort_index,
    class = c('multi_burst')
  )
}

#' @rdname burst-class
make_multi_burst <-
  function(burst_list = NULL,
    active_burst = NULL) {
    # check duplicated
    # if (any(duplicated(names(burst_list)))) {
    #   stop('burst names can not be duplicated')
    # }

    if (!is.null(burst_list)) {
      burst <-
        do.call(function(...)
          mapply(list, ..., SIMPLIFY = FALSE), burst_list)
    }

    check_NAburst(burst)
    check_burst_names(burst)
    if (is.null(active_burst)) {
      active_burst <- names(burst[[1]])
    }
    ret <- lapply(burst,
      function(x, ...)
        make_ind_burst(x))

    mb <- multi_burst(ret, active_burst = active_burst)
    #check more than one burst
    check_two_bursts(mb)
    return(mb)
  }

# make_multi_burst(burst_list=burst_list, active_burst=c('id','month'))
#' @rdname burst-class
#' @method c ind_burst
#' @export
#' @param ... objects to be pasted together into a multi_burst
c.ind_burst <- function(...) {
  ret = list(...)
  #ret = list(a,b)
  if (length(unique_active_bursts(ret)) > 1) {
    stop('There are more than one possible active bursts')
  }

  check_burst_names(ret)
  active_burst <- names(ret[[1]])
  df <- lapply(ret, function(x) {
    make_ind_burst(unclass(x))
  })
  mb <- multi_burst(df, active_burst = active_burst)
  check_two_bursts(mb)
  check_NAburst(mb)
  return(mb)
}

#' @rdname burst-class
#' @export
#' @method c multi_burst
#' @param recursive ignored
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
  multi_burst(NextMethod(), active_burst = attr(x, 'active_burst'))
}
# mb[1:10]

#' @export
"[<-.multi_burst" <- function(x, i, value) {
  if (is.null(value) || inherits(value, "ind_burst"))
    value = list(value)
  x = unclass(x) # becomes a list, but keeps attributes
  ret = multi_burst(NextMethod(), active_burst = active_burst(x))
  check_burst_names(ret)
  check_two_bursts(ret)
  check_NAburst(ret)
  ret
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
  levelz <- attr(object, 'sort_index')
  summary(levelz)
}
#summary(mb)

#' @title Print the sort_index from a burst
#' @export
#' @param burst a multi_burst
burst_sort <- function(burst) {
  attr(burst, 'sort_index')
}

#' @title Access the burst levels
#' @name burst_levels
#' @param burst a multi_burst
#' @param value a character vector of the new levels, must contain all the current levels
#' @export
#' @examples
#'
#' raccoon_data <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))
#'
#'  mb1 <- make_multi_burst(burst_list = list(id = raccoon_data$sensor_code))
#'  # Access the burst levels
#'  burst_levels(mb1)
#'
#'  # edit the burst levels by adding more values
#'  burst_levels(mb1) <- c('CJ11', 'CJ12', 'CJ13')

burst_levels <- function(burst, value) {
  levels(attr(burst, 'sort_index'))
}
#burst_levels(mb1)

#' @rdname burst_levels
#' @export
'burst_levels<-' <- function(burst, value) {
  levels(attr(burst, 'sort_index')) <- value
  burst
}

#' @title Select the active multi_bursts.
#' @return a list where each position is a list of the active bursts from each ind_burst
#' @param burst a multi_burst
#' @param select (optional), the burst names to subset by, defaults to the current active_burst.
#' @export
burst_select <- function(burst, select = NULL) {
  if (is.null(select)) {
    select <- attr(burst, 'active_burst')
  }

  names_id <- names(burst[[1]]) %in% select
  ret <- lapply(burst, function(x)
    x[names_id])
  return(ret)
}

#' @title Calculates burst labels created from the ind_burst and the active_burst
#' @param burst a multi_burst
#' @param factor logical, whether to return a factor, defaults return is a character
#' @param active_burst (optional), the active_burst to subset by, defaults to the current active_burst.
#' @export
burst_labels <- function(burst,
  factor = FALSE,
  active_burst = NULL) {
  if (is.null(active_burst)) {
    active_burst <- attr(burst, 'active_burst')
  }
  names_id <- names(burst[[1]]) %in% active_burst

  ret <-
    vapply(burst, function(x)
      paste(x[names_id], collapse = '_'), NA_character_)

  if (factor) {
    ret <- factor(ret)
  }

  return(ret)
}

unique_active_bursts <-
  function(burst)
    unique(vapply(burst, function(x)
      paste0(attr(x, 'active_burst'), collapse = ', '), character(1)))

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
#'  mb1 <- make_multi_burst(burst_list=burstz, active_burst=c('id','month'))
#'
#'  # see the current active burst
#'  active_burst(mb1)
#'
#'  # change the active burst
#'  active_burst(mb1) <- 'id'
#'
#'  # Using a full data set
#'  my_track <- as_sftrack(raccoon_data, time_col = 'acquisition_time',
#'   error = NA, coords = c('longitude','latitude'),
#'   burst_list = burstz)
#'
#'  summary(my_track)
#'
#'  # change active_burst
#'  active_burst(my_track$burst) <- 'id'
#'
#'  summary(my_track)
#'
active_burst <- function(burst) {
  check_burst_names(burst)
  attr(burst, 'active_burst')
}
#active_burst(mb)

#' @export
#' @rdname active_burst
#' @param value character vector of the burst names to make active
'active_burst<-' <- function(burst, value) {
  if (!all(value %in% attr(burst, 'burst_names'))) {
    stop('not all values not found in burst')
  }
  attr(burst, 'active_burst') <- value
  attr(burst, 'sort_index') <-
    burst_labels(burst, TRUE, active_burst = value)
  burst
}

