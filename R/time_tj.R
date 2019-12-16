#' Methods for time traj class
#' new_time_tj()
#'
#' This class is a time object that can either be an integer vector or an as.POSIXct
#' object, but with extra checks that the time is unique and ordered.
#'
#' @param time vector of time or as.POSIX format object.
#' @param tz as.POSIX time zone. Not necessary if an integer vector
#'
#' @examples
#' data(raccoon_data)
# timez <- raccoon_data$acquisition_time
#
# new_time_tj(timez)
# Constructor
new_time_tj <- function(time = c(),
  id = c(),
  ...,
  tz = 'UTC') {
  # Check if its unique and ordered
  # Built to be either posix or time
  # Buildedr
  stopifnot(is(time, 'integer') | is(time, 'POSIXct'))

  if (is.integer(time)) {
    timez <- time
    time_class <- 'integer'
  }

  if (is(time, 'POSIXct')) {
    timez <- as.numeric(as.POSIXct(time, tz = tz))
    time_class <- 'posix'
  }

  #check if time and IDs are unique
  id <- factor(id)
  unique_q <- tapply(timez, id, function(x)
    any(duplicated(x)))

  if (any(unique_q)) {
    stop(paste0('time is not unique for individuals: ', names(unique_q)[unique_q]))
  }

  structure(
    timez,
    order_att = order(timez),
    time_class = time_class,
    class = c("time_tj", 'POSIXct')
  )
}



# Methods
as.data.frame.time_tj <- function(x, ...) {
  ret = data.frame(row.names = seq_along(x))
  ret$time = x
  ret
}

# # ################3
# # Test bed
# timez <- df1$acquisition_time
# id <- df1$sensor_code
#
# pp <- new_time_tj(timez,id)
# attributes(pp)
#
# time2 <- as.POSIXct(timez[c(1,1)])
# here <- new_time_tj(time2, id = id[1:2])
#
# here <- new_time_tj(timez, id = id)
