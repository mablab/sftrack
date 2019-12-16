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
new_time_tj <- function(time = c(),tz='UTC',...) {
  # Check if its unique and ordered
  # Built to be either posix or time
  # Build
  structure(as.POSIXct(time,origin='1970-01-01 00:00.00 UTC', tz = tz),
    order_att = order(as.POSIXct(time,origin='1970-01-01 00:00.00 UTC')),
    class = c("time_tj",'POSIXct')
  )
}
as.data.frame.time_tj <- function(x,...){
  ret = data.frame(row.names = seq_along(x))
  ret$time = x
  ret
}

# ################3
# # Test bed
# timez <- df1$acquisition_time
#
# new_time_tj(timez)
#
