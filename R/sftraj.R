#' sftraj Class
#'
#' @description This is the highest level class that collects the error, time, and burst class.
#' It converts x,y,z data into an sftraj object and gives it an sf$geometry column,
#' and creates and error, time, and burst column as well of each respective class.
#'
#' @param data Data.frame input, these columns will remain unchanged, and any columns refered to
#' in later parameters are not deleted from this column. The function simply copies the data.frame
#' and adds the appropriate columns.
#' @param proj4 projection (for sf)
#' @param time vector of time
#' @param id vector of ids for the data
#' @param burst list of named vectors
#' @param error error vector
#' @param coords vector of three column names for the x,y,z coordinates, in that order.
#' @param tz timezone component, same as as.POSIX
#'
#' @import sf
#' @export new_sftraj
#' @examples
#'  data(raccoon_data)
#'  burstz <- list(month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
#' my_traj <- new_sftraj(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time), id = raccoon_data$sensor_code,
#'   error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',
#'   burst =burstz)
######################
# Builder
#

new_sftraj<-
  function(data = data.frame(),
    proj4 = NA,
    time = NA,
    burst = NULL,
    error = NA,
    coords = c('x','y','z'),
    tz = NULL
  ) {
    mb <- make_multi_burst(burst=burst)
    time_tj <- new_time_tj(time,id=burst$id,tz=tz)
    structure(
      sf::st_as_sf(
        data.frame(
          id = seq_len(nrow(data)),
          data,
          time = time_tj ,
         burst = mb,
         error = error
        ),
        coords = coords,
        dim = 'XYZ'
      ),
      projection = proj4,
      class = c("sftraj", 'data.frame')
    )
  }

#' @export
print.sftraj <- function(x,...){
  cat('this is a sftraj object\n')
  cat(paste0('proj : ',attr(x,'projection'),'\n'))
  cat(paste0('unique ids : ', paste(unique(sapply(x$burst, function(x) x$id)),collapse=', '), '\n'))
  cat(paste0('bursts : ', length(x$burst[[1]]), '\n'))
  n <- ifelse(nrow(x)>10,10,nrow(x))
  row_l <- length(colnames(x)!=c('time','burst','error','geometry'))
  p <- ifelse(row_l>6,6,row_l)
  cat(paste("First", n, "features w/",p+4, "truncated columns:\n"))
  y <- cbind(x[1:n,colnames(x)[1:p]],
    data.frame('...' = rep('...',n)),
    x[1:n,c('time','burst','error','geometry')])
  print.data.frame(y, ...)
}
# ################################################
# # Test bed
# library(sf)

## Test data set
# df1 <- read.csv('/home/matt/Documents/sftraj/data_raccoon.csv')
# colnames(df1)[colnames(df1)=='latitude'] <- 'y'
# colnames(df1)[colnames(df1)=='longitude'] <- 'x'
#df1$m <- as.numeric(as.POSIXlt(df1$acquisition_time))
#df1$z <- df1$height
# # sf1 <- new_sftraj(df1[,], proj4 = 4236, time = df1$acquisition_time, time_column = 'acquisition_time')
# # sf1
#
#
# pp <- new_sftraj(df1, time =df1$acquisition_time, id = df1$sensor_code,
#   error = NA, coords = c('x','y','height'), tz = 'UTC',
#   burst = list(month = as.POSIXlt(df1$utc_date)$mon, height =as.numeric(df1$height>5)))
# pp
# pp$time[1]
# burstz[['id']] <- df1$sensor_code
# burst_list <- do.call(function(...) mapply(list,...,SIMPLIFY=F), burstz)
# data <- raccoon_data
# error <- NA
# pp <- data.frame(
#   id = seq_len(nrow(data)),
#   data,
#   time_traj = new_time_tj(time,id=id,tz=tz),
#   burst = make_multi_burst(id=id, burst=burst),
#   error = error
# )

