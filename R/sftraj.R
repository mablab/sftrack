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
#'  burstz <- list( id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
#' my_traj <- new_sftraj(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time),
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
    tz = NULL,
    active_burst = 'id'
  ) {

    # Make multi burst
    mb <- make_multi_burst(burst=burst, active_burst = active_burst)
    time_tj <- new_time_tj(time,id=burst$id,tz=tz)

    # Order data frame for later
    torder <- do.call(order,append(burst[active_burst], list(time)))
    # id label should be the first mentioned in the burst
    active_burst <- c('id',active_burst[active_burst!='id'])
    new_data <- data.frame(
      traj_id = NA,
      data,
      time = time_tj,
      burst = mb,
      error = error
    )
    new_data <- new_data[torder,]
    traj_id <- seq_len(nrow(new_data))
    new_data$traj_id = traj_id
    burst_labels = factor(sapply(new_data$burst,function(x)paste0(unlist(x[active_burst]),collapse='_')))
    ret <- structure(
      sf::st_as_sf(
        new_data,
        coords = coords,
        dim = 'XYZ',
        na.fail = FALSE
      ),
      projection = proj4,
      active_burst = active_burst,
      burst_labels = burst_labels,
      label_traj_id = traj_id,
      class = c("sftraj", "sf",'data.frame')
    )
  }

#' @export
print.sftraj <- function(x,...){
  x <- as.data.frame(x)
  cat('This is an sftraj object\n')
  cat(paste0('proj : ',attr(x,'projection'),'\n'))
  cat(paste0('unique ids : ', paste(unique(sapply(x$burst, function(x) x$id)),collapse=', '), '\n'))
  cat(paste0('bursts : total = ', length(x$burst[[1]]),' | active burst = ',paste0(attr(x, 'active_burst'),collapse=', '), '\n'))
  n <- ifelse(nrow(x)>10,10,nrow(x))
  row_l <- length(!colnames(x)%in%c('time','burst','error','geometry'))
  p <- ifelse(row_l>6,6,row_l)
  cat(paste("First", n, "features w/",p+4, "truncated columns:\n"))
  if(ncol(x)>10){
  y <- cbind(x[1:n,colnames(x)[1:p]],
    data.frame('...' = rep('...',n)),
    x[1:n,c('time','burst','error','geometry')])
  } else y <- x
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

