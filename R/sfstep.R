#' @title Sfstep Class
#' @description This is the highest level class that collects the error, time, and burst class.
#' It converts x,y,z data into an sfstep object and gives it an sf$geometry column. This
#' column is a list of line segments representing each step. It also creates and error, time, and burst column as well of each respective class.
#'
#' @param data Data.frame input, these columns will remain unchanged, and any columns refered to
#' in later parameters are not deleted from this column. The function simply copies the data.frame
#' and adds the appropriate columns.
#' @param proj4 projection (for sf)
#' @param time vector of time
#' @param burst list of named vectors one of which must be named id
#' @param error error vector
#' @param coords vector of three column names for the x,y,z coordinates, in that order.
#' @param tz timezone component, same as as.POSIX
#'
#' @import sf
#' @export new_sfstep
#' @examples
#'  data(raccoon_data)
#'   burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#'  my_step <- new_sfstep(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time),
#'    error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',
#'    burst =burstz)
######################
as_sfstep <- function(data,...) {
  UseMethod('as_sfstep')
}

new_sfstep <- function(data, burst, time, geometry, error) {
  data_sf <- st_as_sf(cbind(data, burst, time, geometry = step_geometry, error))
  structure(
    data_sf,
    active_burst = attr(burst, 'active_burst'),
    projection = attr(geometry, 'proj4'),
    class = c("sfstep", 'sf','data.frame')
  )
}

#########################
# Methods
as_sfstep.data.frame <- function(
  data,
  burst,
  error = NA,
  time,
  coords
){
  # data(raccoon_data)
  # data <- raccoon_data
  # burst = list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
  # error = rep(NA, nrow(data))
  # time = as.POSIXct(data$acquisition_time, tz = 'UTC')
  # coords = c('latitude','longitude','height')
  # calculate point geom if not already a geom
  geom <- st_as_sf(data[,coords], coords = coords, na.fail = FALSE)
  # pull out other relevant info
  burst = make_multi_burst(burst)
  error = new_error_tj(error)
  time = new_time_tj(time)

  step_geometry <- make_step_geom(burst_id = burst_select(burst), geometry = geom$geometry,
    timez= time)

  ret <- new_sfstep(
    data = data ,
    burst = burst,
    error = error,
    time = time,
    geometry = step_geometry
  )
  #Sanity check

  #
  return(ret)
}
# data.frame
# as_sfstep(
#   data = raccoon_data ,
#   burst = list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5)),
#   error = rep(NA, nrow(data)),
#   time = as.POSIXct(data$acquisition_time, tz = 'UTC'),
#   coords = c('latitude','longitude','height')
# )


## Track
as_sfstep.sftrack <-function(data){
  burst <- data$burst
  geometry <-  data$geometry
  time = data$time
  step_geometry <- make_step_geom(burst_id = burst_select(burst), geometry = geometry,
    timez = time)

  ret <- new_sfstep(
    data = data ,
    burst = burst,
    error = error,
    time = time,
    geometry = step_geometry
  )

  return(ret)
}



as_sfstep.ltraj <- function(data){
  # This is done so we dont have to import adehabitat. (instead of ld())
  # But it could go either way depending
  new_data <- lapply(seq_along(data), function(x) {
    sub <- data[x]
    attributes(sub[[1]])
    id <-  attr(sub[[1]], 'burst')
    infolocs <- infolocs(sub)
    date <- sub[[1]]$date
    coords <- c('x','y')
    data.frame(sub[[1]][,coords],z=0,date,id,infolocs)
  }
  )
  df1 <- do.call(rbind, new_data)
  time = df1$date
  burst = list(id=df1$id)
  error = rep(NA, nrow(df1))
  coords = c('x','y','z')
  geom <- st_as_sf(df1[,coords], coords = coords, na.fail = FALSE )

  #
  burst = make_multi_burst(burst)
  error = new_error_tj(error)
  time = new_time_tj(time)
  step_geometry <- make_step_geom(burst_id = burst_select(burst), geometry = geom$geometry,
    timez= time)

  ret <- new_sfstep(
    data = df1 ,
    burst = burst,
    error = error,
    time = time,
    geometry = step_geometry
  )

  #Sanity check? Necessary?

  #
  return(ret)
}

#' @export
print.sfstep <- function(x,...){
  x <- as.data.frame(x) # have to do this because otherwise it uses sf rules...hmmm
  cat('this is a sfstep object\n')
  cat(paste0('proj : ',attr(x,'projection'),'\n'))
  cat(paste0('unique ids : ', paste(unique(sapply(x$burst, function(x) x$id)),collapse=', '), '\n'))
  cat(paste0('bursts : total = ', length(x$burst[[1]]),' | active burst = ',paste0(attr(x, 'active_burst'),collapse=', '), '\n'))
  n <- ifelse(nrow(x)>10,10,nrow(x))
  row_l <- length(colnames(x)!=c('time','burst','error','geometry'))
  p <- ifelse(row_l>6,6,row_l)
  cat(paste("First", n, "features w/",p+4, "truncated columns:\n"))
  if(ncol(x)>10){
    y <- cbind(x[1:n,colnames(x)[1:p]],
    data.frame('...' = rep('...',n)),
    x[1:n,c('time','burst','error','geometry')])
} else y <- x
print.data.frame(y, ...)
}

