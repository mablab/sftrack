#' @title as_sftrack
#' @description This generic has multiple inputs and gathers relevant information
#' to sftrack class.
#' It converts x,y,z data into an sftrack object and gives it an sf$geometry column. This
#' column is a list of points. It also creates and error, time, and burst column as well of each respective class.
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
#' @export as_sftrack
#' @export new_sftrack
#' @examples
#'
#' data(raccoon_data)
#'   burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#'   # Input is a data.frame
#' my_track <- as_sfrack(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time),
#'   error = NA, coords = c('longitude','latitude','height'),
#'   burst =burstz)
#'   # Input is a ltraj
#'
#'   # Input is a sf object
#'
#'   # Input is an sftrack object
######################
# Builder
#' @exportMethod as_sftrack
as_sftrack <- function(data,...) {
  UseMethod('as_sftrack')
}


new_sftrack <- function(data, burst, time, geometry, error) {
  if(sum(is.na(error)==1)){error <- rep(NA, nrow(data))}

  data_sf <- st_as_sf(cbind(data, burst, time, geometry = geometry, error))
  structure(
    data_sf,
    active_burst = attr(burst, 'active_burst'),
    projection = attr(geometry, 'proj4'),
    class = c("sftrack", 'sf','data.frame')
  )
}

########################
# Methods
#' @export
as_sftrack.data.frame <- function(
  data,
  burst,
  active_burst = 'id',
  error = NA,
  time,
  coords = c('x','y','z')
){
  # data(raccoon_data)
  # data <- raccoon_data
  # burst = list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
  # error = rep(NA, nrow(data))
  # time = as.POSIXct(data$acquisition_time, tz = 'UTC')
  # coords = c('latitude','longitude','height')
  # calculate point geom
  geom <- st_as_sf(data[,coords], coords = coords, na.fail = FALSE)
  # pull out other relevant info
  burst = make_multi_burst(burst, active_burst = active_burst)
  error = new_error_tj(error)
  time = new_time_tj(time)

  ret <- new_sftrack(
    data = data ,
    burst = burst,
    error = error,
    time = time,
    geometry = geom$geometry
  )
  #Sanity check

  #
  return(ret)
}
#data.frame
# data(raccoon_data)
#
# df <- as_sftrack(
#   data = raccoon_data ,
#   burst = list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5)),
#   error = rep(NA, nrow(data)),
#   time = as.POSIXct(data$acquisition_time, tz = 'UTC'),
#   coords = c('latitude','longitude','height')
# )

#' @export
as_sftrack.sfstep <- function(data){

  geometry <- data$geometry

  new_geom <- lapply(geometry, function(x){
    if(c('GEOMETRYCOLLECTION')%in%class(x)){
      return(st_point(unlist(x)[1:3], dim = 'XYZ'))
    }
    if(c('LINESTRING')%in%class(x)){
      return(st_point(x[c(1,3,5)], dim = 'XYZ'))
    }
  })
  geometry <- st_sfc(new_geom)
  burst <- data$burst
  error <- data$error
  time <- data$utc_time

  ret <- new_sftrack(
    data = data ,
    burst = burst,
    error = error,
    time = time,
    geometry = geometry
  )

  return(ret)
}


#data.frame
# data(raccoon_data)
#
# as_sftrack(
#   data = raccoon_data ,
#   burst = list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5)),
#   error = rep(NA, nrow(data)),
#   time = as.POSIXct(data$acquisition_time, tz = 'UTC'),
#   coords = c('latitude','longitude','height')
# )


### Ltraj
#' @export as_sftrack.ltraj
# library(adehabitatLT)
# data(raccoon_data)
# ltraj_df <- as.ltraj(xy=raccoon_data[,c('longitude','latitude')], date = as.POSIXct(raccoon_data$acquisition_time),
#  id = raccoon_data$sensor_code, typeII = TRUE,
#  infolocs = raccoon_data[,1:6] )
# as_sftrack(data = ltraj_df)

as_sftrack.ltraj <- function(data){
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
  coords = c('x','y','z')
  geom <- st_as_sf(df1[,coords], coords = coords , na.fail = FALSE)
  # pull out other relevant info
  burst = make_multi_burst(burst)
  error = new_error_tj(error)
  time = new_time_tj(time)

  ret <- new_sftrack(
    data = df1 ,
    burst = burst,
    error = error,
    time = time,
    geometry = geom$geometry
  )
  #Sanity check? Necessary?

  #
  return(ret)
}


# Methods for 'sftrack' class
#' @export
print.sftrack <- function(x,...){
  x <- as.data.frame(x)
  cat('This is an sftrack object\n')
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

# Sumary
#summary.sftrack

