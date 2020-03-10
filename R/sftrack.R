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
# data(raccoon_data)
#   burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#   # Input is a data.frame
# my_track <- as_sftrack(raccoon_data, time_col = 'acquisition_time',
#   error = NA, coords = c('longitude','latitude','height'),
#   burst_list = burstz)
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


new_sftrack <- function(data, burst, time, geometry, error = NA) {

  data_sf <- sf::st_as_sf(cbind(data, burst, geometry = geometry))
  structure(
    data_sf,
    active_burst = attr(burst, 'active_burst'),
    time = time,
    error = error,
    class = c("sftrack", 'data.frame','sf')
  )
}

########################
# Methods
#' @export
as_sftrack.data.frame <- function(
  data,
  xyz,
  coords = c('x','y','z'),
  burst_list,
  id,
  burst_col = NULL,
  active_burst = NA,
  error,
  error_col,
  time,
  time_col,
  crs = NA,
  zeroNA = F
){
  # data(raccoon_data)
  # data <- raccoon_data
  # burst_list = list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
  # error = rep(NA, nrow(data))
  # time = as.POSIXct(data$acquisition_time, tz = 'UTC')
  # coords = c('latitude','longitude','height')
  # check if columns exist

  if(!missing(id)){
    check_names_exist(data, c(id,burst_col))
    burst_list <- lapply(data[,c(id,burst_col), F], function(x)x)
    names(burst_list)[1] <- 'id'}

  if(!missing(coords)){
    check_names_exist(data, coords)
    xyz <- data[,coords]
  }
  if(zeroNA){xyz <- fixzero(xyz)}
  if(!missing(error_col)){ check_names_exist(data, error_col)}
  if(!missing(time_col)){ check_names_exist(data, time_col)}
  # vector mode
  # time
  if(!missing(time)){
    data$reloc_time <- time
    time_col = 'reloc_time'
  }

  if(!missing(error)){
    data$sftrack_error <- error
    error_col = 'sftrack_error'
  } else { if(missing(error_col)) error_col = NA}

  geom <- sf::st_as_sf(xyz, coords = names(xyz), crs = crs, na.fail = FALSE)
  # Force calculation of empty geometries.
  attr(geom$geometry, 'n_empty') <- sum(vapply(geom$geometry, sf:::sfg_is_empty, TRUE))
  # pull out other relevant info
  if(any(is.na(active_burst))){active_burst <- names(burst_list)}
  burst <- make_multi_burst(burst_list = burst_list, active_burst = active_burst)

  ret <- new_sftrack(
    data = data ,
    burst = burst,
    error = error_col,
    time = time_col,
    geometry = geom$geometry
  )
  #Sanity check
  #dup_timestamp(ret)
  #ret <- ret[ordered(ret$burst, ret[,attr(ret,'time'),drop=T]),,drop=T]
  #
  return(ret)
}

#' @export
as_sftrack.sftraj <- function(data){

  geometry <- data$geometry

  point_d <- class(geometry[[1]])[1]
  nd <- which(point_d==c(NA, 'XY', 'XYZ'))
  d_seq <- seq(1,(2*nd),by = 2)
  new_geom <- lapply(geometry, function(x){

    if(c('GEOMETRYCOLLECTION')%in%class(x)){
      return(unclass(x)[[1]])
    }
    if(c('LINESTRING')%in%class(x)){
      return(sf::st_point(x[d_seq], dim = point_d))
    }
  })

  crs <- attr(geometry, 'crs')
  geometry <- sf::st_sfc(new_geom, crs = crs)
  burst <- data$burst
  error <- attr(data, 'error')
  time <- attr(data, 'time')
  new_data <- as.data.frame(data)
  new_data <- new_data[ ,!colnames(new_data) %in%c('geometry','burst')]
  ret <- new_sftrack(
    data = new_data,
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

as_sftrack.ltraj <- function(data, crs = NA){
  # This is done so we dont have to import adehabitat. (instead of ld())
  # But it could go either way depending
  new_data <- lapply(seq_along(data), function(x) {
    sub <- data[x,]
    attributes(sub[[1]])
    id <-  attr(sub[[1]], 'id')
    burst <- attr(sub[[1]],'burst')
    infolocs <- infolocs(sub)
    time <- sub[[1]]$date
    coords <- c('x','y')
    data.frame(sub[[1]][,coords],id,burst,time,infolocs)
  }
  )
  df1 <- do.call(rbind, new_data)
  time = 'reloc_time'
  burst = list(id = df1$id)
  if(!all(burst(data)==id(data))){burst$group <- df1$burst}
  coords = c('x','y')
  geom <- sf::st_as_sf(df1[,coords], coords = coords ,crs = crs, na.fail = FALSE)
  # pull out other relevant info
  burst = make_multi_burst(burst)
  error = NA

  ret <- new_sftrack(
    data = df1[,!colnames(df1)%in%c('id','burst')] ,
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
  cat(paste0('proj : ',attr(x,'crs'),'\n'))
  cat(paste0('unique ids : ', paste(unique(sapply(x$burst, function(x) x$id)),collapse=', '), '\n'))
  cat(paste0('bursts : total = ', length(x$burst[[1]]),' | active burst = ',paste0(attr(x, 'active_burst'),collapse=', '), '\n'))
  n <- ifelse(nrow(x)>10,10,nrow(x))
  row_l <- length(!colnames(x)%in%c('burst','geometry'))
  p <- ifelse(row_l>8,8,row_l)
  cat(paste("First", n, "features w/",p+2, "truncated columns:\n"))
  if(ncol(x)>10){
  y <- cbind(x[1:n,colnames(x)[1:p]],
    data.frame('...' = rep('...',n)),
    x[1:n,c('burst','geometry')])
  } else y <- x
  print.data.frame(y, ...)
}

# Sumary
#summary.sftrack


