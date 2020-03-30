#' @title as_sftraj
#' @description This generic has multiple inputs and gathers relevant information
#' to sftraj class.
#' It converts x,y,z data into an sftraj object and gives it an sf$geometry column. This
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
#' @export as_sftraj
#' @examples
#'
#' data(raccoon_data)
#'   burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#'   # Input is a data.frame
#' my_step <- as_sftraj(raccoon_data, time_col ='acquisition_time',
#'   error = NA, coords = c('longitude','latitude'),
#'   burst_list =burstz)
#'   # Input is a ltraj
#'
#'   # Input is a sf object
#'
#'   # Input is an sftrack object
######################

#' @exportMethod as_sftraj
as_sftraj <- function(data,...) {
  UseMethod('as_sftraj')
}
#' @export new_sftraj
new_sftraj <- function(data, burst, time, geometry, error = NA) {

  data_sf <- sf::st_as_sf(cbind(data, burst, geometry = geometry))
  structure(
    data_sf,
    time = time,
    error = error,
    class = c("sftraj", 'data.frame','sf')
  )
}

#########################
# Methods
#' @export
as_sftraj.data.frame <- function(
  data,
  xyz,
  coords = c('x','y'),
  burst_list,
  id,
  burst_col = NULL,
  active_burst = NA,
  error,
  error_col,
  time,
  time_col,
  crs = NA,
  zeroNA = FALSE
){
  # data(raccoon_data)
  # data <- raccoon_data
  # burst = list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
  # error = rep(NA, nrow(data))
  # time = as.POSIXct(data$acquisition_time, tz = 'UTC')
  # coords = c('latitude','longitude','height')

  # data.frame mode

  if(!missing(id)){
    check_names_exist(data, c(id,burst_col))
    burst_list <- lapply(data[,c(id,burst_col), F], function(x)x)
    names(burst_list)[1] <- 'id'}

  # xyz coordinates
  if(!missing(coords)){
    check_names_exist(data, coords)
    xyz <- data[,coords]

    } else{xyz <- as.data.frame(xyz)}
  if(zeroNA){xyz <- fixzero(xyz)}
  check_NA_coords(xyz)

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

  #
  if(any(is.na(active_burst))){active_burst <- names(burst_list)}
  burst <- make_multi_burst(burst_list = burst_list, active_burst = active_burst)

  step_geometry <- make_step_geom(burst_id = burst_select(burst), geometry = geom$geometry,
    time_data = data[, time_col, drop = T])

  ret <- new_sftraj(
    data = data ,
    burst = burst,
    error = error_col,
    time = time_col,
    geometry = step_geometry
  )
  #Sanity check
  dup_timestamp(ret)
  check_z_coords(ret)
  ret <- ret[ordered(ret$burst, ret[,attr(ret,'time')]),]
  #
  return(ret)
}

## Track
#' @export
as_sftraj.sftrack <-function(data){
  burst <- data$burst
  geometry <-  data$geometry
  time <-  attr(data, 'time')
  error <- attr(data, 'error')
  step_geometry <- make_step_geom(burst_id = burst_select(burst), geometry = geometry,
    time_data = data[, time, drop = T])
  new_data <- as.data.frame(data)
  new_data <- new_data[ ,!colnames(new_data) %in%c('geometry','burst')]
  ret <- new_sftraj(
    data = new_data,
    burst = burst,
    error = error,
    time = time,
    geometry = step_geometry
  )

  return(ret)
}

# sf
#' @export
as_sftraj.sf <- function(
  data,
  burst_list,
  id,
  burst_col = NULL,
  active_burst = NA,
  error,
  error_col,
  time,
  time_col
){
  # data(raccoon_data)
  # data <- raccoon_data
  # burst = list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
  # error = rep(NA, nrow(data))
  # time = as.POSIXct(data$acquisition_time, tz = 'UTC')
  # coords = c('latitude','longitude','height')
  geom <- data[,attr(data,'sf_column')]
  data <- as.data.frame(data)
  # data.frame mode
  if(!missing(id)){
    check_names_exist(data, c(id,burst_col))
    burst_list <- lapply(data[,c(id,burst_col), F], function(x)x)
    names(burst_list)[1] <- 'id'
  }
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

  #
  if(any(is.na(active_burst))){active_burst <- names(burst_list)}
  burst <- make_multi_burst(burst_list = burst_list, active_burst = active_burst)

  step_geometry <- make_step_geom(burst_id = burst_select(burst), geometry = geom$geometry,
    time_data = data[, time_col, drop = T])

  ret <- new_sftraj(
    data = data ,
    burst = burst,
    error = error_col,
    time = time_col,
    geometry = step_geometry
  )
  #Sanity check
  dup_timestamp(ret)
  check_z_coords(ret)
  ret <- ret[ordered(ret$burst, ret[,attr(ret,'time')]),]
  #
  return(ret)
}

#' @export
as_sftraj.ltraj <- function(data, crs = NA){
  # This is done so we dont have to import adehabitat. (instead of ld())
  # But it could go either way depending
  new_data <- lapply(seq_along(data), function(x) {
    sub <- data[x,]
    attributes(sub[[1]])
    id <-  attr(sub[[1]], 'id')
    burst <- attr(sub[[1]],'burst')
    infolocs <- infolocs(data)[x]
    reloc_time <- sub[[1]]$date
    coords <- c('x','y')
    data.frame(sub[[1]][,coords],id,burst,reloc_time,infolocs)
    #data.frame(sub[[1]][,coords],id,burst,infolocs)
  }
  )
  df1 <- do.call(rbind, new_data)
  time = 'reloc_time'
  burst = list(id = df1$id)
  if(!all(burst(data)==id(data))){burst$group <- df1$burst}
  coords = c('x','y')
  geom <- sf::st_as_sf(df1[,coords], coords = coords, crs = crs, na.fail = FALSE )
  #
  burst = make_multi_burst(burst_list=burst)
  step_geometry <- make_step_geom(burst_id = burst_select(burst), geometry = geom$geometry,
    time_data = df1[, time])

  ret <- new_sftraj(
    data = df1[,!colnames(df1)%in%c('id','burst')] ,
    burst = burst,
    error = error,
    time = time,
    geometry = step_geometry
  )

  #Sanity checks
  ret <- ret[ordered(ret$burst, ret[,attr(ret,'time')]),]
  #
  return(ret)
}

#' @export
print.sftraj <- function(x,n_row,n_col,...){
  x <- as.data.frame(x) # have to do this because otherwise it uses sf rules...hmmm..need to change
  cat('This is an sftraj object\n')
  cat(print(attr(x$geometry,'crs')))
  #cat(paste0('unique bursts : ', paste(levels(attr(x$burst, 'sort_index')),collapse=', '), '\n'))
  cat(paste0('bursts : total = ', length(x$burst[[1]]),' | active burst = ',paste0(attr(x$burst, 'active_burst'),collapse=', '), '\n'))
  if(missing(n_col)){n_col <- ncol(x)}
  if(missing(n_row)){n_row <- nrow(x)}
  row_l <- ifelse(nrow(x)>n_row,n_row,nrow(x))
  col_l <- length(!colnames(x)%in%c('burst','geometry'))
  p <- ifelse(col_l>n_col&n_col<ncol(x),n_col,col_l) -2
  cat(paste0("Rows: ",nrow(x), " | Cols: ",ncol(x),"\n"))
  if(n_col<ncol(x)|n_row<nrow(x)){
    y <- cbind(x[1:row_l,colnames(x)[1:p]],
      data.frame('...' = rep('...',row_l)),
      x[1:row_l,c('burst','geometry')])
  } else y <- x
  print.data.frame(y)
}

#'@export
summary.sftraj <- function(object,...,stats = FALSE){
  if(stats){summary_sftrack(object)}else(NextMethod())
}
#summary(my_sftrack,stats=T)

