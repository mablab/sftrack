#' Convert objects into sftrack objects.
#' @name as_sftrack
#' @title Convert objects into sftrack objects.
#' @description
#' This function converts x,y,z data into an sftrack object with a sf_geometry column of sf_POINTS.
#' Creates a `burst` column to group movement data and sets dedicated time and error columns.
#'
#' Raw data inputted in two ways: vector or data.frame. 'Vector' inputs gives the argument as a vector where
#' length = nrow(data). 'Data.frame' inputs gives the arguments as the column name of `data` where the input can be found.
#' Either input is allowed, but vector mode over.writes data.frame mode if both are given.
#'
#' Some options are global and required regardless
#' @param data (global) a data.frame of the movement data
#' @param xyz (vector) a data.frame of xy or xyz coordinates
#' @param coords (data.frame) a character vector describing where the x,y,z (optional) coordinates are located in `data`
#' @param burst_list (vector) a list of named vectors describing multiple grouping variables
#' @param id (data.frame) a character string naming the 'id' field in `data`. Required if using data.frame inputs
#' @param burst_col (data.frame - optional) a character vector naming the other grouping columns in `data`.
#' @param active_burst (global) a character vector of the burst names to be 'active' to group data by for analysis
#' @param time (vector) a vector of time information, can be either POSIX or an integer
#' @param time_col (data.frame) a character string naming the column in `data` where the time information is located
#' @param error (vector - optional) a vector of error information for the movement data
#' @param error_col (data.frame - optional) a character string naming the column in `data` where the error information is located
#' @param crs a crs string from rgdal of the crs and projection information for the spatial data. Defaults to NA
#' @param zeroNA logical whether to convert 0s in spatial data into NAs. Defaults to FALSE.
#' @param ... extra information to be past to as_sftrack
#' @param burst a multi_burst
#' @param geometry a vector of sf geometries
#' @import sf
#' @export
#' @examples
#'
#' raccoon_data <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))
#' raccoon_data$acquisition_time <- as.POSIXct(raccoon_data$acquisition_time, 'EST')
#'   burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#'   # Input is a data.frame
#' my_track <- as_sftrack(raccoon_data, time_col = 'acquisition_time',
#'   error = NA, coords = c('longitude','latitude'),
#'   burst_list = burstz)
#'
#'   # Input is a ltraj
#'   library(adehabitatLT)
#'   ltraj_df <- as.ltraj(xy=raccoon_data[,c('longitude','latitude')],
#'   date = as.POSIXct(raccoon_data$acquisition_time),
#'   id = raccoon_data$sensor_code, typeII = TRUE,
#'   infolocs = raccoon_data[,1:6] )

#'   my_sftrack <- as_sftrack(ltraj_df)
#'   head(my_sftrack)
#'
#'   # Input is a sf object
#'   library(sf)
#'   df1 <- raccoon_data[!is.na(raccoon_data$latitude),]
#'   sf_df <- st_as_sf(df1, coords=c('longitude','latitude'))
#'   id = 'sensor_code'
#'   time_col = 'acquisition_time'
#'
#'   new_sftrack <- as_sftrack(sf_df, id=id, time_col = time_col)
#'   head(new_sftrack)
#'
#'   # Input is an sftraj object
#'   my_traj <- as_sftraj(raccoon_data, time_col = 'acquisition_time',
#'   error = NA, coords = c('longitude','latitude'),
#'   burst_list = burstz)
#'
#'   new_track <- as_sftrack(my_traj)
#'   head(new_track)
######################
# Builder
as_sftrack <- function(data, ...) {
  UseMethod('as_sftrack')
}


#' @rdname as_sftrack
#' @export
#' @method as_sftrack data.frame
as_sftrack.data.frame <- function(
  data,
  ...,
  coords,
  burst,
  active_burst = NA,
  time,
  error = NA,
  crs = NA,
  zeroNA = FALSE
) {
  # data =sub_gps
  # coords = c('longitude','latitude')
  # burst = 'id'
  # time = 'timez'
  # crs='+init=epsg:4326'
  # error = NA
  # zeroNA = FALSE
  # active_burst = NA
  # ######################
  # Check inputs
  # Id
  if(length(burst)==nrow(data)){
    if(!'id' %in% names(burst)){stop('There is no `id` column in burst names')}
    burst_list <- burst
  } else{
    # check names exist
    check_names_exist(data, burst)
    # check id in burst
    if(!'id'%in%burst){stop('There is no `id` column in burst names')}
    # create burst list from names
    burst_list <- lapply(data[, burst, FALSE], function(x) x)
  }

  # Coords
  if(is.null(nrow(coords))){
    check_names_exist(data, coords)
    xyz <- data[, coords]
  }else{
    xyz <- as.data.frame(coords)
  }
  # fix zeros to NA
  if (zeroNA) {
    xyz <- fix_zero(xyz)
  }
  check_NA_coords(xyz)


  # Time
  if (length(time)==nrow(data) ) {
    data$reloc_time <- time
    time_col = 'reloc_time'
  } else {
    check_names_exist(data, time)
    time_col = time
  }
  check_time(data[, time_col])

  # Error
  #
  if (!is.na(error)) {
    if(length(error) == nrow(data)){
      data$sftrack_error <- error
      error_col = 'sftrack_error'
    } else{
      check_names_exist(data, error)
      error_col = error
    }
  } else { error_col = NA}

  # pull out other relevant info
  if (any(is.na(active_burst))) {
    active_burst <- names(burst_list)
  }
  burst <-
    make_multi_burst(burst_list = burst_list, active_burst = active_burst)

  geom <-
    sf::st_as_sf(xyz,
      coords = names(xyz),
      crs = crs,
      na.fail = FALSE)
  # Force calculation of empty geometries.
  attr(geom[, attr(geom, 'sf_column')], 'n_empty') <-
    sum(vapply(st_geometry(geom), sfg_is_empty, TRUE))

  ret <- new_sftrack(
    data = data ,
    burst = burst,
    error = error_col,
    time = time_col,
    geometry = st_geometry(geom)
  )
  #Sanity checks
  ret <- ret[check_ordered(ret$burst, ret[, attr(ret, 'time'), drop = T]),]
  dup_timestamp(ret)
  check_z_coords(ret)

  return(ret)
}

#' @rdname as_sftrack
#' @export
new_sftrack <- function(data, burst, time, geometry, error = NA) {
  data_sf <- sf::st_as_sf(cbind(data[,!colnames(data)%in%c('burst','geometry'),F], burst, geometry = geometry))
  structure(
    data_sf,
    time = time,
    error = error,
    class = c("sftrack", 'sf','data.frame')
  )
}

#' @rdname as_sftrack
#' @method as_sftrack sftraj
#' @export
as_sftrack.sftraj <- function(data, ...) {
  geometry <- st_geometry(data)

  # pull out first points from straj
  new_geom <- pts_traj(data)
  crs <- attr(geometry, 'crs')

  geometry <- sf::st_sfc(new_geom, crs = crs)
  burst <- data$burst
  error <- attr(data, 'error')
  time <- attr(data, 'time')
  new_data <- as.data.frame(data)
  new_data <-
    new_data[,!colnames(new_data) %in% c(attr(data, 'sf_column'), 'burst'), drop = TRUE]
  ret <- new_sftrack(
    data = new_data,
    burst = burst,
    error = error,
    time = time,
    geometry = geometry
  )

  return(ret)
}

### Ltraj
#' @rdname as_sftrack
#' @method as_sftrack ltraj
#' @export
as_sftrack.ltraj <- function(data, ...) {
  # This is done so we dont have to import adehabitat. (instead of ld())
  # But it could go either way depending
  new_data <- lapply(seq_along(data), function(x) {
    sub <- data[x,]
    attributes(sub[[1]])
    id <-  attr(sub[[1]], 'id')
    burst <- attr(sub[[1]], 'burst')
    infolocs <- infolocs(data)[x]
    reloc_time <- sub[[1]]$date
    coords <- c('x', 'y')
    data.frame(sub[[1]][, coords], id, burst, reloc_time, infolocs)
  })
  df1 <- do.call(rbind, new_data)
  time = 'reloc_time'
  burst = list(id = df1$id)
  crs = attr(data, 'proj4string')
  # pull out id and burst from ltraj object
  id_lt <- vapply(data, function(x)
    attr(x, 'id'), NA_character_)
  burst_lt <-
    vapply(data, function(x)
      attr(x, 'burst'), NA_character_)

  if (!all(burst_lt == id_lt)) {
    burst$group <- df1$burst
  }
  coords = c('x', 'y')
  geom <-
    sf::st_as_sf(df1[, coords],
      coords = coords ,
      crs = crs,
      na.fail = FALSE)
  # pull out other relevant info
  burst = make_multi_burst(burst_list = burst)
  error = NA

  ret <- new_sftrack(
    data = df1[,!colnames(df1) %in% c('id', 'burst')] ,
    burst = burst,
    error = error,
    time = time,
    geometry = st_geometry(geom)
  )
  #Sanity check. Which are necessary?
  ret <- ret[check_ordered(ret$burst, ret[, attr(ret, 'time'), drop = T]),]
  #
  return(ret)
}
#sf
#' @rdname as_sftrack
#' @method as_sftrack sf
#' @export
as_sftrack.sf <- function(data,
  ...,
  burst_list,
  id,
  burst_col = NULL,
  active_burst = NA,
  error,
  error_col,
  time,
  time_col) {
  geom <- st_geometry(data)
  data <- as.data.frame(data)
  # Check inputs
  # Id
  if(length(burst)==nrow(data)){
    if(!'id' %in% names(burst)){stop('There is no `id` column in burst names')}
    burst_list <- burst
  } else{
    # check names exist
    check_names_exist(data, burst)
    # check id in burst
    if(!'id'%in%burst){stop('There is no `id` column in burst names')}
    # create burst list from names
    burst_list <- lapply(data[, burst, FALSE], function(x) x)
  }

  # Time
  if (length(time)==nrow(data) ) {
    data$reloc_time <- time
    time_col = 'reloc_time'
  } else {
    check_names_exist(data, time)
    time_col = time
  }
  check_time(data[, time_col])

  # Error

  if (!is.na(error)) {
    if(length(error) == nrow(data)){
      data$sftrack_error <- error
      error_col = 'sftrack_error'
    } else{
      check_names_exist(data, error)
      error_col = error
    }
  } else { error_col = NA}

  #
  if (any(is.na(active_burst))) {
    active_burst <- names(burst_list)
  }
  burst <-
    make_multi_burst(burst_list = burst_list, active_burst = active_burst)

  ret <- new_sftrack(
    data = data ,
    burst = burst,
    error = error_col,
    time = time_col,
    geometry = st_geometry(geom)
  )
  #Sanity check
  ret <- ret[check_ordered(ret$burst, ret[, attr(ret, 'time'), drop = T]),]
  dup_timestamp(ret)
  check_z_coords(ret)

  #

  return(ret)
}


# Methods for 'sftrack' class

#' @export
print.sftrack <- function(x, n_row, n_col, ...) {

  cat('This is an sftrack object\n')
  cat(paste0('crs: ', format(attr(st_geometry(x), 'crs')), '\n'))
  cat(paste0(
    'bursts : total = ',
    length(burst_levels(x$burst)),
    ' | active burst = ',
    paste0(attr(x$burst, 'active_burst'), collapse = ', '),
    '\n'
  ))
  if (missing(n_col)) {
    n_col <- ncol(x)
  }
  if (missing(n_row)) {
    n_row <- nrow(x)
  }
  row_l <- ifelse(nrow(x) > n_row, n_row, nrow(x))
  col_l <- length(!colnames(x) %in% c('burst', attr(x, 'sf_column')))
  p <- ifelse(col_l > n_col & n_col < ncol(x), n_col, col_l) - 2
  cat(paste0("Rows: ", nrow(x), " | Cols: ", ncol(x), "\n"))
  if (n_col < ncol(x) | n_row < nrow(x)) {
    y <- cbind(x[1:row_l, colnames(x)[1:p]],
      data.frame('...' = rep('...', row_l)),
      x[1:row_l, c('burst', attr(x, 'sf_column'))])
  } else
    y <- x
  print.data.frame(y)
}
# print(my_track,10,10)

# Sumary
#summary.sftrack
#' @export
summary.sftrack <- function(object, ..., stats = FALSE) {
  if (stats) {
    summary_sftrack(object)
  } else
    (NextMethod())
}
#summary(my_sftrack,stats=TRUE)

`[.sftrack` <- function(x,i,j,...,drop=F){
  #x = my_sftrack
  #i = 1:10
  #rm(j)
  sf_col =  attr(x,'sf_column')
  time_col = attr(x, 'time')
  error_col = attr(x,'error')
  #if(is.na(error_col)){ error_col <- NULL}
  nargs = nargs()
  if(!missing(j) && missing(i)){
    i <- seq_len(nrow(x))
  }
  if (!missing(i) && nargs > 2) {
    if (is.character(i))
      i = match(i, row.names(x))
  }

  #x = as.data.frame(x)
  class(x) = setdiff(class(x), c("sftrack",'sf'))
  x = if (missing(j)) {
    if (nargs == 2)
      x[i]
    else
      x[i, , drop = drop]
  } else
    x[i, j, drop = drop]

  if(drop){
    return(x)
  }

  exist_name <- c('burst',sf_col,time_col,error_col) %in% colnames(x)

  if(any(!exist_name[1:3]) || !is.na(error_col) & !exist_name[4]){

    warning(paste0(paste0(c('burst','geometry','time','error')[!exist_name],collapse=', '),' subsetted out of sftrack object, reverting to ',class(x)[1]))
    return(x)
  }
  if(!missing(i)){
    x$burst = multi_burst(x$burst)
  }
  new_sftrack(x, burst = x$burst, geometry = x[,sf_col,F],time = time_col, error = error_col)
}

rbind.sftrack <- function(...){
  all = list(...)
  #all = list(my_sftrack, my_sftrack1,my_sftrack2)

  same_att <- all(duplicated(lapply(all, function(x) attributes(x)[c('sf_column','time','error')]))[-1])
  if(!same_att){stop('sf, time, and error columns must be the same')}
  att <- attributes(all[[1]])
  time_col <- att$time
  error_col <- att$error
  sf_col <- att$sf_column
  df1 <- do.call(rbind.data.frame, all)
  ret <- new_sftrack(data = df1[,!colnames(df1)%in%c('burst',sf_col), drop = T], burst = df1$burst,
    time = time_col, error = error_col, geometry = st_geometry(df1))

  #Sanity checks
  dup_timestamp(ret)
    return(ret)

}

cbind.sftrack <- function(...,deparse.level = 1){
  all <- list(...)
  #all <- list(my_sftrack,my_sftrack[,1:5,drop=T])
  sftrack_obj <- sapply(all, function(x){
    inherits(x,'sftrack')
  })
  if(sum(sftrack_obj)>1){
    # return error
    stop('Cannot attempt to merge two sftrack objects together, use new_sftrack() instead')
  }
  # The rest below this is not useful as I'm pretty sure you cbind.sftrack only works when both objects are an sftrack object
  # I'll keep it till we can decide what we want cbind to do
  # check if theres two burst columns
  if(sum(unlist(lapply(all, function(x) colnames(x)=='burst')))>1){
    stop('More than one column named "burst" found')
  }

  my_sftrack <- all[sftrack_obj][[1]]
  att <- attributes(my_sftrack)
  time_col <- att$time
  error_col <- att$error
  sf_col <- att$sf_column
  new_sftrack <- do.call(data.frame,all)

  ret <- new_sftrack(data = new_sftrack[,!colnames(new_sftrack)%in%c('burst',sf_col), drop = T], burst = new_sftrack$burst,
    time = time_col, error = error_col, geometry = new_sftrack[[sf_col]])
  ret
}
