#' Convert objects into sftraj objects.
#' @name as_sftraj
#' @title Convert objects into sftraj objects.
#' @description
#' This function converts x,y,z data into an sftrack object with a sf_geometry column of sf_POINTS.
#' Creates a `burst` column to group movement data and sets dedicated time and error columns.
#'
#' Raw data input can be done using two 'modes': vector or data.frame. 'Vector' inputs gives the argument as a vector where
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
#' @param ... extra information to be past to as_sftraj
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
#' my_traj <- as_sftraj(raccoon_data, time_col = 'acquisition_time',
#'   error = NA, coords = c('longitude','latitude'),
#'   burst_list = burstz)
#'
#'   # Input is a ltraj
#'   library(adehabitatLT)
#'   ltraj_df <- as.ltraj(xy=raccoon_data[,c('longitude','latitude')],
#'   date = as.POSIXct(raccoon_data$acquisition_time),
#'   id = raccoon_data$sensor_code, typeII = TRUE,
#'   infolocs = raccoon_data[,1:6] )

#'   my_sftraj <- as_sftraj(ltraj_df)
#'   head(my_sftraj)
#'
#'   # Input is a sf object
#'   library(sf)
#'   df1 <- raccoon_data[!is.na(raccoon_data$latitude),]
#'   sf_df <- st_as_sf(df1, coords=c('longitude','latitude'))
#'   id = 'sensor_code'
#'   time_col = 'acquisition_time'
#'
#'   new_sftraj <- as_sftraj(sf_df, id=id, time_col = time_col)
#'   head(new_sftraj)
#'
#'   # Input is an sftrack object
#'   my_track <- as_sftrack(raccoon_data, time_col = 'acquisition_time',
#'   error = NA, coords = c('longitude','latitude'),
#'   burst_list = burstz)
#'
#'   new_traj <- as_sftraj(my_track)
#'   head(new_traj)

as_sftraj <- function(data, ...) {
  UseMethod('as_sftraj')
}

#' @rdname as_sftraj
#' @export
new_sftraj <- function(data, burst, time, geometry, error = NA) {
  data_sf <- sf::st_as_sf(cbind(data, burst, geometry = geometry))
  structure(
    data_sf,
    time = time,
    error = error,
    class = c("sftraj", 'data.frame', 'sf')
  )
}

#########################
# Methods
#' @rdname as_sftraj
#' @export
#' @method as_sftraj data.frame
as_sftraj.data.frame <- function(data,
  ...,
  xyz,
  coords = c('x', 'y'),
  burst_list,
  id,
  burst_col = NULL,
  active_burst = NA,
  error,
  error_col,
  time,
  time_col,
  crs = NA,
  zeroNA = FALSE) {
  # check if columns exist

  if (!missing(id)) {
    check_names_exist(data, c(id, burst_col))
    burst_list <- lapply(data[, c(id, burst_col), F], function(x)
      x)
    names(burst_list)[1] <- 'id'
  }

  # xyz coordinates
  if (!missing(coords)) {
    check_names_exist(data, coords)
    xyz <- data[, coords]

  } else{
    xyz <- as.data.frame(xyz)
  }
  if (zeroNA) {
    xyz <- fix_zero(xyz)
  }
  check_NA_coords(xyz)


  # Time
  if (!missing(time_col)) {
    check_names_exist(data, time_col)
  }
  if (!missing(time)) {
    data$reloc_time <- time
    time_col = 'reloc_time'
  }
  check_time(data[, time_col])

  # Error
  #
  if (!missing(error_col)) {
    check_names_exist(data, error_col)
  }

  if (!missing(error)) {
    data$sftrack_error <- error
    error_col = 'sftrack_error'
  } else {
    if (missing(error_col))
      error_col = NA
  }

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
  attr(geom$geometry, 'n_empty') <-
    sum(vapply(geom$geometry, sfg_is_empty, TRUE))

  step_geometry <-
    make_step_geom(
      burst = burst,
      geometry = geom$geometry,
      time_data = data[, time_col, drop = T]
    )

  ret <- new_sftraj(
    data = data ,
    burst = burst,
    error = error_col,
    time = time_col,
    geometry = step_geometry
  )
  #Sanity checks
  ret <- ret[check_ordered(ret$burst, ret[, attr(ret, 'time')]),]
  dup_timestamp(ret)
  check_z_coords(ret)
  #
  return(ret)
}

#' @rdname as_sftraj
#' @method as_sftraj sftrack
#' @export
as_sftraj.sftrack <- function(data, ...) {
  burst <- data$burst
  geometry <-  data$geometry
  time <-  attr(data, 'time')
  error <- attr(data, 'error')
  step_geometry <-
    make_step_geom(burst = burst,
      geometry = geometry,
      time_data = data[, time, drop = T])
  new_data <- as.data.frame(data)
  new_data <-
    new_data[,!colnames(new_data) %in% c('geometry', 'burst')]
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
#' @rdname as_sftraj
#' @method as_sftraj sf
#' @export
as_sftraj.sf <- function(data,
  ...,
  burst_list,
  id,
  burst_col = NULL,
  active_burst = NA,
  error,
  error_col,
  time,
  time_col) {
  # data(raccoon_data)
  # data <- raccoon_data
  # burst = list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
  # error = rep(NA, nrow(data))
  # time = as.POSIXct(data$acquisition_time, tz = 'UTC')
  # coords = c('latitude','longitude','height')
  geom <- data[, attr(data, 'sf_column')]
  data <- as.data.frame(data)
  # data.frame mode
  if (!missing(id)) {
    check_names_exist(data, c(id, burst_col))
    burst_list <- lapply(data[, c(id, burst_col), F], function(x)
      x)
    names(burst_list)[1] <- 'id'
  }
  if (!missing(error_col)) {
    check_names_exist(data, error_col)
  }
  # Time
  if (!missing(time_col)) {
    check_names_exist(data, time_col)
  }
  if (!missing(time)) {
    data$reloc_time <- time
    time_col = 'reloc_time'
  }
  check_time(data[, time_col])

  if (!missing(error)) {
    data$sftrack_error <- error
    error_col = 'sftrack_error'
  } else {
    if (missing(error_col))
      error_col = NA
  }

  #
  if (any(is.na(active_burst))) {
    active_burst <- names(burst_list)
  }
  burst <-
    make_multi_burst(burst_list = burst_list, active_burst = active_burst)

  step_geometry <-
    make_step_geom(
      burst = burst,
      geometry = geom$geometry,
      time_data = data[, time_col, drop = T]
    )

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
  ret <- ret[check_ordered(ret$burst, ret[, attr(ret, 'time')]),]
  #
  return(ret)
}

#' @rdname as_sftraj
#' @method as_sftraj ltraj
#' @export
as_sftraj.ltraj <- function(data, ...) {
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
  burst_lt <- vapply(data, function(x)
    attr(x, 'id'), NA_character_)

  if (!all(burst_lt == id_lt)) {
    burst$group <- df1$burst
  }
  coords = c('x', 'y')
  geom <-
    sf::st_as_sf(df1[, coords],
      coords = coords,
      crs = crs,
      na.fail = FALSE)
  #
  burst = make_multi_burst(burst_list = burst)
  error = NA
  step_geometry <-
    make_step_geom(
      burst = burst,
      geometry = geom$geometry,
      time_data = df1[, time]
    )

  ret <- new_sftraj(
    data = df1[,!colnames(df1) %in% c('id', 'burst')] ,
    burst = burst,
    error = error,
    time = time,
    geometry = step_geometry
  )

  #Sanity checks
  ret <- ret[check_ordered(ret$burst, ret[, attr(ret, 'time')]),]
  #
  return(ret)
}

#' @export
print.sftraj <- function(x, n_row, n_col, ...) {
  x <-
    as.data.frame(x) # have to do this because otherwise it uses sf rules...hmmm..need to change
  cat('This is an sftraj object\n')
  cat(paste0('crs: ', format(attr(x$geometry, 'crs')), '\n'))
  #cat(paste0('unique bursts : ', paste(levels(attr(x$burst, 'sort_index')),collapse=', '), '\n'))
  cat(paste0(
    'bursts : total = ',
    length(x$burst[[1]]),
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
  col_l <- length(!colnames(x) %in% c('burst', 'geometry'))
  p <- ifelse(col_l > n_col & n_col < ncol(x), n_col, col_l) - 2
  cat(paste0("Rows: ", nrow(x), " | Cols: ", ncol(x), "\n"))
  if (n_col < ncol(x) | n_row < nrow(x)) {
    y <- cbind(x[1:row_l, colnames(x)[1:p]],
      data.frame('...' = rep('...', row_l)),
      x[1:row_l, c('burst', 'geometry')])
  } else
    y <- x
  print.data.frame(y)
}

#'@export
summary.sftraj <- function(object, ..., stats = FALSE) {
  if (stats) {
    summary_sftrack(object)
  } else
    (NextMethod())
}
#summary(my_sftraj,stats=T)
