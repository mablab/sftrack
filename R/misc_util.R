###################
# Misc utilitie functions
###################
#' @title Return a list of sf_POINTS or a data.frame from a sftraj object
#' @name traj_geom
#' @param traj a trajectory geometery from sf_traj
#' @param sfc TRUE/FALSE should the return by an sfc or a list of points. Defaults to FALSE
#' @examples
#' raccoon_data <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))
#' raccoon_data$acquisition_time <- as.POSIXct(raccoon_data$acquisition_time, 'EST')
#'   burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#'   # Input is a data.frame
#' my_traj <- as_sftraj(raccoon_data, time ='acquisition_time',
#'   error = NA, coords = c('longitude','latitude'),
#'   burst =burstz)
#' print(my_traj, 5, 10)
#'
#' # extract a list of points
#' pts_traj(my_traj)[1:10]
#'
#' # or a data.frame of points
#' coord_traj(my_traj)[1:10]
#' @export
pts_traj <- function(traj, sfc = FALSE) {
  if (inherits(traj, 'sftraj')) {
    pts <- st_geometry(traj)
  }
  if (inherits(traj, 'sfc')) {
    pts <- traj
  }
  if ('XY' %in% class(pts[[1]])) {
    dim = 2
  } else{
    dim = 3
  }
  this_seq <- seq(1,dim*2,by = 2)
  ret = lapply(pts, function(x) {
    if (inherits(x, 'POINT')) {
      x
    } else{
      st_point(x[this_seq] )
    }
  })
  if(sfc){st_sfc(ret, crs = attr(pts,'crs'))} else {ret}
}

#' @rdname traj_geom
#' @export
coord_traj <- function(traj) {
 # traj = my_sftraj
  if (inherits(traj, 'sftraj')) {
    pts <- traj[[attr(traj, 'sf_column')]]
  }
  if (inherits(traj, 'sfc')) {
    pts <- traj
  }

  if ('XY' %in% class(pts[[1]])) {
    dim = 2
  } else{
    dim = 3
  }
  this_seq <- seq(1,dim*2,by = 2)
  ret <- lapply(pts, function(x) {
    #  x = pts[[499]]
    if (inherits(x, 'POINT')) {

      st_coordinates(x)
      x[1:dim]
      } else{
      x[this_seq]
      # st_coordinates(x)[pos, dim]
    }
  })
  do.call(rbind, ret)

}

#' @title Is a trajectory geometry a linestring or a point
#' @description A step is a movement from one point to the next, with an sftraj object
#' this manifests as a linestring. If, however, one of these two points is missing, the sftraj
#' is created as a geometery collection of two points, the beginning and the end point, where one
#' of the steps is NA. This function checks a trajectory geometry if its a linestring and returns
#' a vector of T/F.
#' @export
#' @param x an sftraj object
is_linestring <- function(x) {
  if (inherits(x, 'sftraj')) {
    pts <- x[[attr(x, 'sf_column')]]
  }
  if (inherits(x, 'sfc')) {
    pts <- x
  }
  # if ('XY' %in% class(pts[[1]])) {
  #   dim = c('X', 'Y')
  # } else{
  #   dim = c('X', 'Y', 'Z')
  # }
  vapply(x, function(y)
    inherits(y, 'LINESTRING'), NA)

  # the sf version might be faster?
  #st_is(x,'LINESTRING')
}

#' @title Summarize sftrack objects
#' @param x an sftrack object
#' @export
summary_sftrack <- function(x) {
  track_class <- class(x)[1]
  #x = my_sftrack
  time_col <- attr(x, 'time')
  error_col <- attr(x, 'error')
  sf_col <- attr(x, 'sf_column')

  sub <- x[, ]
  levelz <- burst_labels(x$burst,factor = TRUE)
  statz <-
    tapply(sub[[time_col]], levelz, function(x)
      list(
        'begin' = min(x),
        'end' = max(x),
        'points' = length(x),
        'NAs' = sum(is.na(x))
      ))

  if (track_class == 'sftrack') {
    my_crs <- attr(sub[[sf_col]], 'crs')
    lenz <- tapply(sub[[sf_col]], levelz, function(pts) {
      new_pts <- pts[!vapply(pts, st_is_empty, NA)]
      my_sfc <- st_sfc(st_linestring(st_coordinates(new_pts)), crs = my_crs )
      st_length(my_sfc)
    })
  }
  if (track_class == 'sftraj') {
    lenz <- tapply(sub[[sf_col]], levelz, function(pts) {
      sum(st_length(pts))
    })
  }
  points = vapply(
    statz,
    FUN = function(x)
      x$points,
    numeric(1)
  )
  NAs = vapply(
    statz,
    FUN = function(x)
      x$NAs,
    numeric(1)
  )
  begin_time = lapply(statz, function(x)
    x$begin)
  end_time = lapply(statz, function(x)
    x$end)
  class(begin_time) <- class(end_time) <- c("POSIXct", "POSIXt")
  attr(begin_time, "tzone") <-attr(x[[attr(x, 'time')]], "tzone")
  attr(end_time, "tzone") <- attr(x[[attr(x, 'time')]], "tzone")
  data.frame(
    burst = levels(levelz),
    points,
    NAs,
    begin_time,
    end_time,
    length_m = lenz,
    row.names = NULL)
}

#recalculates empty geometries (take from sf as it is an internal as well)
sfg_is_empty = function(x) {
  switch(class(x)[2],
    POINT = any(!is.finite(x)),
    MULTIPOINT = , LINESTRING = , CIRCULARSTRING = , CURVE = nrow(x) == 0,
    length(x) == 0
  )
}


#' @title Which burst/time stamp combos are duplicated.
#' @description This function returns a data.frame of which rows are duplicated and their time stamps.
#' @export
#' @param data a data.frame containing burst or time data (if necessary)
#' @param burst a list where each entry is a vector of bursts where length == nrow(data)|nrow(time). Or a character vector describing the column name they are located in data
#' @param time a vector of as.POSIXct time, or a character of the column name where it can be found in data

which_duplicated <- function(data = data.frame(), burst, time){
  # coords = c('longitude','latitude')
  # burst = c(id = 'sensor_code', month = 'month')
  # time = 'time'
  # data$time[1] <- data$time[2]
  # data$time[4] <- data$time[5]
  if(all(sapply(burst,length)==nrow(data))){
    # check id in burst
    check_burst_id(burst)
    burst_list <- burst
  } else{
    # check names exist
    check_names_exist(data, burst)
    # check id in burst
    # check id in burst
    check_burst_id(burst)
    # create burst list from names
    burst_list <- lapply(data[, burst, FALSE], function(x) x)
    if(!is.null(names(burst))){ names(burst_list) <- names(burst) } else {names(burst_list) <- burst}
  }

  if (length(time)==nrow(data) ) {
    reloc_time <- time

  } else {
    check_names_exist(data, time)
    reloc_time <- data[[time]]

  }
  check_time(reloc_time)


  burst <-
    make_multi_burst(x = burst_list)

  results <- unlist(tapply(reloc_time, burst_labels(burst, TRUE), duplicated))
  blt <- paste0(burst_labels(burst, TRUE),' | ',reloc_time)

  results <- blt[duplicated(blt)]
  ret <- lapply(results, function(x) data.frame('burst_time' = x, row = which(blt ==x)))
  do.call(rbind, ret)
}

# Get the position of x2, given the time
get_x2 <- function(time){
  or <- order(time)
  seq_along(time)[or][or+1][order(or)]
}



