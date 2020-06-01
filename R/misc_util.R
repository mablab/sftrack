###################
# Misc utilitie functions
###################
#' @title Return a list of sf_POINTS or a data.frame from a sftraj object
#' @name traj_geom
#' @param traj a trajectory geometery from sf_traj
#' @examples
#' raccoon_data <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))
#' raccoon_data$acquisition_time <- as.POSIXct(raccoon_data$acquisition_time, 'EST')
#'   burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#'   # Input is a data.frame
#' my_traj <- as_sftraj(raccoon_data, time_col ='acquisition_time',
#'   error = NA, coords = c('longitude','latitude'),
#'   burst_list =burstz)
#' print(my_traj, 5, 10)
#'
#' # extract a list of points
#' pts_traj(my_traj)
#'
#' # or a data.frame of points
#' coord_traj(my_traj)
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
  this_seq <- seq(1,dim*2,by = dim)
  ret = lapply(pts, function(x) {
    if (inherits(x, 'GEOMETRYCOLLECTION')) {
      x[1][[1]]
    } else{
      st_point(x[this_seq] )
    }
  })
  if(sfc){st_sfc(ret, crs = attr(pts,'crs'))} else {ret}
}

#' @rdname traj_geom
#' @param first T/F whether you'd like to return the first or second point. Defaults to first.
#' @export
coord_traj <- function(traj, first = TRUE) {
 # traj = my_sftraj
  if (inherits(traj, 'sftraj')) {
    pts <- traj[, attr(traj, 'sf_column')]
  }
  if (inherits(traj, 'sfc')) {
    pts <- traj
  }
  pos <- 2
  if(first) pos <- 1
  if ('XY' %in% class(pts[[1]])) {
    dim = 2
  } else{
    dim = 3
  }
  this_seq <- seq(pos,dim*2,by = dim)
  ret <- lapply(pts, function(x) {
    #  x = pts[[1]]
    if (inherits(x, 'GEOMETRYCOLLECTION')) {
      st_coordinates(x[pos][[1]])
    } else{
      x[this_seq]
      # st_coordinates(x)[pos, dim]
    }
  })
  do.call(rbind, ret)
}

#' @title Is a trajectory geometry a linestring or a geometerycollection
#' @description A step is a movement from one point to the next, with an sftraj object
#' this manifests as a linestring. If, however, one of these two points is missing, the sftraj
#' is created as a geometery collection of two points, the beginning and the end point, where one
#' of the steps is NA. This function checks a trajectory geometry if its a linestring and returns
#' a vector of T/F. Largely an internal function, but can be used to subset sftraj objects.
#' @export
#' @param traj an sftraj object
is_linestring <- function(traj) {
  if (inherits(traj, 'sftraj')) {
    pts <- traj[[attr(traj, 'sf_column')]]
  }
  if (inherits(traj, 'sfc')) {
    pts <- traj
  }
  if ('XY' %in% class(pts[[1]])) {
    dim = c('X', 'Y')
  } else{
    dim = c('X', 'Y', 'Z')
  }
  vapply(traj, function(x)
    inherits(x, 'LINESTRING'), NA)
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

#' @export
which_duplicated <- function(data, burst_list, time_col){
  # data = sub_gps
  # time_col = 'timez'
  # burst_list = list(id = sub_gps$id)
  # data$timez[1] <- data$timez[2]
  burst <-
    make_multi_burst(burst_list = burst_list)

  results <- unlist(tapply(data[[time_col]], burst_labels(burst, TRUE), duplicated))

  data.frame(
    burst = burst_labels(burst, TRUE)[results],
    time = data[[time_col]][results],
    row = which(results),
    row.names = NULL
  )
}
