#' @title Calculate step geometries given a set of groupings, time, and geometries
#'
#' @description This calculates step geometries as individual line segments based on the active_group
#' @param group a c_grouping object
#' @param time sft_timestamp class with start and end date
#' @param geometry the geometery data from either sf or sf_track. Must be an sf geometry class
#' @export make_step_geom
#' @examples
#' #'
#' library(sf)
#' geom <- st_as_sf(data.frame(
#'   x = c(1, 2, 2, 5),
#'   y = c(0, 1, 5, 7)
#' ), coords = c("x", "y"))
#'
#' burst <- list(id = rep(1, 4))
#' time <- 1:4
#'
#' cg <- make_c_grouping(burst)
#'
#' make_step_geom(
#'   group = cg,
#'   geometry = geom$geometry,
#'   time = time
#' )
make_step_geom <- function(group, time, geometry) {
  # burstz <- list(id = raccoon_data$animal_id, month = as.POSIXlt(raccoon_data$timestamp)$mon)
  # #data_sf <- new_sftrack(raccoon_data, time =as.POSIXct(raccoon_data$timestamp),error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',burst =burstz)
  # burst = burst_select(make_multi_burst(burstz, active_burst = c('id')))
  # time = raccoon_data$timestamp

  idz <- group_labels(group)
  t1 <- t1(time)
  t2 <- t2_by_group(t1, group)
  step_geometry <- mapply(function(time2, sub_geom, ids) {
    geom1 <- sub_geom
    geom2 <- geometry[(ids == idz & time2 == t1)]
    if (st_is_empty(geom1) || length(geom2) == 0 || st_is_empty(geom2)) {
      if (all(!st_is_empty(geom1))) {
        new_geom <- sf::st_point(geom1)
      } else {
        new_geom <- sf::st_point()
      }
    } else {
      new_geom <- st_cast(c(geom1, geom2[[1]]), "LINESTRING", ids = 1)
    }
    new_geom
  }, time2 = t2, sub_geom = geometry, ids = idz, SIMPLIFY = FALSE)

  return(sf::st_sfc(step_geometry, crs = attr(geometry, "crs")))
}


# step function
#' @title Calculates step metrics including distance, dt, dx, and dy.
#' @param sftraj an sftrack/sftraj object. sftrack objects will be converted to sftraj internally for calculation.
#' @export
#' @examples
#' #'
#' data("raccoon")
#' raccoon$timestamp <- as.POSIXct(raccoon$timestamp, "EST")
#' burstz <- list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon)
#' # Input is a data.frame
#' my_sftraj <- as_sftraj(raccoon,
#'   group = burstz, time = "timestamp",
#'   error = NA, coords = c("longitude", "latitude")
#' )
#'
#' step_metrics(my_sftraj)[1:10, ]
step_metrics <- function(x) {
  if (inherits(x, "sftrack")) {
    x <- as_sftraj(x)
  }
  group_col <- attr(x, "group_col")
  time_col <- attr(x, "time_col")
  x$sftrack_id <-
    paste0(group_labels(x[[group_col]]), "_", t1(x))
  is_latlong <- any(st_is_longlat(x$geometry), na.rm = TRUE)

  # if only 1 row
  if (nrow(x) == 1) {
    return(
      data.frame(
        dx = NA,
        dy = NA,
        dist = NA,
        dt = NA,
        abs_angle = NA,
        speed = NA,
        sftrack_id = x$sftrack_id
      )
    )
  }

  dx <- get_dx(x)
  dy <- get_dy(x)
  dist <- as.numeric(st_length(x))

  time <- t1(x)
  if (attr(x[[time_col]], "type") == "POSIX") {
    dt <- unclass(difftime(t2(x), time, units = "secs"))
  } else {
    dt <- t2(x) - time
  }
  if (!is_latlong) {
    abs_angle <- ifelse(dist < 1e-07, NA, atan2(dy, dx))
  } else {
    xy1 <- get_point(x, "xy1")
    xy2 <- get_point(x, "xy2")
    abs_angle <- (geosphere::bearing(xy1, xy2) - 90) * -pi / 180
    abs_angle[!is.na(abs_angle) & abs_angle > (pi)] <- abs_angle[!is.na(abs_angle) & abs_angle > (pi)] - 2 * pi
  }
  dist[is.na(dx) | is.na(dy)] <- NA
  speed <- ifelse(is.na(dist), NA, dist / dt)
  rel_angle <- c(NA, abs_angle[-1] - abs_angle[-length(abs_angle)])
  rel_angle <- ifelse(rel_angle <= (-pi), 2 * pi + rel_angle, rel_angle)
  rel_angle <- ifelse(rel_angle > pi, rel_angle - 2 * pi, rel_angle)

  ret <- cbind.data.frame(
    dx = dx,
    dy = dy,
    dist = dist,
    dt = dt,
    abs_angle = abs_angle,
    rel_angle = rel_angle,
    speed = speed
  )
  ret[nrow(ret), c("dx", "dy", "dist", "dt", "abs_angle", "rel_angle", "speed")] <- NA
  ret$sftrack_id <- x$sftrack_id
  ret
}

#' @title recalculate step geometry
#' @description Step geometeries in sftraj objects are linestrings going from t1 to t2 of a 'step'. As these are stored at the row level they are not dynamic to changes in t2.
#' step_recalc allows you to recalculate these geometeries if your data.frame has changed because of subsetting or filtering.
#' @param x an sftraj object.
#' @param return return step_geometry instead of replacing sftraj object with new step geometry. Defaults to FALSE
#' @export
step_recalc <- function(x, return = FALSE) {
  if (!inherits(x, "sftraj")) {
    stop("object is not an sftraj object")
  }
  att <- attributes(x)
  time_col <- att$time_col
  sf_col <- att$sf_column
  group_col <- att$group_col
  geom <- pts_traj(x[[sf_col]], sfc = T)
  step_geometry <-
    make_step_geom(
      group = x[[group_col]],
      geometry = geom,
      time = t1(x)
    )
  if (return) {
    return(step_geometry)
  }

  x[[sf_col]] <- step_geometry
  x
}

get_dx <- function(x) {
  # position = 'x2'
  x <- st_geometry(x)
  crs <- st_crs(x)
  ret <- vapply(
    x, function(y) {
      # y = st_geometry(x)[[1]]
      if (inherits(y, "LINESTRING")) {
        st_length(st_sfc(st_linestring(rbind(c(y[1], y[3]), c(y[2], y[3]))), crs = crs))
      } else {
        0
      }
    },
    numeric(1)
  )
  ret[st_is_empty(x)] <- NA
  ret
}


get_dy <- function(x) {
  # position = 'x2'
  x <- st_geometry(x)
  crs <- st_crs(x)
  ret <- vapply(
    x, function(y) {
      # y = st_geometry(x)[[1]]
      if (inherits(y, "LINESTRING")) {
        st_length(st_sfc(st_linestring(rbind(c(y[1], y[3]), c(y[1], y[4]))), crs = crs))
      } else {
        0
      }
    },
    numeric(1)
  )

  ret[st_is_empty(x)] <- NA
  ret
}
