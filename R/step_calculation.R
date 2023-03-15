#' @title Calculate step geometries given a set of groupings, time, and geometries
#'
#' @description This calculates step geometries as individual line segments based on the active_group
#' @param group a c_grouping object
#' @param time_data time vector
#' @param geometry the geometery data from either sf or sf_track. Must be an sf geometry class
#' @export make_step_geom
#' @examples
#' #'
#' library("sf")
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
#'   time_data = time
#' )
make_step_geom <- function(group, time_data, geometry) {
  # burstz <- list(id = raccoon_data$animal_id, month = as.POSIXlt(raccoon_data$timestamp)$mon)
  # #data_sf <- new_sftrack(raccoon_data, time =as.POSIXct(raccoon_data$timestamp),error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',burst =burstz)
  # burst = burst_select(make_multi_burst(burstz, active_burst = c('id')))
  # time_data = raccoon_data$timestamp

  # if theres more than one burst, then we combine bursts
  # if (length(burst[[1]]) > 1) {
  #   message('more than one burst selected, bursts will be combined for step geometry')
  # }

  idz <- group_labels(group)
  #
  unique_idz <- levels(idz)[table(idz) > 0]

  step_geometry <- rep(NA, length(geometry))

  # check dimensions
  point_d <- class(geometry[[1]])[1]
  nd <- which(point_d == c(NA, "XY", "XYZ"))

  for (i in unique_idz) {
    #  i <- unique_idz[1]
    subz <- idz == i
    # need to order step geometry
    order_t <- order(time_data[subz])

    sub_geom <- geometry[subz]
    sub_geom <- sub_geom[order_t]
    # We cant actually inject a null point and have it convert to line string, so we have to deal with that later

    x1 <- sub_geom[1:length(sub_geom)]
    x2 <-
      c(
        sf::st_sfc(sub_geom[2:(length(sub_geom))]),
        sf::st_sfc(st_point(rep(NA_real_, nd),
          dim = point_d
        ), crs = sf::st_crs(sub_geom))
      )
    first_point <- min(which(subz))

    x3 <- mapply(function(x, y) {
      # x <- x1[[1]]
      # y <- x2[[1]]
      if (any(c(is.na(x), is.na(y)))) {
        if (all(!is.na(x))) {
          new_geom <- sf::st_point(x)
        } else {
          new_geom <- sf::st_point()
        }
      } else {
        new_geom <- sf::st_linestring(rbind(x, y))
      }
      new_geom
    }, x1, x2, SIMPLIFY = FALSE)
    sf_x <- x3
    step_geometry[subz] <- sf_x[order(order_t)]
  }

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
step_metrics <- function(sftraj) {
  if (inherits(sftraj, "sftrack")) {
    sftraj <- as_sftraj(sftraj)
  }
  group_col <- attr(sftraj, "group_col")
  time_col <- attr(sftraj, "time_col")
  sftraj$sftrack_id <-
    paste0(group_labels(sftraj[[group_col]]), "_", sftraj[[time_col]])
  order_t <- order(group_labels(sftraj[[group_col]]), sftraj[[time_col]])
  sftraj <- sftraj[order_t, ]
  is_latlong <- any(st_is_longlat(sftraj), na.rm = TRUE)


  ret <-
    lapply(levels(group_labels(sftraj[[group_col]])), function(index) {
      # index = levels(burst_labels(sftraj$burst, factor = TRUE))[1]
      sub <- sftraj[group_labels(sftraj[[group_col]]) == index, ]
      # if only 1 row
      if (nrow(sub) == 1) {
        return(
          data.frame(
            dx = NA,
            dy = NA,
            dist = NA,
            dt = NA,
            abs_angle = NA,
            speed = NA,
            sftrack_id = sub$sftrack_id
          )
        )
      }

      dx <- get_dx(sub)
      dy <- get_dy(sub)
      dist <- as.numeric(st_length(sub))

      time <- sub[[attr(sub, "time_col")]]
      dt <- c(unclass(time[-1]) - unclass(time[-length(time)]), NA)

      if (!is_latlong) {
        abs_angle <- ifelse(dist < 1e-07, NA, atan2(dy, dx))
      } else {
        x1 <- coord_traj(sub[[attr(sub, "sf_column")]])
        x2 <- rbind(x1[-1, ], c(NA, NA))
        abs_angle <- c((geosphere::bearing(x1[-nrow(x1), ], x2[-nrow(x2), ]) - 90) * -pi / 180, NA)
        abs_angle[!is.na(abs_angle) & abs_angle > (pi)] <- abs_angle[!is.na(abs_angle) & abs_angle > (pi)] - 2 * pi
      }
      dist[is.na(dx) | is.na(dy)] <- NA

      speed <- ifelse(is.na(dist), NA, dist / dt)


      rel_angle <- c(NA, abs_angle[-1] - abs_angle[-length(abs_angle)])
      rel_angle <- ifelse(rel_angle <= (-pi), 2 * pi + rel_angle, rel_angle)
      rel_angle <- ifelse(rel_angle > pi, rel_angle - 2 * pi, rel_angle)

      so <- cbind.data.frame(
        dx = dx,
        dy = dy,
        dist = dist,
        dt = dt,
        abs_angle = abs_angle,
        rel_angle = rel_angle,
        speed = speed
      )
      so[nrow(so), c("dx", "dy", "dist", "dt", "abs_angle", "rel_angle", "speed")] <- NA
      so$sftrack_id <- sub$sftrack_id
      return(so)
    })
  ret <- do.call(rbind, ret)

  ret[order(order_t), ]
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
      time_data = x[[time_col]]
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
