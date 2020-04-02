#' @title Calculate step geometries given a set of bursts, time, and geometries
#'
#' @description This calculates step geometries as individual line segments based on the active_burst
#' @param burst a multi_burst
#' @param time_data time vector
#' @param geometry the geometery data from either sf or sf_track. Must be an sf geometry class
#' @export make_step_geom
#' @examples
#' raccoon_data <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))
#' burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#' data_sf <- as_sftrack(raccoon_data, time_col = 'acquisition_time',
#'   error = NA, coords = c('longitude','latitude'),
#'   burst_list =burstz)
#'
#' make_step_geom(burst = data_sf$burst,
#'                geometry = data_sf$geometry,
#'                time_data = data_sf$acquisition_time)

make_step_geom <- function(burst, time_data, geometry) {
  # Need to check if time is ordered, if not throw an error
  # burstz <- list(id = raccoon_data$sensor_code, month = as.POSIXlt(raccoon_data$utc_date)$mon)
  # #data_sf <- new_sftrack(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time),error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',burst =burstz)
  # burst = burst_select(make_multi_burst(burstz, active_burst = c('id')))
  # time_data = raccoon_data$acquisition_time

  #if theres more than one burst, then we combine bursts
  # if (length(burst[[1]]) > 1) {
  #   message('more than one burst selected, bursts will be combined for step geometry')
  # }
  burst <- burst_select(burst)
  as.character(burst[[3]]$id)
  idz <- factor(paste0(burst))
  #
  unique_idz <- levels(idz)[table(idz) > 0]
  check_ordered(burst, time_data, return = FALSE)
  step_geometry <- rep(NA, length(geometry))

  # check dimensions
  point_d <- class(geometry[[1]])[1]
  nd <- which(point_d == c(NA, 'XY', 'XYZ'))

  for (i in unique_idz) {
    #  i <- unique_idz[1]
    subz <- idz == i
    # need to order step geometry
    order_t <- order(time_data[subz])

    sub_geom <- geometry[subz]
    sub_geom <- sub_geom[order_t]
    #We cant actually inject a null point and have it convert to line string, so we have to deal with that later

    x1 <- sub_geom[1:length(sub_geom)]
    x2 <-
      c(sf::st_sfc(sub_geom[2:(length(sub_geom))]),
        sf::st_sfc(st_point(rep(NA_real_, nd),
          dim = point_d)))
    first_point <- min(which(subz))

    x3 <- mapply(function(x, y) {
      # x <- x1[[12]]
      # y <- x2[[12]]
      if (any(c(is.na(x), is.na(y)))) {
        new_geom <- sf::st_geometrycollection(list(x, y))
      } else{
        new_geom <- sf::st_linestring(rbind(x, y))
      }
      new_geom
    }, x1, x2, SIMPLIFY = F)
    sf_x <- x3
    step_geometry[subz] <- sf_x[order(order_t)]

  }

  return(sf::st_sfc(step_geometry, crs = attr(geometry, 'crs')))
}
#
# burstz <- list(month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
# data_sf <- new_sftrack(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time), id = raccoon_data$sensor_code,
#      error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',
#      burst =burstz)
#
# here <- make_step_geom(burst = data_sf$burst, geometry = data_sf$geometry)
