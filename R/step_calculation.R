#' step geom calculator
#'
#' @description This calculates step geom as individual line segments. It implies your bursts
#'
#' @param burst_id burst object thats subsetted by only the bursts we want to use to calculate one step_geom
#' @param geometry the geometery data from either sf or sf_track. Must be an sf geometry class
#' @param timez time object
#' @export make_step_geom
#' @examples
#' burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#' data_sf <- as_sftrack(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time),
#'      error = NA, coords = c('longitude','latitude','height'),
#'     burst =burstz)
#'
#' make_step_geom(burst = data_sf$burst, geometry = data_sf$geometry)

make_step_geom <- function(burst_id, timez, geometry){
  # Need to check if time is ordered, if not throw an error
  # burstz <- list(id = raccoon_data$sensor_code, month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
  # data_sf <- new_sftrack(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time),error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',burst =burstz)
  # burst_id = burst_id = c('id')
  # geometry = data_sf$geometry
  # timez = data_sf$time

  #if theres more than one burst, then we combine bursts
  if(length(burst_id[[1]]) > 1) { message('more than one burst selected, bursts will be combined for step geometry') }
  idz <- factor(paste0(burst_id))
  #
  unique_idz <- levels(idz)[table(idz)>0]

  step_geometry <- rep(NA,length(geometry))
  for(i in unique_idz){
    #  i <- unique_idz[1]
    subz <- idz==i
    # need to order step geometry

    order_t <- order(timez[subz])
    if( !isTRUE(all.equal(order_t, seq_len(sum(subz)))) ){ message(paste0('time was not ordered for: ',i)) }

    sub_geom <- geometry[subz]
    sub_geom <- sub_geom[order_t]
    #We cant actually inject a null point and have it convert to line string, so we have to deal with that later

    x1 <- sub_geom[1:length(sub_geom)]
    x2 <- c(st_sfc(sub_geom[2:(length(sub_geom))]),st_sfc(st_point(c(NA_real_, NA_real_,NA_real_),dim='XYZ')))
    first_point <- min(which(subz))
    #subz[first_point] <- FALSE
    #st_geometrycollection(list(st_point(), st_point(), st_linestring()))
    x3 <- mapply(function(x,y) {
      # x <- x1[[12]]
      # y <- x2[[12]]
      if(any(c(is.na(x),is.na(y)))){
        new_geom <- st_geometrycollection(list(x,y))
      }else{
        new_geom <- st_linestring(rbind(x, y))
      }
      new_geom
    }, x1, x2, SIMPLIFY = F)
    sf_x <- x3
    step_geometry[subz] <- sf_x[order(order_t)]

  }

  return(st_sfc(step_geometry,crs = attr(geometry,'crs')))
}
#
# burstz <- list(month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
# data_sf <- new_sftrack(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time), id = raccoon_data$sensor_code,
#      error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',
#      burst =burstz)
#
# here <- make_step_geom(burst = data_sf$burst, geometry = data_sf$geometry)


