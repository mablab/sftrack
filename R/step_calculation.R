#' step geom calculator
#'
#' @description This calculates step geom as individual line segments
#'
#' @param burst list of named vectors
#' @param geometry the geometery data from either sf or sf_traj. Must be an sf geometry class
#' @param timez time obviously
#' @export make_step_geom
#' @examples
#' burstz <- list(month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
#' data_sf <- new_sftraj(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time), id = raccoon_data$sensor_code,
#'      error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',
#'     burst =burstz)
#'
#'make_step_geom(burst = data_sf$burst, geometry = data_sf$geometry)

make_step_geom <- function(burst = NA, timez = NA, geometry = NA){
  # Need to check if time is ordered, if not throw an error

  # burst = data_sf$burst
  # geometry = data_sf$geometry
  idz <- sapply(burst, function(x) x$id)
  unique_idz <- levels(idz)[table(idz)>0]
  
  step_geometry <- rep(NA,length(geometry))
  for(i in unique_idz){
    #  i <- unique_idz[1]
    subz <- idz==i
    # need to order step geometry
    
    order_t <- order(timez[subz])
    if( !isTRUE(all.equal(order_t, seq_len(sum(subz)))) ){ message('time was not ordered') }
    geometry <- geometry[order_t]
    
    #We cant actually inject a null point and have it convert to line string, so we have to deal with that later
    
    x1 <- geometry[subz][1:(sum(subz)-1)]
    x2 <- geometry[subz][2:sum(subz)]
    first_point <- min(which(subz))
    #subz[first_point] <- FALSE
    x3 <- mapply(function(x,y) { st_linestring(rbind(x, y)) }, x1, x2, SIMPLIFY = F)
    x3 <- x3[order(order_t)]
    step_geometry[subz] <- c(st_sfc(st_linestring(x = matrix(numeric(0), 0, 3), dim = "XYZ")),st_sfc(x3))
    
  }
  return(st_sfc(step_geometry))
}
# 
# burstz <- list(month = as.POSIXlt(raccoon_data$utc_date)$mon, height =as.numeric(raccoon_data$height>5))
# data_sf <- new_sftraj(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time), id = raccoon_data$sensor_code,
#      error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',
#      burst =burstz)
# 
# here <- make_step_geom(burst = data_sf$burst, geometry = data_sf$geometry)

###############
#' step calculations
#'

