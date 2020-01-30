
#####################################
#' sftstep Class
#'
#' @description This is the highest level class that collects the error, time, and burst class.
#' It converts x,y,z data into an sfstep object and gives it an sf$geometry column. This
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
#' @export new_sfstep
#' @examples
#'  data(raccoon_data)
#'  burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#' my_step <- new_sfstep(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time),
#'   error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',
#'   burst =burstz)
######################
new_sfstep<-
  function(data = data.frame(),
    proj4 = NA,
    time = NA,
    id = NA,
    burst = NULL,
    error = NA,
    coords = c('x','y','z'),
    tz = NULL,
    active_burst = 'id'
  )  {
    # data = raccoon_data
    # time = as.POSIXct(raccoon_data$acquisition_time)
    # error = error
    # tz = NULL
    # coords = c('longitude', 'latitude','height')
    # burst = burstz
    # active_burst = c('id','month')

    #convert to sf object

    data_sf <- new_sftraj(data, time =time,
      error = NA, coords = coords, tz = 'UTC',
      burst = burst)
    # have to decide when and where we order datasets
    # torder <- order(time)
    # data_sf <- data_sf[torder,]
    # Function to make the step geometry column

    step_geometry <- make_step_geom(burst = lapply(data_sf$burst, function(x)x[active_burst]), geometry = data_sf$geometry)

    structure(
      data_sf1 <- sf::st_sf(
        data_sf,
        geometry=step_geometry
      ),
      active_burst = active_burst,
      projection = proj4,
      class = c("sfstep", 'sf','data.frame')
    )

  }

# sfs1 <- new_sfstep(raccoon_data, time =as.POSIXct(raccoon_data$acquisition_time), id = raccoon_data$sensor_code,
#   error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',
#   burst =burstz)
#' @export
print.sfstep <- function(x,...){
  x <- as.data.frame(x) # have to do this because otherwise it uses sf rules...hmmm
  cat('this is a sfstep object\n')
  cat(paste0('proj : ',attr(x,'projection'),'\n'))
  cat(paste0('unique ids : ', paste(unique(sapply(x$burst, function(x) x$id)),collapse=', '), '\n'))
  cat(paste0('active bursts : ', paste0(attr(x, 'active_burst'),collapse=', '), '\n'))
  n <- ifelse(nrow(x)>10,10,nrow(x))
  row_l <- length(colnames(x)!=c('time','burst','error','geometry'))
  p <- ifelse(row_l>6,6,row_l)
  cat(paste("First", n, "features w/",p+4, "truncated columns:\n"))
  if(ncol(x)>10){
    y <- cbind(x[1:n,colnames(x)[1:p]],
    data.frame('...' = rep('...',n)),
    x[1:n,c('time','burst','error','geometry')])
} else y <- x
print.data.frame(y, ...)
}
