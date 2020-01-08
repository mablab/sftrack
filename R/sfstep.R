library(sf)
library(sftraj)
df1 <- read.csv('/home/matt/Downloads/data-1576521725288.csv')
#df1 <- df1[df1$sensor_code=='CJ11',]
df2 <- st_as_sf(df1, coords=c('latitude','longitude'))
burstz <- list(month = as.POSIXlt(df1$utc_date)$mon, height =as.numeric(df1$height>5))
# Make a step geometry
# Must be ordered, does not descriminate!
make_step_geom <- function(burst = data_sf$burst, geometry = data_sf$geometry){
  # Need to check if time is ordered, if not throw an error

  burst = data_sf$burst
  idz <- sapply(burst, function(x) x$id)
  unique_idz <- levels(idz)[table(idz)>0]

  step_geometry <- rep(NA,length(geometry))
  for(i in unique_idz){
    #  i <- unique_idz[1]
    subz <- idz==i

    #We cant actually inject a null point and have it convert to line string, so we have to deal with that later

    x1 <- geometry[subz][1:(sum(subz)-1)]

    x2 <- geometry[subz][2:sum(subz)]
    first_point <- min(which(subz))
    #subz[first_point] <- FALSE
    x3 <- mapply(function(x,y) { st_linestring(rbind(x, y)) }, x1, x2, SIMPLIFY = F)

    step_geometry[subz] <- c(st_sfc(st_linestring(x = matrix(numeric(0), 0, 3), dim = "XYZ")),st_sfc(x3))

  }
  return(st_sfc(step_geometry))
}

make_step_geom()
# Whats the bare minimum you need for the class
new_sfstep<-
  function(data = data.frame(),
    proj4 = NA,
    time = NA,
    id = NA,
    burst = NULL,
    error = NA,
    coords = c('x','y','z'),
    tz = NULL
  )  {
    coords = c('longitude', 'latitude','height')

    data = df1
    time = as.POSIXct(df1$acquisition_time)
    id = df1$sensor_code
    error = error
    tz = NULL

    #convert to sf object
    # tapply(timez, idz, duplicated)
    # unique_q <- tapply(timez, idz, function(x)
    #   any(duplicated(x)))

    data_sf <- new_sftraj(data, time =time, id = id,
      error = NA, coords = coords, tz = 'UTC',
      burst = burstz)
    torder <- order(time)
    data_sf <- data_sf[torder,]
    # Function to make the step geometry column

    data_sf$step_geometry <- make_step_geom(burst = data_sf$burst, geometry = data_sf$geometry)

    structure(
      data_sf1 <- sf::st_sf(
        data_sf,
        geometry=data_sf$step_geometry
      ),
      projection = proj4,
      class = c("sfstep", 'data.frame')
    )

  }
