library(sf)
library(sftraj)
df1 <- read.csv('/home/matt/Downloads/data-1576521725288.csv')
df1 <- df1[df1$sensor_code=='CJ11',]
df2 <- st_as_sf(df1, coords=c('latitude','longitude'))
burstz <- list(month = as.POSIXlt(df1$utc_date)$mon, height =as.numeric(df1$height>5))
id = df1$sensor_code
my_traj <- new_sftraj(df1, time =as.POSIXct(df1$acquisition_time), id = df1$sensor_code,
   error = NA, coords = c('longitude','latitude','height'), tz = 'UTC',
   burst = burstz)
#Works to convert from sftraj
# Try not to change
df3 <- df2[is.na(as.numeric(st_intersects(df2$geometry,st_point(c(0,0))))),]
x1 <- df3$geometry[1:(nrow(df3)-1)]
x2 <- df3$geometry[2:nrow(df3)]
df3 <- df3[2:(nrow(df3)),]
df3$geom2 <- NA
df3$geom2 <- st_sfc(mapply(function(x,y) st_linestring(rbind(x, y)), x1, x2, SIMPLIFY = F))

plot(df3$geom2)
burst[['id']] <- id
burst_list <- do.call(function(...) mapply(list,...,SIMPLIFY=F), burst)
##

# Whats the bare minimum you need for the class though

y = df1$latitude
x = df1$longitude
timez = as.POSIXct(df1$acquisition_time)
idz = df1$sensor_code

####
# Note for tomorrow
# Have to figure out how to order points before calculating
# Particularly for multiple IDs

df3$geometry <- st_sfc(mapply(function(x,y) st_linestring(rbind(x, y)), x1, x2, SIMPLIFY = F))


sf::st_as_sf(
  data.frame(
    id = seq_len(nrow(data)),
    data,
    time_traj = new_time_tj(time,id=id,tz=tz),
    burst = make_multi_burst(id=id, burst=burst),
    error = error
  ),
  coords = coords,
  dim = 'XYZ'
)
##############
# Make class
new_sfstep<-
  function(data = data.frame(),
    proj4 = NA,
    time = NA,
    id = NA,
    burst = NULL,
    error = NA,
    coords = c('x','y'),
    tz = NULL
  ) {
    burst[['id']] <- id
    burst_list <- do.call(function(...) mapply(list,...,SIMPLIFY=F), burst)
    
    structure(
      sf::st_as_sf(
        data.frame(
          id = seq_len(nrow(data)),
          data,
          time_traj = new_time_tj(time,id=id,tz=tz),
          burst = make_multi_burst(id=id, burst=burst),
          error = error
        ),
        coords = coords,
        dim = 'XYZ'
      ),
      projection = proj4,
      class = c("sftraj", 'data.frame')
      beginning = 
        end = 
    )
  }
