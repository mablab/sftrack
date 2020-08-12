## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
#devtools::load_all("/home/matt/r_programs/sftrack")
library(sftrack)
#raccoon <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))

#data
data('raccoon', package = 'sftrack')

#xyz
coords = raccoon[,c('longitude','latitude')]
crs = '+init=epsg:4326'
#bursts
burst = list(id = raccoon$animal_id,month = as.POSIXlt(raccoon$timestamp)$mon+1)
active_burst = c('id','month')
#time
time = as.POSIXct(raccoon$timestamp, tz='EST')
#error
error = raccoon$fix
my_sftrack <- as_sftrack(data = raccoon, coords = coords, burst = burst, 
                         active_burst = active_burst, time = time, 
                         crs = crs, error = error)

head(my_sftrack)


## -----------------------------------------------------------------------------
raccoon$time <- as.POSIXct(raccoon$timestamp, tz='EST')
raccoon$month <- as.POSIXlt(raccoon$timestamp)$mon+1

coords = c('longitude','latitude')
burst = c(id = 'animal_id', month = 'month')
time = 'time'
error = 'fix'

my_sftraj <- as_sftraj(data = raccoon, coords = coords, burst = burst, time = time, error = error)

head(my_sftraj)

## ---- message = FALSE---------------------------------------------------------
library(adehabitatLT)

ltraj_df <- as.ltraj(xy=raccoon[,c('longitude','latitude')], date = as.POSIXct(raccoon$timestamp),
 id = raccoon$animal_id, typeII = TRUE,
 infolocs = raccoon[,1:6] )

my_sf <- as_sftrack(ltraj_df)
head(my_sf)



## -----------------------------------------------------------------------------
library(sf)
df1 <- raccoon[!is.na(raccoon$latitude),]
sf_df <- st_as_sf(df1, coords=c('longitude','latitude'), crs = crs)
burst = c(id = 'animal_id')
time_col = 'time'

new_sftraj <- as_sftraj(sf_df,burst = burst, time = time_col) 
head(new_sftraj)

new_sftrack <- as_sftrack(sf_df,burst = burst, time= time_col) 
head(new_sftrack)


## -----------------------------------------------------------------------------
# Make tracks from raw data
coords = c('longitude','latitude')
burst = c(id = 'animal_id', month = 'month')
time = 'time'
error = 'fix'

my_sftraj <- as_sftraj(data = raccoon, coords = coords, burst = burst, time = time, error = error)
my_sftrack <- as_sftrack(data = raccoon, coords = coords, burst = burst, time = time, error = error)

# Convert between types
new_sftrack <- as_sftrack(my_sftraj)
#head(new_sftrack)
new_sftraj <- as_sftraj(my_sftrack)
#head(new_sftraj)

identical(my_sftraj,new_sftraj)
identical(my_sftrack,new_sftrack)

## -----------------------------------------------------------------------------

raccoon$time[1] <- raccoon$time[2]
 try(as_sftrack(data = raccoon, coords = coords, burst = burst, time = time, error = error))


## -----------------------------------------------------------------------------
which_duplicated(data = raccoon , burst = burst, time = time)
raccoon <- raccoon[-2,]
my_sftrack <- as_sftrack(data = raccoon, coords = coords, burst = burst, time = time, error = error)


