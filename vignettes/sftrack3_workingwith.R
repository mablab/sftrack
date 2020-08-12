## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -----------------------------------------------------------------------------
#devtools::load_all("/home/matt/r_programs/sftrack")
library(sftrack)
# Make tracks from raw data
data('raccoon', package = 'sftrack')
#raccoon <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))
raccoon$month <- as.POSIXlt(raccoon$timestamp)$mon+1

raccoon$time <- as.POSIXct(raccoon$timestamp, tz='EST')
coords = c('longitude','latitude')
burst = list(id = raccoon$animal_id, month = as.POSIXlt(raccoon$timestamp)$mon+1)
time = 'time'
error = 'fix'
crs = '+init=epsg:4326'
my_sftrack <- as_sftrack(data = raccoon, coords = coords, burst = burst, time = time, error = error, crs = crs)
my_sftraj <- as_sftraj(data = raccoon, coords = coords, burst = burst, time = time, error = error, crs = crs)


## -----------------------------------------------------------------------------
attributes(my_sftrack)

## -----------------------------------------------------------------------------
my_sftrack$geometry

## -----------------------------------------------------------------------------
  df1 <- data.frame(
    id = c(1, 1, 1, 1,1,1),
    month = c(1,1,1,1,1,1),
    x = c(27, 27, 27, NA,29,30),
    y = c(-80,-81,-82,NA, 83,83),
    timez = as.POSIXct('2020-01-01 12:00:00', tz = 'UTC') + 60*60*(1:6)
  )

  test_sftraj <- as_sftraj(data = df1,burst=list(id=df1$id, month = df1$month),
    time = df1$timez, active_burst = c('id','month'), coords = df1[,c('x','y')])
  test_sftraj$geometry

## -----------------------------------------------------------------------------
attributes(my_sftrack$burst)
summary(my_sftrack)

## -----------------------------------------------------------------------------

my_sftrack[1:10,]

## -----------------------------------------------------------------------------

my_sftrack[1:3,c(1:3)]

## -----------------------------------------------------------------------------
my_sftrack[1:3,c(1:3), drop = TRUE]

