#' Movements of two raccoons in an urban park in Florida
#'
#' A dataset of two raccoons collared with GPS collars for one month in January 2019
#' in Tree Tops Park, Broward County, Florida, US.
#'
#' @format A data frame with 445 rows and 10 variables:
#' \describe{
#'   \item{sensor_code}{ID of individual}
#'   \item{utc_date}{The date of gps fix in UTC}
#'   \item{utc_time}{The time of gps fix in UTC}
#'   \item{latitude}{Latitude in degrees}
#'   \item{longitude}{Longitude in degrees}
#'   \item{height}{Altitude in meters based on satellite positios}
#'   \item{hdop}{Horizontal precision}
#'   \item{vdop}{Vertical precision}
#'   \item{fix}{The number of satellite fixes}
#'   \item{acquisition_time}{Date and Time of gps fix paste(utc_date, utc_time)}
#' }

"raccoon"