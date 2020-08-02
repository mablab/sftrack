#' Movements of two raccoons in an urban park in Florida
#'
#' A dataset of two raccoons collared with GPS collars for one month in January 2019
#' in Tree Tops Park, Broward County, Florida, US.
#'
#' @format A data frame with 445 rows and 10 variables:
#' \describe{
#'   \item{animal_id}{ID of individual. TTP: tree tops park, i.e the tagging site.}
#'   \item{timestamp}{The date and time of gps fix in UTC}
#'   \item{latitude}{Latitude in degrees}
#'   \item{longitude}{Longitude in degrees}
#'   \item{height}{Altitude in meters based on satellite positios}
#'   \item{hdop}{Horizontal precision}
#'   \item{vdop}{Vertical precision}
#'   \item{fix}{The number of satellite fixes}
#' }

"raccoon"
