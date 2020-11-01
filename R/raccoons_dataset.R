#' Movements of two raccoons in Tree Tops Park, Florida
#'
#' Example dataset including tracking data of two raccoons, monitored
#' with GPS telemetry in Tree Tops Park, an urban recreational park in
#' Davie, Florida, USA, in January–February 2019. Raw data is
#' available in a \code{data.frame} (\code{raccoons}). Processed data
#' is available as tracking data (locations) in an \code{sftrack}
#' object (\code{racc_track}) and as movement data (step model) in an
#' \code{sftraj} object (\code{racc_traj}).
#'
#' @name raccoons
#' @aliases racc_track racc_traj
#' @format \code{raccoons} is a \code{data.frame} with 445 rows and 8 variables:
#' \describe{
#'   \item{animal_id}{Factor; ID of individual. TTP: Tree Tops Park, i.e the tagging site.}
#'   \item{latitude}{Numeric; latitude in decimal degrees}
#'   \item{longitude}{Numeric; longitude in decimal degrees}
#'   \item{timestamp}{POSIXct; date and time of GPS fixes in EST5EDT}
#'   \item{height}{Integer; altitude in meters based on satellite positions}
#'   \item{hdop}{Numeric; horizontal dilution of precision (HDOP)}
#'   \item{vdop}{Numeric; vertical dilution of precision (VDOP)}
#'   \item{fix}{Factor; The number of satellite available for fixes (2D/3D/NO)}
#' }
#'
#'\code{racc_track} and \code{racc_traj} are \code{sftrack} and
#' \code{sftraj} objects, respectively, and provide the additional
#' columns:
#' \describe{
#'   \item{sft_group}{c_grouping; grouping of the data.}
#'   \item{geometry}{sfc_GEOMETRY; sf geometries associated to locations or steps}
#' }
"raccoons"
