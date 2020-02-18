#' Convert ltraj to sftrack
#' @title Convert ltraj to sftrack
#' @description This function converts ltraj to sftrack by making some assumptions on data structure from the ltraj
#'
#' @param data ltraj object created from adehabitatLT
#' @param proj4 projection (for sf)
#' @param tz timezone component, same as as.POSIX
#' @export new_sftrack
#' @examples
#' library(adehabitatLT)
#' data(raccoon_data)
#' ltraj_df <- as.ltraj(xy=raccoon_data[,c('longitude','latitude')], date = as.POSIXct(raccoon_data$acquisition_time),
#'  id = raccoon_data$sensor_code, typeII = TRUE,
#'  infolocs = raccoon_data[,1:6] )
#' ltraj2track(data = ltraj_df)

ltraj2track <- function(data){
new_data <- lapply(seq_along(data), function(x) {
  sub <- data[x]
  attributes(sub[[1]])
  id <-  attr(sub[[1]], 'burst')
  infolocs <- infolocs(sub)
  date <- sub[[1]]$date
  coords <- c('x','y')
  data.frame(sub[[1]][,coords],z=NA,date,id,infolocs)
  }
)
df1 <- do.call(rbind, new_data)
new_sftrack(data = subset(df1, select = -c(date, id)), time = df1$date, burst = list(id=df1$id), coords = c('x','y','z'))
}

