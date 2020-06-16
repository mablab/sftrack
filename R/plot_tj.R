#' @title methods for plot sftrack/sftraj
#' @export
#' @param x sftrack object
#' @param ... arguments to passed to plot
#' @method plot sftrack
plot.sftrack <- function(x, ...) {
 # x <- my_sftrack
  bl <-   burst_labels(x$burst,factor=TRUE)
  b_lvl <- levels(bl)
  what <- as.numeric(bl)
  col1 <- scales::alpha(what, 0.5)
  my_pts <- st_geometry(x)
  graphics::plot(my_pts,
    col = col1,...)
}

#' @title methods for plot sftrack/sftraj
#' @export
#' @param x sftrack object
#' @param ... arguments to passed to plot
#' @method plot sftraj
plot.sftraj <- function(x, ...) {
  #x <- my_sftraj
  bl <-   burst_labels(x$burst,factor=TRUE)
  b_lvl <- levels(bl)
  geom = st_geometry(x)
  il <- is_linestring(geom)
  col1 <-  vapply(burst_labels(x[['burst']][!il]), function(x) which(x==b_lvl), NA_integer_)
  my_pts <- geom[!il]
  attr(my_pts,'bbox') <- st_bbox(geom)
  graphics::plot(my_pts, col = col1,...)

  here = lapply(b_lvl, function(y){
    st_multilinestring(geom[bl==y&il])
  })

  for (i in seq_along(here)) {
    if(is.null(here[[i]])){next}
    #lvl <- names(here[[i]])
    graphics::plot(here[[i]],
      type = 'l',
      add = TRUE ,
      col = i,...)
  }
}


# plot(my_step)
#' @title Function to plot sftrack objects in ggplot
#' @name geom_sftrack
#' @description
#' This function can be added to ggplot() to plot an sftrack and sftraj
#' Function does not yet work with ggplot grammer so you must but data= in this function
#' @name geom_sftrack
#' @param data the sftraj or sftrack object.
#' @param ... arguments to passed to ggplot
#' @examples
#' library(ggplot2)
#' library(sftrack)
#' raccoon_data <- read.csv(system.file('extdata/raccoon_data.csv', package='sftrack'))
#' raccoon_data$acquisition_time <- as.POSIXct(raccoon_data$acquisition_time, 'EST')
#'   burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#' my_step <- as_sftraj(raccoon_data, time_col = 'acquisition_time',
#'    coords = c('longitude','latitude'),
#'   burst_list =burstz)
#'
#' ggplot() + geom_sftrack(data = my_step)
#' @export
geom_sftrack <- function(data, ...) {
  UseMethod('geom_sftrack')
}

#' @rdname geom_sftrack
#' @export
geom_sftrack.sftrack <- function(mapping = aes(),data = NULL,...) {
  sub <- data[!st_is_empty(data[[attr(data, 'sf_column')]]),]
  bl <- burst_labels(sub$burst, factor = T)
  list(ggplot2::geom_sf(data = sub, ggplot2::aes(
    color = bl, fill = bl
  )),
    ggplot2::guides(color = FALSE) ,
    ggplot2::labs(fill = "Bursts"),...)
}

#'@name geom_sftrack
#'@export
geom_sftrack.sftraj <- function(mapping = aes(),data = NULL,...) {
  sub <- data[!st_is_empty(data[[attr(data, 'sf_column')]]),]
  bl <- burst_labels(sub$burst, factor = T)
  list(
    ggplot2::geom_sf(data = st_sfc(
      pts_traj(sub), crs = attr(sub[[attr(sub, 'sf_column')]], 'crs')
    ), ggplot2::aes(color = bl)),
    ggplot2::geom_sf(data = sub, ggplot2::aes(
      color = bl, fill = bl
    )),
    ggplot2::guides(color = FALSE) ,
    ggplot2::labs(fill = "Bursts")
  )
}
#ggplot() + geom_sftrack(data = my_sftraj)
