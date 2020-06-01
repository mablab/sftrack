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
  col1 <- scales::alpha(what, 0.2)
  my_pts <- st_geometry(x)
  graphics::plot(my_pts,
    col = col1, cex = 1)
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
  what <- as.numeric(bl)
  col1 <- scales::alpha(what, 0.2)
  my_pts <- pts_traj(x[[attr(x, 'sf_column')]], TRUE)

  graphics::plot(my_pts,
    col = col1, cex = 1)
  my_coords <- st_coordinates(my_pts)
  here <- lapply(b_lvl, function(y){
    #for(y in b_lvl){
    #y = b_lvl[22]
    sub_coords <- my_coords[bl ==y,]
    if(!inherits(sub_coords,'matrix')){return()}
    sub_time <- x[[attr(x,'time')]][bl==y]
    sub_time <- order(sub_time)
    which_na <- c(0,which(is.na(sub_coords[,1])),  (nrow(sub_coords)+1))
    what <- lapply(seq_len(length(which_na)-1), function(z){
      #z=3
      st_linestring(sub_coords[(which_na[z]+1):(which_na[z+1]-1),])
    })
    ret = st_multilinestring(what)
    names(ret) <- y
    return(ret)
  })

  for (i in seq_along(here)) {
    if(is.null(here[[i]])){next}
    #lvl <- names(here[[i]])
    graphics::plot(here[[i]],
      type = 'l',
      add = TRUE ,
      col = i)
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
geom_sftrack.sftrack <- function(data, ...) {
  sub <- data[!st_is_empty(data[, attr(data, 'sf_column')]), ]
  list(ggplot2::geom_sf(data = sub, ggplot2::aes(
    color = burst_sort(sub$burst), fill = burst_sort(sub$burst)
  )),
    ggplot2::guides(color = FALSE) ,
    ggplot2::labs(fill = "Bursts"))
}

#'@name geom_sftrack
#'@export
geom_sftrack.sftraj <- function(data, ...) {
  sub <- data[!st_is_empty(data[,attr(data, 'sf_column')]), ]
  list(
    ggplot2::geom_sf(data = st_sfc(
      pts_traj(sub), crs = attr(data[,attr(data, 'sf_column')], 'crs')
    ), ggplot2::aes(color = burst_sort(sub$burst))),
    ggplot2::geom_sf(data = sub, ggplot2::aes(
      color = burst_sort(sub$burst), fill = burst_sort(sub$burst)
    )),
    ggplot2::guides(color = FALSE) ,
    ggplot2::labs(fill = "Bursts")
  )
}
