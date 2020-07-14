#' @title methods for plot sftrack/sftraj
#' @name plot_sftrack
#' @export
#' @param x sftrack/sftraj object
#' @param ... arguments to passed to plot
#' @method plot sftrack
#' @examples
#' library(sftrack)
#' data('raccoon')
#' raccoon$acquisition_time <- as.POSIXct(raccoon$acquisition_time, 'EST')
#'   burstz <- c(id = 'sensor_code')
#' my_sftrack <- as_sftrack(raccoon, time = 'acquisition_time',
#'    coords = c('longitude','latitude'),
#'   burst = burstz)
#'
#' # Plotting with sftrack is just like sf. `...` will accept most arguments as plot.sf
#'
#' plot(my_sftrack, axes = TRUE, lwd =5 ,cex=5, bgc = 'gray80')
#'
#' # sftraj will as well for the most part, however as its a more
#' # complex structure to speed up plotting.
#' my_sftraj <- as_sftraj(raccoon, time = 'acquisition_time',
#'   coords = c('longitude','latitude'),
#'   burst = burstz)
#' plot(my_sftraj, axes = TRUE, lwd =5 ,cex=5, bgc = 'gray80', graticule = TRUE)


plot.sftrack <- function(x, ...) {
  # x <- my_sftrack
  sf_col <- attr(x,'sf_column')
  bl <-   burst_labels(x$burst, factor = TRUE)
  x = st_sf(data.frame(st_geometry(x), bursts = bl),sf_column_name = sf_col )
  NextMethod()
}
#plot(my_sftrack)
#' @title methods for plot sftrack/sftraj
#' @export
#' @rdname plot_sftrack
#' @method plot sftraj
plot.sftraj <- function(x, ...) {
 # x <- my_sftraj
  bl <-   burst_labels(x$burst, factor = TRUE)
  b_lvl <- levels(bl)
  geom = st_geometry(x)
  il <- is_linestring(geom)

  here = lapply(b_lvl, function(y) {
    st_multilinestring(geom[bl == y & il])
  })
  new_sfc <- st_sfc(here, crs = attr(geom, 'crs'))
  x = st_sf(data.frame(geometry = new_sfc, burst = b_lvl))
  NextMethod()
}

#plot(my_sftraj, lwd=5, axes = T)
# plot(my_step)
#' @title Function to plot sftrack objects in ggplot
#' @name geom_sftrack
#' @description
#' This function can be added to ggplot() to plot an sftrack and sftraj
#' Function does not yet work with ggplot grammer so you must but data= in this function
#' @name geom_sftrack
#' @param mapping mapping aesthetics for ggplot.
#' @param data the sftraj or sftrack object.
#' @param ... arguments to passed to ggplot
#' @examples
#' #'
#' library(ggplot2)
#' library(sftrack)
#' data('raccoon')
#' raccoon$acquisition_time <- as.POSIXct(raccoon$acquisition_time, 'EST')
#'   burstz <- c(id = 'sensor_code')
#'
#' # sftraj will as well for the most part, however as its a more complex
#' # structure to speed up plotting.
#' my_sftraj <- as_sftraj(raccoon, time = 'acquisition_time',
#'   coords = c('longitude','latitude'),
#'   burst = burstz)
#'
#' ggplot() + geom_sftrack(data = my_sftraj)

#' @export
geom_sftrack <- function(mapping, data, ...) {
  UseMethod('geom_sftrack')
}

#' @rdname geom_sftrack
#' @export
geom_sftrack.sftrack <-
  function(mapping = ggplot2::aes(),
    data = NULL,
    ...) {
    sub <- data[!st_is_empty(data[[attr(data, 'sf_column')]]), ]
    bl <- burst_labels(sub$burst, factor = T)
    list(
      ggplot2::geom_sf(data = sub, ggplot2::aes(color = bl, fill = bl)),
      ggplot2::guides(color = FALSE) ,
      ggplot2::labs(fill = "Bursts"),
      ...
    )
  }

#'@name geom_sftrack
#'@export
geom_sftrack.sftraj <-
  function(mapping = ggplot2::aes(),
    data = NULL,
    ...) {
    #x = my_sftraj
    x = data
    bl <-   burst_labels(x$burst, factor = TRUE)
    b_lvl <- levels(bl)
    geom = st_geometry(x)
    il <- is_linestring(geom)

    here = lapply(b_lvl, function(y) {
      st_multilinestring(geom[bl == y & il])
    })
    new_sfc <- st_sfc(here, crs = attr(geom, 'crs'))
    x = st_sf(data.frame(geometry = new_sfc, burst = b_lvl))

    list(
      ggplot2::geom_sf(data = x, ggplot2::aes(color = burst, fill = burst)),
      ggplot2::guides(color = FALSE),
      ggplot2::labs(fill = "Bursts"))
  }


#ggplot() + geom_sftrack(data = my_sftraj)
