#' @title methods for plot sftrack/sftraj
#' @export
#' @param x sftrack object
#' @param ... arguments to passed to plot
#' @method plot sftrack
plot.sftrack <- function(x, ...) {
  graphics::plot(x$geometry)
  burst_srt <- burst_sort(x$burst)
  burst_lbl <- as.character(burst_srt)
  new_clrs <- grDevices::rainbow(length(unique(burst_srt)))
  for (i in seq_along(new_clrs)) {
    #i=1
    lvl <- levels(burst_srt)[i]
    graphics::plot(x$geometry[burst_lbl == lvl],
      type = 'l',
      add = T ,
      col = new_clrs[i])
  }
}

#' @title methods for plot sftrack/sftraj
#' @export
#' @param x sftrack object
#' @param ... arguments to passed to plot
#' @method plot sftraj
plot.sftraj <- function(x, ...) {
  graphics::plot(x$geometry)
  burst_srt <- burst_sort(x$burst)
  burst_lbl <- as.character(burst_srt)
  new_clrs <- grDevices::rainbow(length(unique(burst_srt)))
  for (i in seq_along(new_clrs)) {
    #i=1
    lvl <- levels(burst_srt)[i]
    graphics::plot(x$geometry[burst_lbl == lvl],
      type = 'l',
      add = T ,
      col = new_clrs[i])
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
  sub <- data[!st_is_empty(data$geometry), ]
  list(ggplot2::geom_sf(data = sub, ggplot2::aes(
    color = burst_sort(sub$burst), fill = burst_sort(sub$burst)
  )),
    ggplot2::guides(color = FALSE) ,
    ggplot2::labs(fill = "Bursts"))
}

#'@name geom_sftrack
#'@export
geom_sftrack.sftraj <- function(data, ...) {
  sub <- data[!st_is_empty(data$geometry), ]
  list(
    ggplot2::geom_sf(data = st_sfc(
      pts_traj(sub), crs = attr(data$geometry, 'crs')
    ), ggplot2::aes(color = burst_sort(sub$burst))),
    ggplot2::geom_sf(data = sub, ggplot2::aes(
      color = burst_sort(sub$burst), fill = burst_sort(sub$burst)
    )),
    ggplot2::guides(color = FALSE) ,
    ggplot2::labs(fill = "Bursts")
  )
}
