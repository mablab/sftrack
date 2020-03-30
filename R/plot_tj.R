#' Methods for plot an sftraj
#'
#' @export plot.sftrack
plot.sftrack <- function(x, ...) {
  plot(x$geometry)
  burst_srt <- burst_sort(x$burst)
  burst_lbl <- as.character(burst_srt)
  new_clrs <- rainbow(length(unique(burst_srt)))
  for (i in seq_along(new_clrs)) {
    #i=1
    lvl <- levels(burst_srt)[i]
    plot(x$geometry[burst_lbl == lvl],
      type = 'l',
      add = T ,
      col = new_clrs[i])
  }
}

# plot(my_track)

#' @export plot.sftraj
plot.sftraj <- function(x, ...) {
  plot(x$geometry)
  burst_srt <- burst_sort(x$burst)
  burst_lbl <- as.character(burst_srt)
  new_clrs <- rainbow(length(unique(burst_srt)))
  for (i in seq_along(new_clrs)) {
    #i=1
    lvl <- levels(burst_srt)[i]
    plot(x$geometry[burst_lbl == lvl],
      type = 'l',
      add = T ,
      col = new_clrs[i])
  }
}

#' @exportMethod geom_sftrack
geom_sftrack <- function(data, ...) {
  UseMethod('geom_sftrack')
}

# plot(my_step)
#' geom_sftrack()
#' This function can be added to ggplot() to plot an sftrack and sftraj
#' Function does not yet work with ggplot grammer so you must but data= in this function
#' @param data the sftraj or sftrack object.

#' @examples
#'
#' data(raccoon_data)
#'   burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#' my_step <- as_sftraj(raccoon_data, time_col = 'acquisition_time',
#'    coords = c('longitude','latitude'),
#'   burst_list =burstz)
#' library(ggplot2)
#'
#' ggplot() + geom_sftrack(data = my_step)
#' @export geom_sftrack
#'

#' @export
geom_sftrack.sftrack <- function(data, ...) {
  sub <- data[!st_is_empty(data$geometry), ]
  list(ggplot2::geom_sf(data = sub, aes(
    color = burst_sort(sub$burst), fill = burst_sort(sub$burst)
  )),
    guides(color = FALSE) ,
    labs(fill = "Bursts"))
}
#'@export
geom_sftrack.sftraj <- function(data, ...) {
  sub <- data[!st_is_empty(data$geometry), ]
  list(
    ggplot2::geom_sf(data = st_sfc(
      pts_traj(sub), crs = attr(data$geometry, 'crs')
    ), aes(color = burst_sort(sub$burst))),
    ggplot2::geom_sf(data = sub, aes(
      color = burst_sort(sub$burst), fill = burst_sort(sub$burst)
    )),
    guides(color = FALSE) ,
    labs(fill = "Bursts")
  )
}
