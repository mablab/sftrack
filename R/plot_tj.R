#' Methods for plot an sftraj
#'
#' @export plot.sftrack
plot.sftrack <- function(x,...){
plot(x$geometry)
burst_srt <- burst_sort(x$burst)
burst_lbl <- burst_labels(x$burst)
new_clrs <- rainbow(length(unique(burst_srt)))
  for(i in seq_along(new_clrs)){
    #i=1
    lvl <- levels(burst_srt)[i]
  plot(x$geometry[burst_lbl==lvl], type='l',add = T ,col=new_clrs[i])
  }
}

# plot(my_track)

#' @export plot.sftraj
plot.sftraj <- function(x,...){
  plot(x$geometry)
  burst_srt <- burst_sort(x$burst)
  burst_lbl <- burst_labels(x$burst)
  new_clrs <- rainbow(length(unique(burst_srt)))
  for(i in seq_along(new_clrs)){
    #i=1
    lvl <- levels(burst_srt)[i]
    plot(x$geometry[burst_lbl==lvl], type='l',add = T ,col=new_clrs[i])
  }
}

# plot(my_step)
#' geom_sftraj()
#' This function can be added to ggplot() to plot an sftrack and sftraj
#' Function does not yet work with ggplot grammer so you must but data= in this function
#' @param data the sftraj or sftrack object.
#' @export geom_sftraj
#' @examples
#'
#' data(raccoon_data)
#'   burstz <- list(id = raccoon_data$sensor_code,month = as.POSIXlt(raccoon_data$utc_date)$mon)
#' my_step <- as_sftraj(raccoon_data, time = 'acquisition_time',
#'    coords = c('longitude','latitude','height'),
#'   burst =burstz)
#' library(ggplot2)
#'
#' ggplot() + geom_sftraj(data = my_step)
geom_sftraj <- function(data,...){
  sub <- data[!sapply(data$geometry, function(x) all(is.na(x[[1]]))),]
  geom_sf(data = sub, aes(color=burst_sort(sub$burst)))
}
#library(ggplot2)
#ggplot(data=my_track) +geom_sf(aes(color=burst_sort(my_track$burst)))
#ggplot() + geom_sftraj(my_step)
